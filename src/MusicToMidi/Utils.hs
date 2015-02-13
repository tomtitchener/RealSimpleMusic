{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MusicToMidi.Utils where

import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Music.Data
import           Music.Utils()
import           Control.Monad
import           Control.Monad.State
import qualified Data.EventList.Relative.TimeBody as EventList
import           Data.Traversable
import qualified Sound.MIDI.File                  as MidiFile
import qualified Sound.MIDI.KeySignature          as MidiKeySignature
import qualified Sound.MIDI.File.Event            as Event
import qualified Sound.MIDI.File.Event.Meta       as Meta
import qualified Sound.MIDI.General               as GeneralMidi
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.File.Save             as SaveFile
import qualified Data.ByteString.Lazy             as LazyByteString

newtype Duration = Dur { getDur :: Integer } deriving (Read, Show, Ord, Eq, Num)

-- | Midi-ism:  bounds for Duration are from Midi
instance Bounded Duration where
    minBound = Dur 0
    maxBound = Dur 0x0FFFFFFF

-- | Midi-ism:  Duration is in 64th notes, at default of 128 ticks per quarter or 512 ticks per whole note translation is (* 8 * 64)
--   e.g. 1:1 for whole note * 64 * 8 => 512 ticks.  NB: conversion to Midi doesn't accept ratios that don't divide evenly into 512.
rhythmToDuration :: Rhythm -> Duration
rhythmToDuration rhythm
  | 1 /= toInteger (denominator (512%1 * r)) = error $ "rhythm " ++ show rhythm ++ " does not evenly multiply by 512%1, result " ++ show ((512%1) * r)
  | otherwise = fromIntegral $ fromEnum $ 512%1 * r
  where
    r = getRhythm rhythm
    
pitchClassToOffset :: (Num a) => PitchClass -> a
pitchClassToOffset Bs  = 0
pitchClassToOffset C   = 0
pitchClassToOffset Dff = 0
pitchClassToOffset Bss = 1
pitchClassToOffset Cs  = 1
pitchClassToOffset Df  = 1
pitchClassToOffset Css = 2
pitchClassToOffset D   = 2
pitchClassToOffset Eff = 2
pitchClassToOffset Ds  = 3
pitchClassToOffset Ef  = 3
pitchClassToOffset Fff = 3
pitchClassToOffset Dss = 4
pitchClassToOffset E   = 4
pitchClassToOffset Ff  = 4
pitchClassToOffset Es  = 5
pitchClassToOffset F   = 5
pitchClassToOffset Gff = 5
pitchClassToOffset Ess = 6
pitchClassToOffset Fs  = 6
pitchClassToOffset Gf  = 6
pitchClassToOffset Fss = 7
pitchClassToOffset G   = 7
pitchClassToOffset Aff = 7
pitchClassToOffset Gs  = 8
pitchClassToOffset Af  = 8
pitchClassToOffset Gss = 9
pitchClassToOffset A   = 9
pitchClassToOffset Bff = 9
pitchClassToOffset As  = 10
pitchClassToOffset Bf  = 10
pitchClassToOffset Cff = 10
pitchClassToOffset Ass = 11
pitchClassToOffset B   = 11
pitchClassToOffset Cf  = 11

-- | Translates to Midi dynamic control, e.g. swells on a sustained pitch, or just overall loudness.
dynamicToVolume :: Num a => Dynamic -> a
dynamicToVolume NoDynamic  = 50
dynamicToVolume Pianissimo = 10
dynamicToVolume Piano      = 30
dynamicToVolume MezzoPiano = 50
dynamicToVolume MezzoForte = 80
dynamicToVolume Forte      = 100
dynamicToVolume Fortissimo = 120

-- | VoiceMsg.normalVelocity => Velocity {fromVelocity = 64}
accentToVelocity :: Num a => Accent -> a
accentToVelocity Softest  = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity) - 60
accentToVelocity VerySoft = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity) - 40
accentToVelocity Soft     = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity) - 20
accentToVelocity Normal   = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity)
accentToVelocity Hard     = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity) + 20
accentToVelocity VeryHard = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity) + 40
accentToVelocity Hardest  = fromIntegral (VoiceMsg.fromVelocity VoiceMsg.normalVelocity) + 60

-- | Balance enum -> Balance value
balanceToBalance :: Num a => Balance -> a
balanceToBalance NoBalance       = 64
balanceToBalance LeftBalance     = 1
balanceToBalance MidLeftBalance  = 32
balanceToBalance CenterBalance   = 64
balanceToBalance MidRightBalance = 96
balanceToBalance RightBalance    = 127

-- | Articulation -> sustenuto value
articulationToSustenuto :: Num a => Articulation -> a
articulationToSustenuto NoArticulation = 64
articulationToSustenuto Legato         = 127
articulationToSustenuto Marcato        = 64
articulationToSustenuto Staccato       = 0

pitchToMidi :: Pitch -> Int
pitchToMidi (Pitch pitchClass oct) =
  let
    pitchOffset = pitchClassToOffset pitchClass
    octaveOffset = getOctave $ oct * 12
    midiOffset = 60
  in
    pitchOffset + octaveOffset + midiOffset

stringToDrum :: String -> GeneralMidi.Drum
stringToDrum instr = GeneralMidi.drums !! Data.Maybe.fromJust (Data.List.elemIndex instr (map show GeneralMidi.drums))

-- | Simplify Note from Music.Note by collapsing
--   unpitched instances into "pitched" ones where
--   pitch is mapped by drum following midi map
data MidiNote = MidiNote VoiceMsg.Pitch Rhythm
              | AccentedMidiNote VoiceMsg.Pitch Rhythm Accent
              | MidiRest Rhythm

-- | MidiVoice equates to Music.Voice but
--   with channel and with MidiNotes instead
--   of Notes
data MidiVoice = MidiVoice Instrument ChannelMsg.Channel [MidiNote] [[Control]]

-- | Compose Sound types into generic event for streaming.
genEvent :: ChannelMsg.Channel -> VoiceMsg.T -> Event.T
genEvent chan = Event.MIDIEvent . ChannelMsg.Cons chan . ChannelMsg.Voice

-- | Create midi note on event
genMidiNoteOn :: ChannelMsg.Channel -> VoiceMsg.Pitch -> Accent -> Event.T
genMidiNoteOn channel pitch accent =
    genEvent channel $ VoiceMsg.NoteOn pitch vel
    where
      vel = VoiceMsg.toVelocity (accentToVelocity accent)

-- | Create midi note off event
genMidiNoteOff :: ChannelMsg.Channel -> VoiceMsg.Pitch -> Accent -> Event.T
genMidiNoteOff channel pitch accent =
    genEvent channel $ VoiceMsg.NoteOff pitch vel
    where
      vel = VoiceMsg.toVelocity (accentToVelocity accent)

-- | Create two element list, each with pair containing duration and Sound event
--   where first element contains delay and note on (e.g. for preceding rest)
--   and second contains duration and note off (e.g. for length of note).
genMidiNoteEvents :: Duration -> ChannelMsg.Channel -> VoiceMsg.Pitch -> Duration -> Accent -> [(Duration, Event.T)]
genMidiNoteEvents delay channel pitch duration accent
  | minBound > delay = error $ "genMidiNoteEvents delay " ++ show delay ++ " is less than minimum value " ++ show (minBound::Duration)
  | maxBound < delay = error $ "genMidiNoteEvents delay " ++ show delay ++ " is greater than minimum value " ++ show (maxBound::Duration)
  | minBound > duration = error $ "genMidiNoteEvents duration " ++ show duration ++ " is less than minimum value " ++ show (minBound::Duration)
  | maxBound < duration = error $ "genMidiNoteEvents duration " ++ show duration ++ " is greater than minimum value " ++ show (maxBound::Duration)
  | otherwise = [(delay, genMidiNoteOn channel pitch accent), (duration, genMidiNoteOff channel pitch accent)]

-- | Translate the note-on/note-off pair of events for a MidiNote into
--   a State where the answer is the duration carried over from the
--   previous rest or rests and the state is the "running" rest for
--   the next event and a list of durations and  notes produced so far.
--   Note: so the final answer may yield a period of rest at the end?
--   Is it necessary to have the first item in the pair that is the
--   state be the rest as well as ther answer to the state?
midiNoteToEvents :: ChannelMsg.Channel -> MidiNote -> State (Duration, [(Duration, Event.T)]) Duration
midiNoteToEvents ch (MidiNote pitch rhythm) =
  do (rest, events) <- get
     put (Dur 0, events ++ genMidiNoteEvents rest ch pitch (rhythmToDuration rhythm) Normal)
     return (Dur 0)
midiNoteToEvents ch (AccentedMidiNote pitch rhythm accent) =
  do (rest, events) <- get
     put (Dur 0, events ++ genMidiNoteEvents rest ch pitch (rhythmToDuration rhythm) accent)
     return (Dur 0)
midiNoteToEvents _ (MidiRest rhythm) = 
  do (rest, events) <- get
     put (dur + rest, events)
     return $ dur + rest
  where
    dur = rhythmToDuration rhythm

genMidiDynamicControlEvent :: ChannelMsg.Channel -> Dynamic -> Event.T
genMidiDynamicControlEvent chan dynamic =
  genEvent chan (VoiceMsg.Control VoiceMsg.mainVolume $ dynamicToVolume dynamic)
         
genMidiBalanceControlEvent :: ChannelMsg.Channel -> Balance -> Event.T
genMidiBalanceControlEvent chan balance =
  genEvent chan (VoiceMsg.Control VoiceMsg.panorama $ balanceToBalance balance)
  
genMidiPanControlEvent :: ChannelMsg.Channel -> Pan -> Event.T
genMidiPanControlEvent chan pan
  | minBound > pan = error $ "genMidiPanControlEvent Pan value " ++ show pan ++ " is less than minimum " ++ show (minBound::Pan)
  | maxBound < pan = error $ "genMidiPanControlEvent Pan value " ++ show pan ++ " is greater than than maximum " ++ show (maxBound::Pan)
  | otherwise = genEvent chan (VoiceMsg.Control VoiceMsg.panorama (getPan pan))

genMidiTempoMetaEvent :: Tempo -> Event.T
genMidiTempoMetaEvent (Tempo tempo) =
  (Event.MetaEvent . Meta.SetTempo) $ Meta.toTempo tempo
  
genMidiKeySignatureMetaEvent :: KeySignature -> Event.T
genMidiKeySignatureMetaEvent (KeySignature countAccidentals) =
  (Event.MetaEvent . Meta.KeySig) $ MidiKeySignature.Cons MidiKeySignature.Major (MidiKeySignature.Accidentals countAccidentals)
  
genMidiSustenutoControlEvent :: ChannelMsg.Channel -> Articulation -> Event.T    
genMidiSustenutoControlEvent chan articulation =
  genEvent chan (VoiceMsg.Control VoiceMsg.sustenuto $ articulationToSustenuto articulation)

genMidiInstrumentControlEvent :: ChannelMsg.Channel -> Instrument -> Event.T    
genMidiInstrumentControlEvent chan (Instrument instrName) =
  genEvent chan (VoiceMsg.ProgramChange instr)
  where
    instr = Data.Maybe.fromJust $ GeneralMidi.instrumentNameToProgram instrName

genMidiTextMetaEvent :: String -> Event.T
genMidiTextMetaEvent  = Event.MetaEvent . Meta.TextEvent

controlToEvent :: ChannelMsg.Channel -> Control -> (Duration, Event.T)
controlToEvent channel (DynamicControl dynamic rhythm) =
  (rhythmToDuration rhythm, genMidiDynamicControlEvent channel dynamic)
controlToEvent channel (BalanceControl balance rhythm) =
  (rhythmToDuration rhythm, genMidiBalanceControlEvent channel balance)
controlToEvent channel (PanControl pan rhythm) =
  (rhythmToDuration rhythm, genMidiPanControlEvent channel pan)
controlToEvent _ (TempoControl tempo rhythm) =
  (rhythmToDuration rhythm, genMidiTempoMetaEvent tempo)
controlToEvent _ (KeySignatureControl keySignature rhythm) =
  (rhythmToDuration rhythm, genMidiKeySignatureMetaEvent keySignature)
controlToEvent channel (ArticulationControl articulation rhythm) =
  (rhythmToDuration rhythm, genMidiSustenutoControlEvent channel articulation)
controlToEvent channel (InstrumentControl instrument rhythm) =
  (rhythmToDuration rhythm, genMidiInstrumentControlEvent channel instrument)
controlToEvent _ (TextControl text rhythm) =
  (rhythmToDuration rhythm, genMidiTextMetaEvent text)
    
durEventToNumEvent :: Num a => (Duration, Event.T) -> (a, Event.T)
durEventToNumEvent (Dur dur, event) = (fromInteger dur, event)
--durEventToNumEvent (dur, event) = (fmap (,) durationFromInteger) dur event

-- | Traverse notes accumulating and emiting rests, converting to Midi
--   traverse controls converting to Midi,
--   convert results to single event list merged in time.
midiVoiceToEventList :: MidiVoice -> EventList.T Event.ElapsedTime Event.T
midiVoiceToEventList (MidiVoice (Instrument instrName) channel notes controlss)
  | null noteEventLists = EventList.empty
  | null ctrlEventLists = progAndNoteEvents
  | otherwise           = EventList.merge progAndNoteEvents ctrlEvents
  where
    instr = Data.Maybe.fromJust $ GeneralMidi.instrumentNameToProgram instrName
    noteEventss = map (\ns -> snd $ execState (traverse (midiNoteToEvents channel) ns) (Dur 0, [])) [notes]
    noteEventLists = map (EventList.fromPairList . map durEventToNumEvent) noteEventss
    ctrlEventss = map (map (controlToEvent channel)) controlss
    ctrlEventLists = map (EventList.fromPairList . map durEventToNumEvent) ctrlEventss
    progAndNoteEvents = EventList.cons 0 (genEvent channel (VoiceMsg.ProgramChange instr)) (foldl1 EventList.merge noteEventLists)
    ctrlEvents = foldl1 EventList.merge ctrlEventLists

-- | rhythmToDuration(Rhythm(1%4)) == 512%1 * 1%4 == 512%4 == 128 ticks per quarter note
standardTicks :: MidiFile.Division
standardTicks = MidiFile.Ticks $ fromIntegral $ getDur (rhythmToDuration (Rhythm (1%4))) -- 1:1 for Duration:Tick

-- | Collapse PercussionNote and AccentedPercussionNote
--   from Note to MidiNote and AccentedMidiNote for
--   MidiNote by mapping percussion instrument to
--   pitch.  Then conversion from MidiNote to MidiEvent
--   can be uniform for pitched and non-pitched
--   instruments.
noteToMidiNote :: Instrument -> Note -> MidiNote
noteToMidiNote _ (Note pitch rhythm) =
  MidiNote (VoiceMsg.toPitch (pitchToMidi pitch)) rhythm
noteToMidiNote _ (AccentedNote pitch rhythm accent) =
  AccentedMidiNote (VoiceMsg.toPitch (pitchToMidi pitch)) rhythm accent
noteToMidiNote (Instrument instr) (AccentedPercussionNote rhythm accent) =
  AccentedMidiNote (GeneralMidi.drumToKey (stringToDrum instr)) rhythm accent
noteToMidiNote _ (Rest rhythm) =
  MidiRest rhythm
noteToMidiNote (Instrument instr) (PercussionNote rhythm) =
  MidiNote (GeneralMidi.drumToKey (stringToDrum instr)) rhythm

isMidiPercussion :: String -> Bool
isMidiPercussion = flip elem (map show GeneralMidi.drums)

voiceAndChannelToMidiVoice :: Voice -> ChannelMsg.Channel -> MidiVoice
voiceAndChannelToMidiVoice (Voice instr notes controlss) channel =
  MidiVoice instr channel midiNotes controlss
  where
    midiNotes = map (noteToMidiNote instr) notes
    
-- | Given a MidiVoice, convert instrument, channel, notes, and
--   controls to midi, then assemble MidiFile.T in preparation
--   for Sound.MIDI.File.Save.toByteString.
midiVoiceToMidiFile :: MidiVoice -> MidiFile.T
midiVoiceToMidiFile midiVoice =
  MidiFile.Cons MidiFile.Mixed standardTicks [voiceEventList]
  where
    voiceEventList = midiVoiceToEventList midiVoice
    
-- | Given title, voice, and part number, generate
--   title, convert voice to create midi file byte
--   string and write file.
titleAndMidiVoiceToMidiFile :: Title -> MidiVoice -> Int -> IO ()
titleAndMidiVoiceToMidiFile title voice@(MidiVoice (Instrument instr) _ _ _) part =
  LazyByteString.writeFile fileName $ SaveFile.toByteString midiFile
  where
    fileName = title ++ "-" ++ instr ++ "-" ++ show part ++ ".mid"
    midiFile = midiVoiceToMidiFile voice

-- | Given title and list of voices, create midi file per voice.
titleVoicesAndChannelsToMidiFiles :: Title -> [Voice] -> [ChannelMsg.Channel] -> IO ()
titleVoicesAndChannelsToMidiFiles title voices channels =
  zipWithM_ (titleAndMidiVoiceToMidiFile title) midiVoices [1..]
  where
    midiVoices = zipWith voiceAndChannelToMidiVoice voices channels
      
-- | All percussion voices are the same instrument
--   for the purpose of track allocation in a multi-track
--   file.
equalVoiceByInstrument :: Voice -> Voice -> Bool
equalVoiceByInstrument (Voice (Instrument instr1) _ _) (Voice (Instrument instr2) _ _) =
  isMidiPercussion instr1 && isMidiPercussion instr2 || instr1 == instr2
  
-- | All percussion voices are equal instruments 
--   for the purpose of track allocation in a multi-track
--   file.
orderVoiceByInstrument :: Voice -> Voice -> Ordering
orderVoiceByInstrument (Voice (Instrument instr1) _ _) (Voice (Instrument instr2) _ _)
  | isMidiPercussion instr1 && isMidiPercussion instr2 = EQ
  | otherwise = instr1 `compare` instr2

-- | Organize a list of voices into a list of list of
--   voices grouped by instrument where all percussion
--   instruments are one instrument.
collectVoicesByInstrument :: [Voice] -> [[Voice]]
collectVoicesByInstrument =
  groupVoicesByInstrument . sortVoicesByInstrument
  where
    groupVoicesByInstrument = groupBy equalVoiceByInstrument
    sortVoicesByInstrument = sortBy orderVoiceByInstrument

-- | Map list of list of voice to list of list of midi channel
--   using drum channel for percussion voices, otherwise channel
--   zero.  Use when emitting Midi file per voice to be assembled
--   later on, e.g. with Logic or GarageBand.
assignAnyMidiChannelsByVoices :: [[Voice]] -> [[ChannelMsg.Channel]]
assignAnyMidiChannelsByVoices =
  map toMidiChannelss
  where
    toMidiChannelss = map toAnyMidiChannel 
    toAnyMidiChannel (Voice (Instrument instr) _ _)
      | isMidiPercussion instr = GeneralMidi.drumChannel
      | otherwise = ChannelMsg.toChannel 0
    
-- | Collect list of voices into list of list
--   of voices with same instrument, then
--   create a parallel list of list of Midi
--   channels for each voices and use the
--   title, list of list of voices, and list
--   of list of channels to create individual
--   Midi files, one per voice, with name e.g.
--   "<title>-<instrument>-1.mid".
scoreToMidiFiles :: Score -> IO ()
scoreToMidiFiles (Score title voices) =
  zipWithM_ (titleVoicesAndChannelsToMidiFiles title) voicess channelss
  where
    voicess = collectVoicesByInstrument voices
    channelss = assignAnyMidiChannelsByVoices voicess

-- | Collect list of voices into list of list
--   of voices with same instrument, then
--   create a parallel list of list of the
--   same Midi channel for each voices and use 
--   the title, list of list of voices, and list
--   of list of channels to create as a Midi
--   file for each voice.  Unlimited by Midi
--   track count constraints.  But files must
--   be assembled one-by-one into editor like
--   Logic or GarageBand.
voicessAndChannelssToByteString :: [[Voice]] -> [[ChannelMsg.Channel]] -> LazyByteString.ByteString
voicessAndChannelssToByteString voicess channelss
  | null voicess = error "voicessAndChannelssToOneMidiFile empty voicess"
  | null channelss = error "voicessAndChannelssToOneMidiFile empty channelss"
  | length voicess /= length channelss = error $ "voicessAndChannelssToOneMidiFile mismatched lengths voicess: " ++ show (length voicess) ++ " channelss: " ++ show (length channelss)
  | null midiVoices = error "voicessAndChannelssToOneMidiFile empty midiVoices"
  | null voiceEventLists = error "voicessAndChannelssToOneMidiFile empty voiceEventLists"
  | otherwise = SaveFile.toByteString midiFile
  where
    midiVoices = concat $ (zipWith . zipWith) voiceAndChannelToMidiVoice voicess channelss
    voiceEventLists = map midiVoiceToEventList midiVoices
    midiFile = MidiFile.Cons MidiFile.Mixed standardTicks [foldl1 EventList.merge voiceEventLists]
    
titleVoicessAndChannelssToOneMidiFile :: Title -> [[Voice]] -> [[ChannelMsg.Channel]] -> IO ()
titleVoicessAndChannelssToOneMidiFile title voicess channelss =
  LazyByteString.writeFile fileName byteStream
  where
    fileName = title ++ ".mid"
    byteStream = voicessAndChannelssToByteString voicess channelss

-- | Map list of list of voice to list of list of midi channel
--   using drum channel for percussion voices, otherwise channels
--   0 to 15, with 9 reserved for percussion instruments (if any).
--   TBD: refactor using traverse.        
assignCommonMidiChannelsByVoicess :: [[Voice]] -> [[ChannelMsg.Channel]]
assignCommonMidiChannelsByVoicess voicess =
  reverse $ snd $ foldl foldVoices (0, []) voicess
  where
    foldVoices (count, chanss) voices
      | isMidiPercussion instr = (count, replicate countVoices GeneralMidi.drumChannel : chanss)
      | otherwise = (count', replicate countVoices (ChannelMsg.toChannel count) : chanss)
      where
        (Voice (Instrument instr) _ _) = head voices
        countVoices = length voices
        isAnyPerc = (any . any) isPercVoice voicess
        isPercVoice (Voice (Instrument instrName) _ _) = isMidiPercussion instrName
        count'
          | isAnyPerc = count + 1
          | otherwise = (+) count $ if count + 1 == ChannelMsg.fromChannel GeneralMidi.drumChannel then 2 else 1

-- | Deep copy from [[Voice]] to [[ChannelMsg.Channel]]
--   incrementing channel per Voice, for use only when
--   count of all Voice across [[Voice]] is less than 16,
--   otherwise ChannelMsg.Channel > 16 violates Midi
--   constraint and fails when converting.  Reserve
--   channel 9 for drums, if any are present.
--   TBD: refactor using traverse.        
assignUniqueMidiChannelsByVoicess :: [[Voice]] -> [[ChannelMsg.Channel]]
assignUniqueMidiChannelsByVoicess voicess =
  reverse $ snd $ foldl foldVoices (0, []) voicess 
  where
    foldVoices (count, chanss) voices =
      (count', chans : chanss)
      where
        (count', chans) = foldl foldVoice (count, []) voices
          where
            isAnyPerc = any isPercVoice voices
            isPercVoice (Voice (Instrument instr) _ _) = isMidiPercussion instr
            foldVoice (count'', chans') (Voice (Instrument instr) _ _) 
              | isMidiPercussion instr = (count'', GeneralMidi.drumChannel : chans')
              | otherwise = (next, ChannelMsg.toChannel count'' : chans')
              where
                next
                  | isAnyPerc = count'' + 1
                  | otherwise = (+) count'' (if count'' + 1 == ChannelMsg.fromChannel GeneralMidi.drumChannel then 2 else 1)

-- | Traverse [[Voice]] assigning ChannelMsg.Channel either
--   individually Voice by Voice e.g. if count of Voice is
--   less than highest allowed enum value (16) else in blocks
--   with same ChannelMsg.Channel per [Voice], in which case,
--   count of [Voice] still cannot exceed 16.
assignMidiChannelsByVoices :: [[Voice]] -> [[ChannelMsg.Channel]]
assignMidiChannelsByVoices voicess 
  | countAllVoices < 16 = assignUniqueMidiChannelsByVoicess voicess
  | otherwise =  assignCommonMidiChannelsByVoicess voicess
  where
    countAllVoices = sum $ map length voicess

-- | Collect list of voices into list of list
--   of voices with same instrument, then
--   create a parallel list of list of Midi
--   channels for each voices and use the
--   title, list of list of voices, and list
--   of list of channels to create one Midi
--   file with all the voices together, name
--   e.g. "<title>.mid". Limited by Midi constraint
--   of 1 percussion track and 15 non-percussion
--   tracks.
scoreToMidiFile :: Score -> IO ()
scoreToMidiFile (Score title voices)
  | countInstruments > 16 = error $ "scoreToMidiFile, count of instruments: " ++ show countInstruments ++ " exceeds count of Midi channels: 16"
  | otherwise = titleVoicessAndChannelssToOneMidiFile title voicess channelss
  where
    voicess = collectVoicesByInstrument voices
    countInstruments = length voicess
    channelss = assignMidiChannelsByVoices voicess

scoreToByteString :: Score -> LazyByteString.ByteString
scoreToByteString (Score _ voices)
  | countInstruments > 16 = error $ "scoreToMidiFile, count of instruments: " ++ show countInstruments ++ " exceeds count of Midi channels: 16"
  | otherwise = voicessAndChannelssToByteString voicess channelss
  where
    voicess = collectVoicesByInstrument voices
    countInstruments = length voicess
    channelss = assignMidiChannelsByVoices voicess
