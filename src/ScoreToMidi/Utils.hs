{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScoreToMidi.Utils where

import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.EventList.Relative.TimeBody as EventList
import           Data.List
import           Data.Maybe
import           Data.Ratio
import qualified Data.Set as Set
import           Data.Traversable
import           Music.Data
import           Music.Utils ()
import qualified Sound.MIDI.File as MidiFile
import qualified Sound.MIDI.File.Event as Event
import qualified Sound.MIDI.File.Event.Meta as Meta
import qualified Sound.MIDI.File.Save as SaveFile
import qualified Sound.MIDI.General as GeneralMidi
import qualified Sound.MIDI.KeySignature as MidiKeySignature
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

maxMidiTrack :: Int
maxMidiTrack = 16

newtype Duration = Dur { getDur :: Integer } deriving (Read, Show, Ord, Eq, Num)

-- | Midi-ism:  bounds for Duration are from Midi
instance Bounded Duration where
    minBound = Dur 0
    maxBound = Dur 0x0FFFFFFF

-- | Midi-ism:  Duration is in 64th notes, at default of 128 ticks per quarter or 512 ticks per whole note translation is (* 8 * 64)
--   e.g. 1:1 for whole note * 64 * 8 => 512 ticks.  NB: conversion to Midi doesn't accept ratios that don't divide evenly into 512.
rhythmToDuration :: Rhythm -> Duration
rhythmToDuration rhythm
  | 1 /= toInteger (denominator (512%1 * rat)) = error $ "rhythm " ++ show rhythm ++ " does not evenly multiply by 512%1, result " ++ show ((512%1) * rat)
  | otherwise = fromIntegral $ fromEnum $ 512%1 * rat
  where
    rat = getRhythm rhythm
    
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
balanceToBalance LeftBalance     = 1
balanceToBalance MidLeftBalance  = 32
balanceToBalance CenterBalance   = 64
balanceToBalance MidRightBalance = 96
balanceToBalance RightBalance    = 127

-- | Articulation -> sustenuto value
articulationToSustenuto :: Num a => Articulation -> a
articulationToSustenuto Tenuto         =  96
articulationToSustenuto Portato        =  80
articulationToSustenuto NoArticulation =  64
articulationToSustenuto Marcato        =  32
articulationToSustenuto Staccato       =  16
articulationToSustenuto Staccatissimo  =   0

pitchToMidi :: Pitch -> Int
pitchToMidi (Pitch pitchClass oct) =
  pitchOffset + octaveOffset + midiOffset
  where
    pitchOffset  = pitchClassToOffset pitchClass
    octaveOffset = getOctave $ oct * 12
    midiOffset   = 60

stringToDrum :: String -> GeneralMidi.Drum
stringToDrum instr = GeneralMidi.drums !! Data.Maybe.fromJust (Data.List.elemIndex instr (map show GeneralMidi.drums))

-- | Simplify Note from Music.Note by collapsing
--   unpitched instances into "pitched" ones where
--   pitch is mapped by drum following midi map
data MidiNote =
  MidiNote VoiceMsg.Pitch Rhythm (Set.Set Control)
  | MidiRest Rhythm (Set.Set Control)

-- | MidiVoice equates to Music.Voice but
--   with channel and with MidiNotes instead
--   of Notes
data MidiVoice = MidiVoice Instrument ChannelMsg.Channel [MidiNote]

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
  | otherwise      = genEvent chan (VoiceMsg.Control VoiceMsg.panorama (getPan pan))

-- | Midi Channel Prefix tells system what channel to associate with
--   meta events before an event with a channel appears.  Customary
--   to use channel 1 at the start of a file.
genMidiPrefixMetaEvent :: ChannelMsg.Channel -> Event.T
genMidiPrefixMetaEvent = (Event.MetaEvent . Meta.MIDIPrefix)

-- | Microseconds per quarter note, default 120 beats per minute is
--   500,000 or defltTempo, so that'd be for a quarter that gets 120.
genMidiTempoMetaEvent :: Tempo -> Event.T
genMidiTempoMetaEvent (Tempo (Rhythm rhythm) beats) =
  (Event.MetaEvent . Meta.SetTempo) $ Meta.toTempo microsPerRhythm
  where
    microsPerMinute   = 60000000
    rhythmsPerQuarter = (denominator rhythm) % 4
    microsPerRhythm   = floor $ (rhythmsPerQuarter * microsPerMinute) / (beats % 1)
  
genMidiKeySignatureMetaEvent :: KeySignature -> Event.T
genMidiKeySignatureMetaEvent (KeySignature countAccidentals) =
  (Event.MetaEvent . Meta.KeySig) $ MidiKeySignature.Cons MidiKeySignature.Major (MidiKeySignature.Accidentals countAccidentals)

-- | Numerator is what you'd expect.  Denominator is interpreted
--   0 -> whole, 1 -> half, 2 -> quarter, 3 -> eighth, etc.
genMidiTimeSignatureMetaEvent :: TimeSignature -> Event.T
genMidiTimeSignatureMetaEvent (TimeSignature num denom) =
  Event.MetaEvent $ Meta.TimeSig (fromIntegral num) ((fromIntegral denom) `div` 2) 0 0 -- metronome, n32notes
  
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

foldControl :: ChannelMsg.Channel -> [Event.T] -> Control -> [Event.T]
foldControl _       events (SwellControl         _)             =                                                       events -- TBD
foldControl channel events (DynamicControl       dynamic)       = (genMidiDynamicControlEvent    channel dynamic)      :events
foldControl channel events (BalanceControl       balance)       = (genMidiBalanceControlEvent    channel balance)      :events
foldControl channel events (PanControl           pan)           = (genMidiPanControlEvent        channel pan)          :events
foldControl _       events (TempoControl         tempo)         = (genMidiTempoMetaEvent         tempo)                :events
foldControl _       events (KeySignatureControl  keySignature)  = (genMidiKeySignatureMetaEvent  keySignature)         :events
foldControl _       events (TimeSignatureControl timeSignature) = (genMidiTimeSignatureMetaEvent timeSignature)        :events
foldControl channel events (ArticulationControl  articulation)  = (genMidiSustenutoControlEvent  channel articulation) :events
foldControl channel events (InstrumentControl    instrument)    = (genMidiInstrumentControlEvent channel instrument)   :events
foldControl _       events (TextControl          text)          = (genMidiTextMetaEvent          text)                 :events
foldControl _       events (AccentControl        _)             =                                                       events

accentFromControl :: Control -> Accent
accentFromControl control =
  case control of
    SwellControl _         -> error $ "accentFromControl expected Accent, got Swell"
    DynamicControl _       -> error $ "accentFromControl expected Accent, got Dynamic"
    BalanceControl _       -> error $ "accentFromControl expected Accent, got Balance"
    PanControl _           -> error $ "accentFromControl expected Accent, got Pan"
    TempoControl _         -> error $ "accentFromControl expected Accent, got Tempo"
    KeySignatureControl _  -> error $ "accentFromControl expected Accent, got KeySignature"
    TimeSignatureControl _ -> error $ "accentFromControl expected Accent, got TimeSignature"
    ArticulationControl _  -> error $ "accentFromControl expected Accent, got Articulation"
    TextControl _          -> error $ "accentFromControl expected Accent, got Text"
    InstrumentControl _    -> error $ "accentFromControl expected Accent, got Instrument"
    AccentControl accent   -> accent

lookupAccent :: (Set.Set Control) -> Accent
lookupAccent controls =
  case Set.lookupIndex (AccentControl Normal) controls of
   Nothing -> Normal
   Just ix -> accentFromControl $ Set.elemAt ix controls

-- | Create two element list, each with pair containing duration and Sound event
--   where first element contains delay and note on (e.g. for preceding rest)
--   and second contains duration and note off (e.g. for length of note).
genMidiNoteEvents :: Duration -> ChannelMsg.Channel -> VoiceMsg.Pitch -> Duration -> (Set.Set Control) -> [(Duration, Event.T)]
genMidiNoteEvents delay channel pitch duration controls
  | minBound > delay    = error $ "genMidiNoteEvents delay " ++ show delay ++ " is less than minimum value " ++ show (minBound::Duration)
  | maxBound < delay    = error $ "genMidiNoteEvents delay " ++ show delay ++ " is greater than minimum value " ++ show (maxBound::Duration)
  | minBound > duration = error $ "genMidiNoteEvents duration " ++ show duration ++ " is less than minimum value " ++ show (minBound::Duration)
  | maxBound < duration = error $ "genMidiNoteEvents duration " ++ show duration ++ " is greater than minimum value " ++ show (maxBound::Duration)
  | otherwise           = zip durations events
  where
    accent          = lookupAccent controls
    onEvent         = genMidiNoteOn  channel pitch accent
    offEvent        = genMidiNoteOff channel pitch accent
    controlEvents   = Set.foldl (foldControl channel) [] controls
    events          = controlEvents ++ [onEvent, offEvent]
    durations       = [delay] ++ replicate (length controlEvents) (Dur 0) ++ [duration]

genMidiControlEvents :: ChannelMsg.Channel -> (Set.Set Control) -> [(Duration, Event.T)]
genMidiControlEvents channel controls =
  zip (replicate (Set.size controls) (Dur 0)) $ Set.foldl (foldControl channel) [] controls

midiNoteToEvents :: ChannelMsg.Channel -> MidiNote -> State (Duration, [(Duration, Event.T)]) Duration
midiNoteToEvents ch (MidiNote pitch rhythm controls) =
  do (rest, events) <- get
     put (Dur 0, events ++ genMidiNoteEvents rest ch pitch (rhythmToDuration rhythm) controls)
     return (Dur 0)
midiNoteToEvents ch (MidiRest rhythm controls) =  
  do (rest, events) <- get
     put (dur + rest, events ++ genMidiControlEvents ch controls)
     return $ dur + rest
  where
    dur = rhythmToDuration rhythm
    
durEventToNumEvent :: Num a => (Duration, Event.T) -> (a, Event.T)
durEventToNumEvent (Dur dur, event) = (fromInteger dur, event)
--durEventToNumEvent (dur, event) = (fmap (,) durationFromInteger) dur event

-- | Traverse notes accumulating and emiting rests, converting to Midi
--   traverse controls converting to Midi,
--   convert results to single event list merged in time.
midiVoiceToEventList :: MidiVoice -> EventList.T Event.ElapsedTime Event.T
midiVoiceToEventList (MidiVoice (Instrument instrName) channel notes)
  | null noteEventLists = EventList.empty
  | otherwise           = progAndNoteEvents
  where
    instr             = Data.Maybe.fromJust $ GeneralMidi.instrumentNameToProgram instrName
    noteEvents        = map (\ns -> snd $ execState (traverse (midiNoteToEvents channel) ns) (Dur 0, [])) [notes]
    noteEventLists    = map (EventList.fromPairList . map durEventToNumEvent) noteEvents
    progAndNoteEvents = EventList.cons 0 (genEvent channel (VoiceMsg.ProgramChange instr)) (foldl1 EventList.merge noteEventLists)

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
noteToMidiNote _ (Note pitch rhythm controls) =
  MidiNote (VoiceMsg.toPitch (pitchToMidi pitch)) rhythm controls
noteToMidiNote _ (Rest rhythm controls) =
  MidiRest rhythm controls
noteToMidiNote (Instrument instr) (PercussionNote rhythm controls) =
  MidiNote (GeneralMidi.drumToKey (stringToDrum instr)) rhythm controls

isMidiPercussion :: String -> Bool
isMidiPercussion = flip elem (map show GeneralMidi.drums)

isMidiPercussionInstrument :: Instrument -> Bool
isMidiPercussionInstrument (Instrument name) = isMidiPercussion name

isMidiPercussionVoice :: Voice -> Bool
isMidiPercussionVoice = isMidiPercussionInstrument . voiceInstrument

isMidiPercussionVoices :: [Voice] -> Bool
isMidiPercussionVoices = any isMidiPercussionVoice

isMidiInstrument :: Instrument -> Bool
isMidiInstrument (Instrument instrName) =
  isJust $ GeneralMidi.instrumentNameToProgram instrName

voiceAndChannelToMidiVoice :: Voice -> ChannelMsg.Channel -> MidiVoice
voiceAndChannelToMidiVoice (Voice instr notes) channel =
  MidiVoice instr channel midiNotes
  where
    midiNotes = map (noteToMidiNote instr) notes
    
-- | Given a MidiVoice, convert instrument, channel, notes, and
--   controls to midi, then assemble MidiFile.T in preparation
--   for Sound.MIDI.File.Save.toByteString.
midiVoiceToMidiFile :: EventList.T Event.ElapsedTime Event.T -> MidiVoice -> MidiFile.T
midiVoiceToMidiFile metaEvents midiVoice =
  MidiFile.Cons MidiFile.Mixed standardTicks $ [EventList.merge metaEvents voiceEventList]
  where
    voiceEventList = midiVoiceToEventList midiVoice

-- The first track of a Format 1 file is special, and is also known as the 'Tempo Map'.
-- It should contain all meta-events of the types Time Signature, and Set Tempo.
-- The meta-events Sequence/Track Name, Sequence Number, Marker, and SMTPE Offset
-- should also be on the first track of a Format 1 file.

-- | Generate meta events for start of file.
scoreToMetaEvents :: Score -> EventList.T Event.ElapsedTime Event.T
scoreToMetaEvents (Score _ _ tempo timeSignature keySignature _) =
  foldl1 EventList.merge eventLists
  where
    midiPrefixEventPair    = (Dur 0, genMidiPrefixMetaEvent (ChannelMsg.toChannel 0))
    tempoEventPair         = (Dur 0, genMidiTempoMetaEvent tempo)
    timeSignatureEventPair = (Dur 0, genMidiTimeSignatureMetaEvent timeSignature)
    keySignatureEventPair  = (Dur 0, genMidiKeySignatureMetaEvent keySignature)
    eventLists             = map (EventList.fromPairList . map durEventToNumEvent) [[midiPrefixEventPair], [tempoEventPair], [timeSignatureEventPair], [keySignatureEventPair]]
    
-- | Given title, voice, and part number, generate
--   title, convert voice to create midi file byte
--   string and write file.
scoreAndMidiVoiceToMidiFile :: Score -> MidiVoice -> Int -> IO ()
scoreAndMidiVoiceToMidiFile score@(Score title _ _ _ _ _) voice@(MidiVoice (Instrument instr) _ _) part =
  LazyByteString.writeFile fileName $ SaveFile.toByteString midiFile
  where
    fileName = title ++ "-" ++ instr ++ "-" ++ show part ++ ".mid"
    metaEvents = scoreToMetaEvents score 
    midiFile   = midiVoiceToMidiFile metaEvents voice

-- | Given title and list of voices, create midi file per voice.
scoreVoicesAndChannelsToMidiFiles :: Score -> [Voice] -> [ChannelMsg.Channel] -> IO ()
scoreVoicesAndChannelsToMidiFiles score voices channels =
  zipWithM_ (scoreAndMidiVoiceToMidiFile score) midiVoices [1..]
  where
    midiVoices = zipWith voiceAndChannelToMidiVoice voices channels
      
-- | All percussion voices are the same instrument
--   for the purpose of track allocation in a multi-track
--   file.
equalVoiceByInstrument :: Voice -> Voice -> Bool
equalVoiceByInstrument (Voice (Instrument instr1) _) (Voice (Instrument instr2) _) =
  isMidiPercussion instr1 && isMidiPercussion instr2 || instr1 == instr2
  
-- | All percussion voices are equal instruments 
--   for the purpose of track allocation in a multi-track
--   file.
orderVoiceByInstrument :: Voice -> Voice -> Ordering
orderVoiceByInstrument (Voice (Instrument instr1) _) (Voice (Instrument instr2) _)
  | isMidiPercussion instr1 && isMidiPercussion instr2 = EQ
  | otherwise                                          = instr1 `compare` instr2

-- | Organize a list of voices into a list of list of
--   voices grouped by instrument where all percussion
--   instruments are one instrument and voices list for
--   percussion instruments (if any) are always last.
collectVoicesByInstrumentWithPercussionLast :: [Voice] -> [[Voice]]
collectVoicesByInstrumentWithPercussionLast voices = 
  snd splitVoices ++ fst splitVoices
  where
    groupVoicesByInstrument = groupBy equalVoiceByInstrument
    sortVoicesByInstrument  = sortBy orderVoiceByInstrument
    allVoices               = (groupVoicesByInstrument . sortVoicesByInstrument) voices
    splitVoices             = partition isMidiPercussionVoices allVoices

-- | Map list of list of voice to list of list of midi channel
--   using drum channel for percussion voices, otherwise channel
--   zero.  Use when emitting Midi file per voice to be assembled
--   later on, e.g. with Logic or GarageBand.
mapVoicessToPercussionChannelss :: [[Voice]] -> [[ChannelMsg.Channel]]
mapVoicessToPercussionChannelss voicess = 
  zipWith voiceAndLenToChans (map head voicess) (map length voicess)
  where
    voiceAndLenToChans voice len =
      replicate len $ if isMidiPercussionVoice voice then GeneralMidi.drumChannel else zeroChannel
    zeroChannel =
      ChannelMsg.toChannel 0
    
voicesToNotMidiInstruments :: [Voice] -> [Instrument]
voicesToNotMidiInstruments voices =
  filter (not . isMidiInstrumentOrPercussion) $ map voiceInstrument voices
  where
    isMidiInstrumentOrPercussion instr = isMidiInstrument instr || isMidiPercussionInstrument instr

{--
  RealSimpleMusic API: scoreToMidiFiles
--}
    
-- | Collect list of voices into list of list
--   of voices with same instrument, then
--   create a parallel list of list of Midi
--   channels for each voices and use the
--   title, list of list of voices, and list
--   of list of channels to create individual
--   Midi files, one per voice, with name e.ag.
--   "<title>-<instrument>-1.mid".
scoreToMidiFiles :: Score -> IO ()
scoreToMidiFiles score@(Score _ _ _ _ _ voices) 
  | not (null notMidiInstruments) = error $ "convertScore, found non-midi instrument(s) " ++ show notMidiInstruments
  | otherwise                     = zipWithM_ (scoreVoicesAndChannelsToMidiFiles score) voicess channelss
  where
    notMidiInstruments = voicesToNotMidiInstruments voices
    voicess            = collectVoicesByInstrumentWithPercussionLast voices
    channelss          = mapVoicessToPercussionChannelss voicess

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
scoreVoicessAndChannelssToByteString :: Score -> [[Voice]] -> [[ChannelMsg.Channel]] -> LazyByteString.ByteString
scoreVoicessAndChannelssToByteString score voicess channelss
  | length voicess /= length channelss = error $ "scoreVoicessAndChannelssToOneMidiFile mismatched lengths voicess: " ++ show (length voicess) ++ " channelss: " ++ show (length channelss)
  | null channelss                     = error "scoreVoicessAndChannelssToOneMidiFile empty channelss"
  | null midiVoices                    = error "scoreVoicessAndChannelssToOneMidiFile empty midiVoicess"
  | null voiceEventLists               = error "scoreVoicessAndChannelssToOneMidiFile empty voiceEventLists"
  | null voicess                       = error "scoreVoicessAndChannelssToOneMidiFile empty voicess"
  | otherwise                          = SaveFile.toByteString midiFile
  where
    midiVoices      = concat $ (zipWith . zipWith) voiceAndChannelToMidiVoice voicess channelss
    voiceEventLists = map midiVoiceToEventList midiVoices
    metaEvents      = scoreToMetaEvents score 
    midiFile        = MidiFile.Cons MidiFile.Mixed standardTicks $ [EventList.merge metaEvents (foldl1 EventList.merge voiceEventLists)] 

scoreVoicessAndChannelssToOneMidiFile :: Score -> [[Voice]] -> [[ChannelMsg.Channel]] -> IO ()
scoreVoicessAndChannelssToOneMidiFile score@(Score title _ _ _ _ _) voicess channelss =
  LazyByteString.writeFile fileName byteStream
  where
    fileName   = title ++ ".mid"
    byteStream = scoreVoicessAndChannelssToByteString score voicess channelss

-- | For each Voice in [[Voice]] (given that Instrument is the same),
--   for non-percussion voices, allocate the next Midi channel, else
--   for percussion voices, allocate the single Midi percussion channel.
--   Answer the list of [[ChannelMsg.Channel]] sorted by channel value.
mapVoicessToRepeatedChannelss :: [[Voice]] -> [[ChannelMsg.Channel]]
mapVoicessToRepeatedChannelss voicess = 
  reverse $ snd $ foldl foldVoices (0, []) voicess
  where
    foldVoices (chan, chans) voices =
      if isMidiPercussionVoices voices
      then (chan, replicate countVoices GeneralMidi.drumChannel:chans)
      else (chan + 1, replicate countVoices (ChannelMsg.toChannel chan):chans)
      where
        countVoices = length voices

-- | For each Voice in [[Voice]] (given that Instrument is the same),
--   for non-percussion voices, allocate the next Midi channel, else
--   for percussion voices, allocate the single Midi percussion channel.
--   Answer the list of [[ChannelMsg.Channel]] sorted by channel value.
mapVoicessToUniqueChannelss :: [[Voice]] -> [[ChannelMsg.Channel]]
mapVoicessToUniqueChannelss voicess =
  reverse $ snd $ foldl foldVoices (0, []) voicess
  where
    foldVoices (chan, chans) voices =
      if isMidiPercussionVoices voices
      then (chan, replicate countVoices GeneralMidi.drumChannel:chans)
      else (chan + countVoices, map ChannelMsg.toChannel [chan..(chan + (countVoices - 1))]:chans)
      where
        countVoices = length voices

-- | Given list of list of voices where each list is for the same instrument,
--   return a list of list of channels.  When the count of unique instruments
--   is fewer than the maximum count of Midi channels, then allocate a new
--   channel per voice (allowing for the traditional association of for the
--   percussion channel).  Else, allocate unique channels per list of voice.
mapVoicessToChannelss :: [[Voice]] -> [[ChannelMsg.Channel]]
mapVoicessToChannelss voicess 
  | countAllVoices < maxMidiTrack = mapVoicessToUniqueChannelss voicess
  | otherwise                     = mapVoicessToRepeatedChannelss voicess
  where
    countAllVoices = sum $ map length voicess
      
-- | Refactor
convertScore :: (Score -> [[Voice]] -> [[ChannelMsg.Channel]] -> a) -> Score -> a
convertScore convert score@(Score _ _ _ _ _ voices) 
  | countVoices > maxMidiTrack      = error $ "convertScore, count of voices: " ++ show countVoices ++ " exceeds count of Midi channels: " ++ show maxMidiTrack
  | not (null notMidiInstruments)   = error $ "convertScore, found non-midi instrument(s) " ++ show notMidiInstruments
  | otherwise                       = convert score voicess channelss
  where
    notMidiInstruments = voicesToNotMidiInstruments voices
    voicess            = collectVoicesByInstrumentWithPercussionLast voices
    channelss          = mapVoicessToChannelss voicess
    countVoices        = length voicess
    
{--
  RealSimpleMusic APIs: scoreToMidiFile, scoreToByteString (for test)
--}
                                         
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
scoreToMidiFile = convertScore scoreVoicessAndChannelssToOneMidiFile

-- | Short-circuit write to MidiFile with output to byte string for test.
scoreToByteString :: Score -> LazyByteString.ByteString
scoreToByteString = convertScore scoreVoicessAndChannelssToByteString

{--
bash$ cabal repl
λ: :m +Sound.MIDI.General
λ: Sound.MIDI.General.drums
[AcousticBassDrum,BassDrum1,SideStick,AcousticSnare,HandClap,ElectricSnare,LowFloorTom,ClosedHiHat,HighFloorTom,PedalHiHat,LowTom,OpenHiHat,LowMidTom,HiMidTom,CrashCymbal1,HighTom,RideCymbal1,ChineseCymbal,RideBell,Tambourine,SplashCymbal,Cowbell,CrashCymbal2,Vibraslap,RideCymbal2,HiBongo,LowBongo,MuteHiConga,OpenHiConga,LowConga,HighTimbale,LowTimbale,HighAgogo,LowAgogo,Cabasa,Maracas,ShortWhistle,LongWhistle,ShortGuiro,LongGuiro,Claves,HiWoodBlock,LowWoodBlock,MuteCuica,OpenCuica,MuteTriangle,OpenTriangle]
--}
