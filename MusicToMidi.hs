{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MusicToMidi(
  scoreToMidiFiles
) where

import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Music
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

-- | PanDegrees is integer from 0 to 359
newtype PanDegrees = PanDegrees { getPanDegrees :: Integer } deriving (Show, Ord, Eq, Num)

instance Bounded PanDegrees where
    minBound = PanDegrees 0    --  Mimic circle, maybe replace with -90?
    maxBound = PanDegrees 359  --  Mimic circle, maybe replace with +90?

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
pitchClassToOffset Bs = 0
pitchClassToOffset C  = 0
pitchClassToOffset Cs = 1
pitchClassToOffset Df = 1
pitchClassToOffset D  = 2
pitchClassToOffset Ds = 3
pitchClassToOffset Ef = 3
pitchClassToOffset E  = 4
pitchClassToOffset Es = 5
pitchClassToOffset Ff = 4
pitchClassToOffset F  = 5
pitchClassToOffset Fs = 6
pitchClassToOffset Gf = 6
pitchClassToOffset G  = 7
pitchClassToOffset Gs = 8
pitchClassToOffset Af = 8
pitchClassToOffset A  = 9
pitchClassToOffset As = 10
pitchClassToOffset Bf = 10
pitchClassToOffset B  = 11
pitchClassToOffset Cf = 11

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
articulationToSustenuto Legato   = 127
articulationToSustenuto Marcato  = 64
articulationToSustenuto Staccato = 0

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
genMidiNoteEvents delay channel pitch duration accent =
    [(delay, genMidiNoteOn channel pitch accent), (duration, genMidiNoteOff channel pitch accent)]

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

genMidiTempoMetaEvent :: Tempo -> Event.T
genMidiTempoMetaEvent (Tempo tempo) =
  (Event.MetaEvent . Meta.SetTempo) $ Meta.toTempo tempo
  
genMidiKeySignatureMetaEvent :: KeySignature -> Event.T
genMidiKeySignatureMetaEvent (KeySignature accidentals) =
  (Event.MetaEvent . Meta.KeySig) $ MidiKeySignature.Cons MidiKeySignature.Major (MidiKeySignature.Accidentals accidentals)
  
genMidiSustenutoControlEvent :: ChannelMsg.Channel -> Articulation -> Event.T    
genMidiSustenutoControlEvent chan articulation =
  genEvent chan (VoiceMsg.Control VoiceMsg.sustenuto $ articulationToSustenuto articulation)

controlToEvent :: ChannelMsg.Channel -> Control -> (Duration, Event.T)
controlToEvent channel (DynamicControl dynamic rhythm) =
  (rhythmToDuration rhythm, genMidiDynamicControlEvent channel dynamic)
controlToEvent channel (BalanceControl balance rhythm) =
  (rhythmToDuration rhythm, genMidiBalanceControlEvent channel balance)
controlToEvent channel (TempoControl tempo rhythm) =
  (rhythmToDuration rhythm, genMidiTempoMetaEvent tempo)
controlToEvent channel (KeySignatureControl keySignature rhythm) =
  (rhythmToDuration rhythm, genMidiKeySignatureMetaEvent keySignature)
controlToEvent channel (ArticulationControl articulation rhythm) =
  (rhythmToDuration rhythm, genMidiSustenutoControlEvent channel articulation)
    
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
    noteEventss = map (\notes -> snd $ execState (traverse (midiNoteToEvents channel) notes) (Dur 0, [])) [notes]
    noteEventLists = map (EventList.fromPairList . map durEventToNumEvent) noteEventss
    ctrlEventss = map (map (controlToEvent channel)) controlss
    ctrlEventLists = map (EventList.fromPairList . map durEventToNumEvent) ctrlEventss
    progAndNoteEvents = EventList.cons 0 (genEvent channel (VoiceMsg.ProgramChange instr)) (foldl1 EventList.merge noteEventLists)
    ctrlEvents = foldl1 EventList.merge ctrlEventLists

-- | rhythmToDuration(Rhythm(1%4)) == 512%1 * 1%4 == 512%4 == 128 ticks per quarter note
standardTicks :: MidiFile.Division
standardTicks = MidiFile.Ticks $ fromIntegral $ getDur (rhythmToDuration (Rhythm (1%4))) -- 1:1 for Duration:Tick

-- | Given a MidiVoice, convert instrument, channel, notes, and
--   controls to midi, then assemble MidiFile.T in preparation
--   for Sound.MIDI.File.Save.toByteString.
midiVoiceToMidiFile :: MidiVoice -> MidiFile.T
midiVoiceToMidiFile midiVoice =
  MidiFile.Cons MidiFile.Mixed standardTicks [voiceEventList]
  where
    voiceEventList = midiVoiceToEventList midiVoice

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
noteToMidiNote _ (Rest rhythm) =
  MidiRest rhythm
noteToMidiNote (Instrument instr) (PercussionNote rhythm) =
  MidiNote (GeneralMidi.drumToKey (stringToDrum instr)) rhythm

isMidiPercussion :: String -> Bool
isMidiPercussion = flip elem (map show GeneralMidi.drums)

isMidiInstrument :: String -> Bool
isMidiInstrument = isJust . GeneralMidi.instrumentNameToProgram
  
-- | Select drums channel for non-pitched instruments,
--   channel zero for all others.  Note:  for use by
--   geneator that emits unique file per voice.  To
--   collect voices per file, need way to allocate
--   midi channels for piteched voices.
voiceToMidiVoice :: Voice -> MidiVoice
voiceToMidiVoice (Voice instr notes controlss)
  | isMidiPercussion instrName = MidiVoice instr GeneralMidi.drumChannel midiNotes controlss
  | isMidiInstrument instrName = MidiVoice instr (ChannelMsg.toChannel 0) midiNotes controlss
  | otherwise = error $ "voiceToMidiVoice: instr " ++ instrName ++ " is not a Midi instrument"
  where
    instrName = getInstrument instr
    midiNotes = map (noteToMidiNote instr) notes

-- | Given title, voice, and part number, generate
--   title, convert voice to midi voice, and create
--   midi file for each.
voiceToMidiFile :: Title -> Voice -> Int -> IO ()
voiceToMidiFile title voice@(Voice (Instrument instr) _ _) part =
  LazyByteString.writeFile fileName $ SaveFile.toByteString midiFile
  where
    fileName = title ++ "-" ++ instr ++ "-" ++ show part ++ ".mid"
    midiFile = midiVoiceToMidiFile $ voiceToMidiVoice voice

-- | Given title and list of voices, create midi file per voice.
voicesToMidiFile :: Title -> [Voice] -> IO ()
voicesToMidiFile title voices =
  zipWithM_ (voiceToMidiFile title) voices [1..]

-- | Collect list of voices into list of list
--   of identical voices, then convert all
--   by list to midi file per voice with title
--   e.g. "<title>-<instrument>-1.mid"
scoreToMidiFiles :: Score -> IO ()
scoreToMidiFiles (Score title voices) =
  Control.Monad.mapM_ (voicesToMidiFile title) voicess 
  where
    equalVoiceByInstrument (Voice (Instrument instr1) _ _) (Voice (Instrument instr2) _ _) = instr1 == instr2
    groupVoicesByInstrument = groupBy equalVoiceByInstrument
    orderVoiceByInstrument (Voice (Instrument instr1) _ _) (Voice (Instrument instr2) _ _) = instr1 `compare` instr2
    sortVoicesByInstrument = sortBy orderVoiceByInstrument
    voicess = (groupVoicesByInstrument . sortVoicesByInstrument) voices
