{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{--
  Conversion routines:

    midiScoreToMidiFile :: [Char] -> [Voices] -> MidiFile.T
    midiScoreToMidiFile title voices

    midiPercussionscoreToMidiFile :: [Char] -> [Voices] -> Percussion -> MidiFile.T
    midiPercussionScoreToMidiFile title voices percussion

  Converts an instance of Score to a MidiFile, which then gets written out thus:

  main :: IO ()
  main =
    do
      score <- makeSong
      LazyByteString.writeFile "test.mid" (SaveFile.toByteString (scoreToMidiFile score))
--}

module ScoreToMidi(
   Score(..)
,  midiScore
,  midiVoicesScore
,  midiPercussionScore
,  scoreToMidiFile
,  midiInstrument
,  midiPercussionInstrument
) where

import           Data.List
import           Data.Maybe
import           Data.Ratio
import           Score
import           Control.Monad.State
import qualified Data.EventList.Relative.TimeBody as EventList
import           Data.Traversable
import qualified Sound.MIDI.File                  as MidiFile
import qualified Sound.MIDI.File.Event            as Event
import qualified Sound.MIDI.General               as GeneralMidi
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

newtype Duration = Dur { getDur :: Integer } deriving (Read, Show, Ord, Eq, Num)

-- | Midi-ism:  bounds for Duration are from Midi
instance Bounded Duration where
    minBound = Dur 0
    maxBound = Dur 0x0FFFFFFF

-- | Midi-ism:  Duration is in 64th notes, at default of 128 ticks per quarter or 512 ticks per whole note translation is (* 8 * 64)
--   e.g. 1;1 for whole note * 64 * 8 => 512 ticks.  NB: conversion to Midi doesn't accept ratios that don't divide evenly into 512.
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

pitchToMidi :: Pitch -> Int
pitchToMidi (Pitch pitchClass oct) =
  let
    pitchOffset = pitchClassToOffset pitchClass
    octaveOffset = getOctave $ oct * 12
    midiOffset = 60
  in
    pitchOffset + octaveOffset + midiOffset

genEvent :: ChannelMsg.Channel -> VoiceMsg.T -> Event.T
genEvent chan = Event.MIDIEvent . ChannelMsg.Cons chan . ChannelMsg.Voice

genMidiNoteOn :: ChannelMsg.Channel -> Pitch -> Accent -> Event.T
genMidiNoteOn channel pitch accent =
    genEvent channel (VoiceMsg.NoteOn (VoiceMsg.toPitch (pitchToMidi pitch)) (VoiceMsg.toVelocity (accentToVelocity accent)))

genMidiNoteOff :: ChannelMsg.Channel -> Pitch  -> Event.T
genMidiNoteOff channel pitch =
    genEvent channel (VoiceMsg.NoteOff (VoiceMsg.toPitch (pitchToMidi pitch)) VoiceMsg.normalVelocity)

genMidiNoteEvents :: Duration -> ChannelMsg.Channel -> Pitch -> Duration -> Accent -> [(Duration, Event.T)]
genMidiNoteEvents delay channel pitch duration accent =
    [(delay, genMidiNoteOn channel pitch accent), (duration, genMidiNoteOff channel pitch)]

rhythmToEvents :: Rhythm -> State (Duration, [(Duration, Event.T)]) Duration
rhythmToEvents rhythm =
    do (rest, events) <- get
       put (dur + rest, events)
       return $ dur + rest
       where
         dur = rhythmToDuration rhythm
         
noteToEvents :: ChannelMsg.Channel -> NoteEvent -> State (Duration, [(Duration, Event.T)]) Duration
noteToEvents ch (AccentedNote pitch rhythm accent) =
    do (rest, events) <- get
       put (Dur 0, events ++ genMidiNoteEvents rest ch pitch (rhythmToDuration rhythm) accent)
       return (Dur 0)
noteToEvents ch (Note pitch rhythm) =
    do (rest, events) <- get
       put (Dur 0, events ++ genMidiNoteEvents rest ch pitch (rhythmToDuration rhythm) Normal)
       return (Dur 0)
noteToEvents _ (Rest rhythm) = rhythmToEvents rhythm
         
genMidiDynamicCtrlEvent :: ChannelMsg.Channel -> Dynamic -> Event.T
genMidiDynamicCtrlEvent chan dynamic =
    genEvent chan (VoiceMsg.Control VoiceMsg.mainVolume $ dynamicToVolume dynamic)
    
genMidiPanCtrlEvent :: ChannelMsg.Channel -> PanDegrees -> Event.T
genMidiPanCtrlEvent chan (PanDegrees degrees) =
    genEvent chan (VoiceMsg.Control VoiceMsg.panorama (floor ((127 / 360) * fromInteger degrees)))
    
controlToEvent :: ChannelMsg.Channel -> ControlEvent -> (Duration, Event.T)
controlToEvent channel (DynamicControl dynamic rhythm) =
    (rhythmToDuration rhythm, genMidiDynamicCtrlEvent channel dynamic)
controlToEvent channel (PanControl degrees rhythm) =
    (rhythmToDuration rhythm, genMidiPanCtrlEvent channel degrees)

genPercussionMidiNoteOn :: ChannelMsg.Channel -> VoiceMsg.Pitch -> Accent -> Event.T
genPercussionMidiNoteOn channel pitch accent =
    genEvent channel (VoiceMsg.NoteOn pitch (VoiceMsg.toVelocity (accentToVelocity accent)))

genPercussionMidiNoteOff :: ChannelMsg.Channel -> VoiceMsg.Pitch -> Accent -> Event.T
genPercussionMidiNoteOff channel pitch _ =
    genEvent channel (VoiceMsg.NoteOff pitch VoiceMsg.normalVelocity)

genPercussionMidiNoteEvents :: Duration -> ChannelMsg.Channel -> VoiceMsg.Pitch -> Duration -> Accent -> [(Duration, Event.T)]
genPercussionMidiNoteEvents delay channel pitch duration accent =
    [(delay, genPercussionMidiNoteOn channel pitch accent), (duration, genPercussionMidiNoteOff channel pitch accent)]

percussionToEvents :: ChannelMsg.Channel -> VoiceMsg.Pitch -> PercussionEvent -> State (Duration, [(Duration, Event.T)]) Duration
-- or, cribbed:
-- percussionToEvents :: MonadState (Duration, [(Duration, Event.T)]) m => ChannelMsg.Channel -> VoiceMsg.Pitch -> PercussionEvent -> m Duration
percussionToEvents chan pitch (AccentedPercussionNote rhythm accent) =
    do (rest,events) <- get
       put (Dur 0, events ++ genPercussionMidiNoteEvents rest chan pitch (rhythmToDuration rhythm) accent)
       return (Dur 0)
percussionToEvents chan pitch (PercussionNote rhythm) =
    do (rest,events) <- get
       put (Dur 0, events ++ genPercussionMidiNoteEvents rest chan pitch (rhythmToDuration rhythm) Normal)
       return (Dur 0)
percussionToEvents _ _ (PercussionRest rhythm) = rhythmToEvents rhythm

stringToDrum :: String -> GeneralMidi.Drum
stringToDrum instr = GeneralMidi.drums !! Data.Maybe.fromJust (Data.List.elemIndex instr (map show GeneralMidi.drums))

percussionInstrumentEventsToEvents :: ChannelMsg.Channel -> PercussionInstrumentEvents -> [(Duration, Event.T)]
percussionInstrumentEventsToEvents channel (PercussionInstrumentEvents (PercussionInstrument name) percussionEvents) =
  let pitch = GeneralMidi.drumToKey $ stringToDrum name
  in
      snd $ execState (traverse (percussionToEvents channel pitch) percussionEvents) (Dur 0, [])

durEventToNumEvent :: Num a => (Duration, Event.T) -> (a, Event.T)
durEventToNumEvent (Dur dur, event) = (fromInteger dur, event)
--durEventToNumEvent (dur, event) = (fmap (,) durationFromInteger) dur event

sectionToEventList :: ChannelMsg.Channel -> Section -> EventList.T Event.ElapsedTime Event.T
sectionToEventList channel (Section (Instrument name) notess controlss)
  | null noteEventLists = EventList.empty
  | null ctrlEventLists = EventList.cons 0 (genEvent channel (VoiceMsg.ProgramChange instr)) (foldl1 EventList.merge noteEventLists)
  | otherwise = EventList.merge
                    (EventList.cons 0 (genEvent channel (VoiceMsg.ProgramChange instr)) (foldl1 EventList.merge noteEventLists))
                    (foldl1 EventList.merge ctrlEventLists)
  where
    instr = Data.Maybe.fromJust $ GeneralMidi.instrumentNameToProgram name
    noteEventss = map (\notes -> snd $ execState (traverse (noteToEvents channel) notes) (Dur 0, [])) notess
    noteEventLists = map (EventList.fromPairList . map durEventToNumEvent) noteEventss
    ctrlEventss = map (map (controlToEvent channel)) controlss
    ctrlEventLists = map (EventList.fromPairList . map durEventToNumEvent) ctrlEventss

percussionSectionToEventList :: ChannelMsg.Channel -> PercussionSection -> EventList.T Event.ElapsedTime Event.T
percussionSectionToEventList channel (PercussionSection percussionInstrumentEventss controlss)
  | null percEventLists = EventList.empty
  | null ctrlEventLists = foldl1 EventList.merge percEventLists
  | otherwise = EventList.merge (foldl1 EventList.merge percEventLists) (foldl1 EventList.merge ctrlEventLists)
  where
    percEventss = map (percussionInstrumentEventsToEvents channel) percussionInstrumentEventss
    percEventLists = map (EventList.fromPairList . map durEventToNumEvent) percEventss
    ctrlEventss = map (map (controlToEvent channel)) controlss
    ctrlEventLists = map (EventList.fromPairList . map durEventToNumEvent) ctrlEventss

-- | Score, some versions of which can be rendered to Midi.
--   Midi constraints are by channel, e.g. 1 percussion and
--   15 non-percussion.  If it's possible to override percussion
--   with a program change, then there's a special constructor
--   for 16 non-percussion voices.  When ControlEvents includes
--   ProgramChange, then instrument associated with a channel
--   can change.  Otherwise, limit is 16 unique instruments or
--   15 unique instruments plus percussion, where percussion
--   instruments can range on the one channel, thanks to Midi
--   overloading of Midi pitch with percussion instrument.
--
--   However, the way this gets commonly used is to emit a
--   Midi file with a single track, which are then added
--   individually to e.g. Logic or Garage Band, both of which
--   transparently handle more than 16 Midi tracks.
--
--  TBD:  for a score with more than 16 unique instruments, consider
--  a conversion routine that segments voices to separate output files,
--  which can then be combined in a Midi editor like Logic.  Need to
--  disambiguate ordinary Score from MidiScore in that case, maybe by
--  a special type for different counts of [Section]?
--
--  TBD:  add meta events for composer, date, etc.
--
data Score = {-- Score Title [Section] | --} MidiScore Title [Section] PercussionSection | MidiVoicesScore Title [Section] | MidiPercussionScore Title PercussionSection deriving (Show)

-- | Create a Score instance for Midi with a percussion track.  That allows 15 non-percussion
--   instruments and a percussion track with as many percussion instruments as you want.
midiScore :: Title -> [Section] -> PercussionSection -> Score
midiScore t vs pv
    | length vs <= 15 = MidiScore t vs pv
    | otherwise = error $ "midiScore constructor called with count of voices " ++ show (length vs) ++ " > max value 15"

-- | Create a Score instance for Midi with no percussion track.  That allows 16 non-percussion
--   instruments
midiVoicesScore :: Title -> [Section] -> Score
midiVoicesScore t vs
    | length vs <= 16 = MidiVoicesScore t vs
    | otherwise = error $ "midiVoicesScore constructor called with count of voices " ++ show (length vs) ++ " > max value 16"

-- | Create a Score instance for Midi with just a percussion track.  This allows as many
--   percussion instruments as you want.
midiPercussionScore :: Title -> PercussionSection -> Score
midiPercussionScore = MidiPercussionScore

-- | Private
instrumentIsMidi :: String -> Bool
instrumentIsMidi = isJust . GeneralMidi.instrumentNameToProgram

-- | Constructor limited to Midi instrument strings.
midiInstrument :: String -> Instrument
midiInstrument name
  | instrumentIsMidi name = Instrument name
  | otherwise = error $ "name " ++ name ++ " is not one of Midi instruments: " ++ show GeneralMidi.instrumentNames

-- | private
percussionIsMidi :: String -> Bool
percussionIsMidi = flip elem (map show GeneralMidi.drums)

-- | Constructor limited to Midi percussion strings.
--
--   List of GeneralMidi drums:  AcousticBassDrum, BassDrum1, SideStick, AcousticSnare, HandClap, ElectricSnare, LowFloorTom, ClosedHiHat, HighFloorTom, PedalHiHat,
--   LowTom, OpenHiHat, LowMidTom, HiMidTom, CrashCymbal1, HighTom, RideCymbal1, ChineseCymbal, RideBell, Tambourine, SplashCymbal, Cowbell, CrashCymbal2, Vibraslap,
--   RideCymbal2, HiBongo, LowBongo, MuteHiConga, OpenHiConga, LowConga, HighTimbale, LowTimbale, HighAgogo, LowAgogo, Cabasa, Maracas, ShortWhistle, LongWhistle,
--   ShortGuiro, LongGuiro, Claves, HiWoodBlock, LowWoodBlock, MuteCuica, OpenCuica, MuteTriangle, OpenTriangle
--
midiPercussionInstrument :: String -> PercussionInstrument
midiPercussionInstrument name
  | percussionIsMidi name = PercussionInstrument name
  | otherwise = error $ "name " ++ name ++ " is not one of Midi drums: " ++  show (map show GeneralMidi.drums)

-- | Public API:
--   Convert title and list of voices to a Midi file or title, list of voices, and percussion voice to a Midi file.  TBD: meta event for Title.
--
-- @
--   main :: IO ()
--   main =
--    do
--      score <- makeSong
--      LazyByteString.writeFile "test.mid" (SaveFile.toByteString (scoreToMidiFile score))
-- @
--
scoreToMidiFile :: Score -> MidiFile.T
--scoreToMidiFile (MidiScore title voices) =
--    undefined

standardTicks :: MidiFile.Division
standardTicks = MidiFile.Ticks $ fromIntegral $ getDur (rhythmToDuration (Rhythm (1%4))) -- 1:1 for Duration:Tick

scoreToMidiFile (MidiScore _ voices percussion) =
    let
        chans = map ChannelMsg.toChannel [0 .. (length voices - 1)]
        voiceEventLists = zipWith sectionToEventList chans voices
        percussionEventList = percussionSectionToEventList GeneralMidi.drumChannel percussion
    in
        MidiFile.Cons MidiFile.Mixed standardTicks [foldl1 EventList.merge (voiceEventLists ++ [percussionEventList])]

scoreToMidiFile (MidiVoicesScore _ voices) =
    let
        chans = map ChannelMsg.toChannel [0 .. (length voices - 1)]
        voiceEventLists = zipWith sectionToEventList chans voices
    in
        MidiFile.Cons MidiFile.Mixed standardTicks [foldl1 EventList.merge voiceEventLists]

scoreToMidiFile (MidiPercussionScore _ percussion) =
    let
        percussionEventList = percussionSectionToEventList GeneralMidi.drumChannel percussion
    in
        MidiFile.Cons MidiFile.Mixed standardTicks [foldl1 EventList.merge [percussionEventList]]

{--
  main :: IO ()
  main =
    do
      percussionInstrumentEvents <- sample' $ suchThat (arbitrary::Gen PercussionInstrumentEvents)  ((((durationFromRhythm Quarter) * 4) ==) . percussionInstrumentEventsDuration)
      percussionSection <- return $ PercussionSection percussionInstrumentEvents []
      score <- return $ MidiPercussionScore "test" percussionSection
      LazyByteString.writeFile "test.mid" (SaveFile.toByteString (scoreToMidiFile score))
--}
