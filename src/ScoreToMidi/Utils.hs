{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ScoreToMidi.Utils where

import           Control.Monad
import           Control.Monad.State
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.EventList.Relative.TimeBody as EventList
import           Data.List
import           Data.Maybe
import           Data.Ratio
--import           Data.Functor
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

newtype Duration = Dur { getDur :: Integer } deriving (Read, Show, Ord, Eq, Num, Enum)

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
dynamicToVolume Pianissimo     = 10
dynamicToVolume Piano          = 30
dynamicToVolume MezzoPiano     = 50
dynamicToVolume MezzoForte     = 80
dynamicToVolume Forte          = 100
dynamicToVolume Fortissimo     = 120
dynamicToVolume Crescendo      = error "dynamicToVolume Crescendo"
dynamicToVolume EndCrescendo   = error "dynamicToVolume EndCrescendo"
dynamicToVolume Decrescendo    = error "dynamicToVolume Decrescendo"
dynamicToVolume EndDecrescendo = error "dynamicToVolume EndDecrescendo"

-- | When decrescendo indicates end of crescendo instead of an explicit
--   dynamic, then what's the next higher dynamic to terminate the
--   crescendo?
incrDynamic :: Dynamic -> Dynamic
incrDynamic Fortissimo = error "incrDynamic Fortissimo is already the max"
incrDynamic dyn = toEnum (fromEnum dyn + 1)::Dynamic

-- | When crescendo indicates end of decrescendo instead of an explicit
--   dynamic, then what's the next lower dynamic to terminate the
--   decrescendo?
decrDynamic :: Dynamic -> Dynamic
decrDynamic Pianissimo = error "decrDynamic Pianissimo is already the min"
decrDynamic dyn = toEnum (fromEnum dyn - 1)::Dynamic

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

-- | Map pitch class and octave to Midi range.
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
  MidiNote VoiceMsg.Pitch Rhythm (Set.Set VoiceControl)
  | MidiRest Rhythm (Set.Set VoiceControl) deriving (Show)

midiNoteToRhythm :: MidiNote -> Rhythm
midiNoteToRhythm (MidiNote _ rhythm _) = rhythm
midiNoteToRhythm (MidiRest rhythm _)   = rhythm

midiNoteToControls :: MidiNote -> Set.Set VoiceControl
midiNoteToControls (MidiNote _ _ controls) = controls
midiNoteToControls (MidiRest _ controls)   = controls
    
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

genMidiVolumeEvent :: ChannelMsg.Channel -> Int -> Event.T
genMidiVolumeEvent chan vol =
  genEvent chan $ VoiceMsg.Control VoiceMsg.mainVolume vol

genMidiDynamicControlEvent :: ChannelMsg.Channel -> Dynamic -> Event.T
genMidiDynamicControlEvent chan dyn = genMidiVolumeEvent chan $ dynamicToVolume dyn
         
genMidiBalanceControlEvent :: ChannelMsg.Channel -> Balance -> Event.T
genMidiBalanceControlEvent chan balance =
  genEvent chan (VoiceMsg.Control VoiceMsg.panorama $ balanceToBalance balance)

genMidiPanControlEvent :: ChannelMsg.Channel -> Pan -> Event.T
genMidiPanControlEvent chan (Pan pan)
  | 0 > pan = error $ "genMidiPanControlEvent Pan value " ++ show pan ++ " is less than minimum 0"
  | 127 < pan = error $ "genMidiPanControlEvent Pan value " ++ show pan ++ " is greater than than maximum 127"
  | otherwise      = genEvent chan (VoiceMsg.Control VoiceMsg.panorama pan)
genMidiPanControlEvent _ PanUp   = error "genMidiPanControl PanUp"
genMidiPanControlEvent _ PanDown = error "genMIdiPanControl PanDown"

-- | Midi Channel Prefix tells system what channel to associate with
--   meta events before an event with a channel appears.  Customary
--   to use channel 1 at the start of a file.
genMidiPrefixMetaEvent :: ChannelMsg.Channel -> Event.T
genMidiPrefixMetaEvent = Event.MetaEvent . Meta.MIDIPrefix

-- | Microseconds per quarter note, default 120 beats per minute is
--   500,000 or defltTempo, so that'd be for a quarter that gets 120.
genMidiTempoMetaEvent :: Tempo -> Event.T
genMidiTempoMetaEvent (Tempo (Rhythm rhythm) beats) =
  (Event.MetaEvent . Meta.SetTempo) $ Meta.toTempo microsPerRhythm
  where
    microsPerMinute   = 60000000
    rhythmsPerQuarter = denominator rhythm % 4
    microsPerRhythm   = floor $ (rhythmsPerQuarter * microsPerMinute) / (beats % 1)
genMidiTempoMetaEvent (Ritardando)  = error "genMIdiTempoEvent Ritardando"
genMidiTempoMetaEvent (Accelerando) = error "genMidiTempoMetaEvent Accelerando"
  
genMidiKeySignatureMetaEvent :: KeySignature -> Event.T
genMidiKeySignatureMetaEvent (KeySignature countAccidentals) =
  (Event.MetaEvent . Meta.KeySig) $ MidiKeySignature.Cons MidiKeySignature.Major (MidiKeySignature.Accidentals countAccidentals)

-- | Numerator is what you'd expect.  Denominator is interpreted
--   0 -> whole, 1 -> half, 2 -> quarter, 3 -> eighth, etc.
genMidiTimeSignatureMetaEvent :: TimeSignature -> Event.T
genMidiTimeSignatureMetaEvent (TimeSignature num denom) =
  Event.MetaEvent $ Meta.TimeSig (fromIntegral num) (fromIntegral denom `div` 2) 0 0 -- metronome, n32notes

genMidiInstrumentControlEvent :: ChannelMsg.Channel -> Instrument -> Event.T    
genMidiInstrumentControlEvent chan (Instrument instrName) =
  genEvent chan (VoiceMsg.ProgramChange instr)
  where
    instr = Data.Maybe.fromJust $ GeneralMidi.instrumentNameToProgram instrName

genMidiTextMetaEvent :: String -> Event.T
genMidiTextMetaEvent  = Event.MetaEvent . Meta.TextEvent

-- | Articulation and Accent are per-note controls that render immediately in the note-on/note-off Midi event stream.
voiceControlToMaybeEvent :: ChannelMsg.Channel -> VoiceControl -> Maybe Event.T
voiceControlToMaybeEvent channel (DynamicControl       dynamic)       = Just $ genMidiDynamicControlEvent    channel dynamic
voiceControlToMaybeEvent channel (BalanceControl       balance)       = Just $ genMidiBalanceControlEvent    channel balance
voiceControlToMaybeEvent channel (PanControl           pan)           = Just $ genMidiPanControlEvent        channel pan
voiceControlToMaybeEvent channel (InstrumentControl    instrument)    = Just $ genMidiInstrumentControlEvent channel instrument
voiceControlToMaybeEvent _       (TextControl          text)          = Just $ genMidiTextMetaEvent          text
voiceControlToMaybeEvent _       (KeySignatureControl  keySignature)  = Just $ genMidiKeySignatureMetaEvent  keySignature
voiceControlToMaybeEvent _       (TimeSignatureControl timeSignature) = Just $ genMidiTimeSignatureMetaEvent timeSignature
voiceControlToMaybeEvent _       (ArticulationControl  _)             = Nothing
voiceControlToMaybeEvent _       (AccentControl        _)             = Nothing

-- | If an AccentControl exists in controls, answer Accent, else answer Normal
lookupAccent :: Set.Set VoiceControl -> Accent
lookupAccent controls =
  fromMaybe
    Normal
    (find (\ accent -> Set.member (AccentControl accent) controls) [(minBound::Accent)..(maxBound::Accent)])

-- | If an ArticulationControl exists in controls, answer Articulation, else answer NoArticulation
lookupArticulation :: Set.Set VoiceControl -> Articulation
lookupArticulation controls =
  fromMaybe
    NoArticulation
    (find (\ articulation -> Set.member (ArticulationControl articulation) controls) [(minBound::Articulation)..(maxBound::Articulation)])

-- | Shorten duration to simulate articulation.
--   Answer pair with new duration and remaining
--   duration to insert as delay before next note.
articulate :: Articulation -> Duration -> (Duration,Duration)
articulate articulation duration =
  case elemIndex articulation articulations of
    Just idx -> (durNote, durRest)
                  where
                    durNote = Dur (floor $ (articulated !! idx) * fromInteger (getDur duration))
                    durRest = duration - durNote
    Nothing -> error $ "articulate unrecognized articulation " ++ show articulation
  where
    articulated  :: [Double]
    articulated   = [1.0,            1.0,    0.9,     0.6,     0.5,      0.25]
    articulations = [NoArticulation, Tenuto, Portato, Marcato, Staccato, Staccatissimo]

-- | A discrete control can be converted to a single unique Midi control event.
discreteControl :: VoiceControl -> Bool
discreteControl (DynamicControl Crescendo)      = False
discreteControl (DynamicControl EndCrescendo)   = False
discreteControl (DynamicControl Decrescendo)    = False
discreteControl (DynamicControl EndDecrescendo) = False
discreteControl (PanControl PanUp)              = False
discreteControl (PanControl PanDown)            = False
discreteControl _                               = True

-- | Create two element list, each with pair containing duration and Sound event
--   with at least first element of delay and note on (e.g. for preceding rest)
--   and second of duration and note off (e.g. for length of note), maybe preceded
--   by list of discrete control events.  Answer pair with first element that is
--   list of duration event pairs that is control events followed by note on and
--   note off events and second element that is left over rest from e.g. articulated
--   note.
genMidiDiscreteEvents :: Duration -> ChannelMsg.Channel -> VoiceMsg.Pitch -> Duration -> Set.Set VoiceControl -> ([(Duration, Event.T)], Duration)
genMidiDiscreteEvents delay channel pitch duration controls
  | minBound > delay    = error $ "genMidiDiscreteEvents delay " ++ show delay ++ " is less than minimum value " ++ show (minBound::Duration)
  | maxBound < delay    = error $ "genMidiDiscreteEvents delay " ++ show delay ++ " is greater than minimum value " ++ show (maxBound::Duration)
  | minBound > duration = error $ "genMidiDiscreteEvents duration " ++ show duration ++ " is less than minimum value " ++ show (minBound::Duration)
  | maxBound < duration = error $ "genMidiDiscreteEvents duration " ++ show duration ++ " is greater than minimum value " ++ show (maxBound::Duration)
  | otherwise           = (zip durations events, restDur)
  where
    accent                = lookupAccent controls
    articulation          = lookupArticulation controls
    noteOnEvent           = genMidiNoteOn  channel pitch accent
    noteOffEvent          = genMidiNoteOff channel pitch accent
    discreteControls      = Set.filter discreteControl controls
    discreteControlEvents = mapMaybe (voiceControlToMaybeEvent channel) $ Set.toAscList discreteControls
    events                = discreteControlEvents ++ [noteOnEvent, noteOffEvent]
    (noteDur, restDur)    = articulate articulation duration
    durations             = [delay] ++ replicate (length discreteControlEvents) (Dur 0) ++ [noteDur]

-- | Emit just control events for a rest.  
genMidiDiscreteControlEvents :: ChannelMsg.Channel -> Set.Set VoiceControl -> [(Duration, Event.T)]
genMidiDiscreteControlEvents channel controls =
  zip [Dur 0..] discreteControlEvents
  where
    discreteControls      = Set.filter discreteControl controls
    discreteControlEvents = mapMaybe (voiceControlToMaybeEvent channel) $ Set.toAscList discreteControls

durEventToNumEvent :: Num a => (Duration, Event.T) -> (a, Event.T)
durEventToNumEvent (Dur dur, event) = (fromInteger dur, event)
--durEventToNumEvent (dur, event) = (fmap (,) fromInteger) dur event

midiNoteToDiscreteEvents :: ChannelMsg.Channel -> MidiNote -> State Duration (EventList.T Event.ElapsedTime Event.T)
midiNoteToDiscreteEvents ch (MidiNote pitch rhythm controls) =
  do rest <- get
     let (durEvents, restDur) = genMidiDiscreteEvents rest ch pitch (rhythmToDuration rhythm) controls
     put restDur
     return $ (EventList.fromPairList . map durEventToNumEvent) durEvents
midiNoteToDiscreteEvents ch (MidiRest rhythm controls) =
  do rest <- get
     put $ rest + rhythmToDuration rhythm
     return $ (EventList.fromPairList . map durEventToNumEvent) (genMidiDiscreteControlEvents ch controls)

equalExplicitDynamic :: VoiceControl -> Bool
equalExplicitDynamic (DynamicControl Pianissimo)     = True 
equalExplicitDynamic (DynamicControl Piano)          = True 
equalExplicitDynamic (DynamicControl MezzoPiano)     = True 
equalExplicitDynamic (DynamicControl MezzoForte)     = True 
equalExplicitDynamic (DynamicControl Forte)          = True 
equalExplicitDynamic (DynamicControl Fortissimo)     = True
equalExplicitDynamic _                               = False

-- | Distribute control val 'd' over count 'c' buckets.
--   Sum of values in each bucket must equal 'd'.  Smaller
--   values come first, larger values come last in a 
--   smooth, stepwise sequence.  Progressing from low to
--   high intensity, as with soft to loud or slow to fast,
--   the rate of change mirrors audibility, as in it's easier
--   to hear the small differences at low intensities.  In
--   the reverse direction, when progressing from high to
--   low intensity, start with faster rate of change.
--   TBD:  accelerate rate of change.
unfoldControl :: (Eq t, Num t, Eq b, Num b) => (t -> b -> t) -> (t,b) -> Maybe (t, (t,b))
unfoldControl divT (d,c)
  | c == 0 = Nothing
  | c == 1 = Just (d, (d, 0))
  | otherwise = Just (x, (d - x, c - 1))
    where
      x = d `divT` c

-- Sum running total with new increment.
-- Used to traverse a list of increments into a running total.
-- TBD:  why isn't this just scanl?
carriedSum :: Num a => a -> State a a
carriedSum i = get >>= \v -> let v' = v + i in put v' >> return v'

-- | Work-around for raw `div` to fit more relaxed constraint for unfoldControl
divInts :: Int -> Int -> Int
divInts x y = x `div` y

-- | Increment Midi dynamic control by increment to answer values to span the range for a duration.
--   Stop values must be equal to Volume translation for start and stop inputs, intermediate
--   values must be consistently increasing (repetitions allowed).   Initial value is equal to
--   start less increment.
synthesizeCrescendoSpan :: Dynamic -> Dynamic -> Duration -> [Int]
synthesizeCrescendoSpan start stop (Dur dur)
  | dur == 0      = error $ "synthesizeCrescendoSpan zero dur for dynamics start " ++ show start ++ " and stop " ++ show stop
  | start >= stop = error $ "synthesizeCrescendoSpan target dynamic " ++ show stop ++ " is not softer than source dynamic " ++ show start
  | otherwise     = evalState (traverse carriedSum increments) (dynamicToVolume start)
  where
    increments = map fromIntegral $ unfoldr (unfoldControl divInts) (dynamicToVolume stop - dynamicToVolume start, fromIntegral dur)

-- | Decrement Midi dynamic control by increment to answer values to span the range for a duration.
--   Stop values must be equal to Volume translation for start and stop inputs, intermediate
--   values must be consistently decreasing (repetitions allowed).   Initial value is equal to
--   start plus increment.
synthesizeDecrescendoSpan :: Dynamic -> Dynamic -> Duration -> [Int]
synthesizeDecrescendoSpan start stop (Dur dur)
  | dur == 0      = error $ "synthesizeDecrescendoSpan zero dur for dynamics start " ++ show start ++ " and stop " ++ show stop
  | stop >= start = error $ "synthesizeDecrescendoSpan target dynamic " ++ show stop ++ " is not softer than source dynamic " ++ show start
  | otherwise     = evalState (traverse carriedSum increments) (dynamicToVolume start)
  where
    increments = map fromIntegral $ unfoldr (unfoldControl divInts) (dynamicToVolume stop - dynamicToVolume start, fromIntegral dur)

-- | Work-around for raw `div` to fit more relaxed constraint for unfoldControl
divIntegers :: Integer -> Integer -> Integer
divIntegers x y = x `div` y

-- | Answer a list of Duration to cover span of synthesized control events
--   given the duration for the continuous control and the count of synthesized
--   control events.  Reverse so larger values, which mean slower perceived 
--   rate of change happen first.
--   Sum of Duration in answer must exactly equal total Duration as second argument.
--   Assumption is durations are for list of controls, one-by-one, so count of
--   controls should never be larger than total duration.
synthesizeDurationSpan :: Int -> Duration -> [Duration]
synthesizeDurationSpan cntCntrls (Dur dur)
  | cntCntrls /= fromIntegral dur = error $ "synthesizeDurationSpan cntCntrls " ++ show cntCntrls ++ " /= dur " ++ show dur
  | otherwise = map Dur $ reverse $ unfoldr (unfoldControl divIntegers) (dur, fromIntegral cntCntrls)

data ControlBufferingState = ControlBufferingNone | ControlBufferingUp | ControlBufferingDown deriving (Bounded, Enum, Show, Ord, Eq)

-- | State when traversing span of continuous controls.
--   First Duration is for accumulating Rest.
--   Second Duration is for accumulating total Span.
data MidiControlContext control = MidiControlContext ControlBufferingState control Duration Duration 
type MidiDynamicControlContext  = MidiControlContext Dynamic  
type MidiPanControlContext      = MidiControlContext Pan
type MidiTempoControlContext    = MidiControlContext Tempo

startDynamicControlContext :: MidiDynamicControlContext
startDynamicControlContext = MidiControlContext ControlBufferingNone MezzoForte (Dur 0) (Dur 0)

genMidiContinuousDynamicEvents :: (Dynamic -> Dynamic -> Duration -> [Int]) -> ChannelMsg.Channel -> Duration -> Duration -> Dynamic -> Dynamic -> EventList.T Event.ElapsedTime Event.T
genMidiContinuousDynamicEvents synth ch rest dur start stop  =
  EventList.fromPairList $ map durEventToNumEvent synthDynamicEventPairs
  where
    -- BUG:  vol span will have one fewer elements than dur span!  And etc. for Pan, Tempo.
    synthVolSpan           = synth start stop dur
    synthDurSpan           = rest : synthesizeDurationSpan (length synthVolSpan) dur -- CRASH:  synth dur span says 4K length synthVolSpan vs. .5 K dur!
    synthDynamicEventPairs = zipWith (\dur' vol -> (dur', genMidiVolumeEvent ch vol)) synthDurSpan synthVolSpan
dynamicFromControl :: VoiceControl -> Dynamic
dynamicFromControl (DynamicControl dynamic) = dynamic
dynamicFromControl control = error $ "accentFromControl expected Dynamic, got " ++ show control

-- | Refactor
bindDynamicControlVars :: MidiNote -> (Dynamic -> Dynamic) -> Dynamic -> (Rhythm, Set.Set VoiceControl, Set.Set VoiceControl, Set.Set VoiceControl, Set.Set VoiceControl, Dynamic, Dynamic)
bindDynamicControlVars midiNote modDynamic dynamic =
  (rhythm, controls, ctrlsDyn, ctrlsCresc, ctrlsDecresc, dynamic', target) 
  where
    rhythm        = midiNoteToRhythm midiNote  
    controls      = midiNoteToControls midiNote  
    ctrlsDyn      = Set.filter equalExplicitDynamic controls
    ctrlsCresc    = Set.filter (== DynamicControl Crescendo) controls
    ctrlsDecresc  = Set.filter (== DynamicControl Decrescendo) controls
    dynamic'      = if Set.null ctrlsDyn then dynamic else dynamicFromControl $ Set.elemAt 0 ctrlsDyn
    target        = if Set.null ctrlsDyn then modDynamic dynamic else dynamicFromControl $ Set.elemAt 0 ctrlsDyn

-- NB:  add to EventList only stream of control messages approximating continous change!
-- Isolated discrete controls are emitted separately, see genMidiDiscreteEvents and genMidiDiscreteControlEvents.
updateDynamicControlContext :: ChannelMsg.Channel -> MidiNote -> MidiDynamicControlContext -> (MidiDynamicControlContext, EventList.T Event.ElapsedTime Event.T)
updateDynamicControlContext _ midiNote (MidiControlContext ControlBufferingNone dynamic rest len)
  | Set.null ctrlsCresc       && Set.null ctrlsDecresc       = (MidiControlContext ControlBufferingNone dynamic' (rest + rhythmToDuration rhythm) len, EventList.empty)
  | not (Set.null ctrlsCresc) && Set.null ctrlsDecresc       = (MidiControlContext ControlBufferingUp   dynamic'  rest (len + rhythmToDuration rhythm), EventList.empty)
  | Set.null ctrlsCresc       && not (Set.null ctrlsDecresc) = (MidiControlContext ControlBufferingDown dynamic'  rest (len + rhythmToDuration rhythm), EventList.empty)
  | otherwise                                                = error $ "updateDynamicControlContext note with both cresc and decresc controls " ++ show controls
  where
    (rhythm, controls, _, ctrlsCresc, ctrlsDecresc, dynamic', _) = bindDynamicControlVars midiNote id dynamic
updateDynamicControlContext chan midiNote (MidiControlContext ControlBufferingUp dynamic rest len) 
  | not (Set.null ctrlsCresc)                              = error $ "updateDynamicControlContext overlapping crescendos for MidiNote " ++ show midiNote
  | not (Set.null ctrlsDyn) || not (Set.null ctrlsDecresc) = (MidiControlContext ControlBufferingNone target (Dur 0) (Dur 0), crescendoEvents)
  | otherwise                                              = (MidiControlContext ControlBufferingUp   dynamic rest (len + rhythmToDuration rhythm), EventList.empty)
  where
    (rhythm, _, ctrlsDyn, ctrlsCresc, ctrlsDecresc, _, target) = bindDynamicControlVars midiNote incrDynamic dynamic
    crescendoEvents                                            = genMidiContinuousDynamicEvents synthesizeCrescendoSpan chan rest len dynamic target
updateDynamicControlContext chan midiNote (MidiControlContext ControlBufferingDown dynamic rest len)
  | not (Set.null ctrlsDecresc)                          = error $ "updateDynamicControlContext overlapping decrescendos for MidiNote " ++ show midiNote
  | not (Set.null ctrlsDyn) || not (Set.null ctrlsCresc) = (MidiControlContext ControlBufferingNone target (Dur 0) (Dur 0), decrescendoEvents)
  | otherwise                                            = (MidiControlContext ControlBufferingDown dynamic rest (len + rhythmToDuration rhythm), EventList.empty)
  where
    (rhythm, _, ctrlsDyn, ctrlsCresc, ctrlsDecresc, _, target) = bindDynamicControlVars midiNote decrDynamic dynamic
    decrescendoEvents                                          = genMidiContinuousDynamicEvents synthesizeDecrescendoSpan chan rest len dynamic target

midiNoteToContinuousDynamicEvents :: ChannelMsg.Channel -> MidiNote -> State MidiDynamicControlContext (EventList.T Event.ElapsedTime Event.T)
midiNoteToContinuousDynamicEvents chan midiNote =
  get >>= \ctrlCtxt -> let (ctrlCtxt', events) = updateDynamicControlContext chan midiNote ctrlCtxt in put ctrlCtxt' >> return events

startPanControlContext :: MidiPanControlContext
startPanControlContext = MidiControlContext ControlBufferingNone (Pan 64) (Dur 0) (Dur 0) 

equalExplicitPan :: VoiceControl -> Bool
equalExplicitPan (PanControl (Pan _)) = True
equalExplicitPan _                    = False     

panFromControl :: VoiceControl -> Pan
panFromControl (PanControl pan) = pan
panFromControl control          = error $ "panFromControl expected Pan, got " ++ show control

getPanVal :: Pan -> Int
getPanVal (Pan val) = val
getPanVal pan       = error $ "getPan called for continuous instance " ++ show pan

synthesizeUpPanSpan :: Pan -> Pan -> Duration -> [Pan]
synthesizeUpPanSpan start stop (Dur dur)
  | dur == 0      = error $ "synthesizeUpPanSpan zero dur for pans start " ++ show start ++ " and stop " ++ show stop
  | start >= stop = error $ "synthesizeUpPanSpan target pan " ++ show stop ++ " is not greater than source pan " ++ show start
  | otherwise     = map Pan $ evalState (traverse carriedSum increments) (getPanVal start)
  where
    increments = unfoldr (unfoldControl divInts) (getPanVal stop - getPanVal start, fromIntegral dur)
synthesizeDownPanSpan :: Pan -> Pan -> Duration -> [Pan]
synthesizeDownPanSpan start stop (Dur dur)
  | dur == 0      = error $ "synthesizeDownPanSpan zero dur for pans start " ++ show start ++ " and stop " ++ show stop
  | stop >= start = error $ "synthesizeDownPanSpan target pan " ++ show stop ++ " is not less than source pan " ++ show start
  | otherwise     = map Pan $ evalState (traverse carriedSum increments) (getPanVal start)
  where
    increments = unfoldr (unfoldControl divInts) (getPanVal stop - getPanVal start, fromIntegral dur)

genMidiContinuousPanEvents :: (Pan -> Pan -> Duration -> [Pan]) -> ChannelMsg.Channel -> Duration -> Duration -> Pan -> Pan -> EventList.T Event.ElapsedTime Event.T
genMidiContinuousPanEvents synth chan rest dur start stop  =
  EventList.fromPairList $ map durEventToNumEvent synthPanEventPairs
  where
    synthPanSpan       = synth start stop dur
    synthDurSpan       = rest : synthesizeDurationSpan (length synthPanSpan) dur
    synthPanEventPairs = zipWith (\dur' pan -> (dur', genMidiPanControlEvent chan pan)) synthDurSpan synthPanSpan

-- | Refactor
bindPanControlVars :: MidiNote -> Pan -> (Rhythm, Set.Set VoiceControl, Set.Set VoiceControl, Set.Set VoiceControl, Set.Set VoiceControl, Pan)
bindPanControlVars midiNote pan =
  (rhythm, controls, ctrlsPan, ctrlsPanUp, ctrlsPanDown, target) 
  where
    rhythm        = midiNoteToRhythm midiNote  
    controls      = midiNoteToControls midiNote  
    ctrlsPan      = Set.filter equalExplicitPan controls
    ctrlsPanUp    = Set.filter (== PanControl PanUp) controls
    ctrlsPanDown  = Set.filter (== PanControl PanDown) controls
    target        = if Set.null ctrlsPan then pan else panFromControl $ Set.elemAt 0 ctrlsPan

-- NB:  add to EventList only stream of control messages approximating continous change.
-- Isolated discrete controls are emitted separately, see genMidiDiscreteEvents and genMidiDiscreteControlEvents.
updatePanControlContext :: ChannelMsg.Channel -> MidiNote -> MidiPanControlContext -> (MidiPanControlContext, EventList.T Event.ElapsedTime Event.T)
updatePanControlContext _ midiNote (MidiControlContext ControlBufferingNone pan rest len)
  | Set.null ctrlsUp       && Set.null ctrlsDown       = (MidiControlContext ControlBufferingNone target (rest + rhythmToDuration rhythm) len, EventList.empty)
  | not (Set.null ctrlsUp) && Set.null ctrlsDown       = (MidiControlContext ControlBufferingUp   pan rest (len + rhythmToDuration rhythm), EventList.empty)
  | Set.null ctrlsUp       && not (Set.null ctrlsDown) = (MidiControlContext ControlBufferingDown pan rest (len + rhythmToDuration rhythm), EventList.empty)
  | otherwise                                          = error $ "updatePanControlContext note with both up and down controls " ++ show controls
  where
    (rhythm, controls, _, ctrlsUp, ctrlsDown, target) = bindPanControlVars midiNote pan
updatePanControlContext chan midiNote (MidiControlContext ControlBufferingUp pan rest len) 
  | not (Set.null ctrlsUp)   = error $ "updatePanControlContext overlapping up for MidiNote " ++ show midiNote
  | not (Set.null ctrlsDown) = error $ "updatePanControlContext overlapping down for MidiNote " ++ show midiNote
  | not (Set.null ctrlsPan)  = (MidiControlContext ControlBufferingNone target (Dur 0) (Dur 0), upEvents)
  | otherwise                = (MidiControlContext ControlBufferingUp   pan    rest (len + rhythmToDuration rhythm), EventList.empty)
  where
    (rhythm, _, ctrlsPan, ctrlsUp, ctrlsDown, target) = bindPanControlVars midiNote pan
    upEvents                                          = genMidiContinuousPanEvents synthesizeUpPanSpan chan rest len pan target
updatePanControlContext chan midiNote (MidiControlContext ControlBufferingDown pan rest len)
  | not (Set.null ctrlsDown) = error $ "updatePanControlContext overlapping down for MidiNote " ++ show midiNote
  | not (Set.null ctrlsUp)   = error $ "updatePanControlContext overlapping up for MidiNote " ++ show midiNote
  | not (Set.null ctrlsPan)  = (MidiControlContext ControlBufferingNone target (Dur 0) (Dur 0), downEvents)
  | otherwise                = (MidiControlContext ControlBufferingDown pan    rest (len + rhythmToDuration rhythm), EventList.empty)
  where
    (rhythm, _, ctrlsPan, ctrlsUp, ctrlsDown, target) = bindPanControlVars midiNote pan
    downEvents                                        = genMidiContinuousPanEvents synthesizeDownPanSpan chan rest len pan target

midiNoteToContinuousPanEvents :: ChannelMsg.Channel -> MidiNote -> State MidiPanControlContext (EventList.T Event.ElapsedTime Event.T)
midiNoteToContinuousPanEvents chan midiNote =
  get >>= \ctrlCtxt -> let (ctrlCtxt', events) = updatePanControlContext chan midiNote ctrlCtxt in put ctrlCtxt' >> return events

-- | Simple type for Num and Ord instances on normalized values.
--   Rational is unit, e.g. (1%2), (1%4) ...
--   Integer is beats per minute.     
data TempoValue = TempoValue Rational Integer deriving (Eq, Show)

-- | Answer normalized first relative to second.
normalizeTempoValue :: TempoValue -> TempoValue -> TempoValue
normalizeTempoValue valOne@(TempoValue rhythmOne bpmOne) (TempoValue rhythmTwo _)
  | numerator rhythmOne == 0 = error $ "normalizeTempoValue div by zero, rhythmOne " ++ show rhythmOne ++ " rhythmTwo " ++ show rhythmTwo
  | rhythmOne <= rhythmTwo = valOne
  | otherwise = TempoValue (rhythmOne * rhythmFact) (bpmOne * numerator bpmFact)
  where
    rhythmFact = rhythmTwo / rhythmOne
    bpmFact    = rhythmOne / rhythmTwo

-- | Normalize first with second, second with first.
normalizeTempoValues :: TempoValue -> TempoValue -> (TempoValue, TempoValue)
normalizeTempoValues tempoOne tempoTwo = (normalizeTempoValue tempoOne tempoTwo, normalizeTempoValue tempoTwo tempoOne)

-- | Normalize values before comparing.
instance Ord TempoValue where
  one `compare` two = bpmOne `compare` bpmTwo where (TempoValue _ bpmOne, TempoValue _ bpmTwo) = normalizeTempoValues one two
                                                    
-- | A little silly for everything but + and -, which are used to generate continuous control span.
instance Num TempoValue where
  t1 + t2                 = TempoValue un (b1n + b2n) where (TempoValue un b1n,TempoValue _ b2n) = normalizeTempoValues t1 t2
  t1 - t2                 = TempoValue un (b1n - b2n) where (TempoValue un b1n,TempoValue _ b2n) = normalizeTempoValues t1 t2
  t1 * t2                 = TempoValue un (b1n * b2n) where (TempoValue un b1n,TempoValue _ b2n) = normalizeTempoValues t1 t2
  abs (TempoValue u b)    = TempoValue u (abs b)
  signum (TempoValue _ b) = TempoValue (1%1) sign where sign = if b > 0 then 1 else (-1)
  fromInteger i           = error $ "fromInteger for TempoValue for integer value " ++ show i -- TempoValue (1%1) i
  negate (TempoValue u b) = TempoValue u (-b)


-- | Unadorned constraint for signature for unfoldControl is Integral t due to use of `div`.
--   But Integral is constrained by (Real a, Enum a), which gets you into a realm of numeric
--   processing too sophisticated for what's needed for a continuous span.
divTempoValueByInt :: TempoValue -> Int -> TempoValue
divTempoValueByInt (TempoValue un bpm) i = TempoValue un (bpm `div` fromIntegral i)

tempoToTempoValue :: Tempo -> TempoValue
tempoToTempoValue (Tempo (Rhythm unit) bpm) = TempoValue unit bpm
tempoToTempoValue tempo = error $ "tempoToTempoValue called for Tempo " ++ show tempo

tempoValueToTempo :: TempoValue -> Tempo
tempoValueToTempo (TempoValue unit bpm) = Tempo (Rhythm unit) bpm

-- | Refactor
bindContinuousTempoValues :: Tempo -> Tempo -> Duration -> (TempoValue, TempoValue, [TempoValue])
bindContinuousTempoValues start stop (Dur dur) =
  (startVal, stopVal, increments)
  where
    startVal   = tempoToTempoValue start
    stopVal    = tempoToTempoValue stop
    increments = unfoldr (unfoldControl divTempoValueByInt) (stopVal - startVal, fromIntegral dur)
  
-- | Convert Tempo to TempoValue for Num instance 
synthesizeAccelerandoSpan :: Tempo -> Tempo -> Duration -> [Tempo]
synthesizeAccelerandoSpan start stop dur
  | dur == Dur 0        = error $ "synthesizeAccelerandoSpan zero dur for accelerando start " ++ show start ++ " and stop " ++ show stop
  | startVal >= stopVal = error $ "synthesizeAccelerandoSpan target tempo " ++ show stop ++ " is not greater than source tempo " ++ show start
  | otherwise           = map tempoValueToTempo $ evalState (traverse carriedSum increments) (tempoToTempoValue start)
  where
    (startVal, stopVal, increments) = bindContinuousTempoValues start stop dur

synthesizeRitardandoSpan :: Tempo -> Tempo -> Duration -> [Tempo]
synthesizeRitardandoSpan start stop dur
  | dur == Dur 0        = error $ "synthesizeRitardandoSpan zero dur for ritardando start " ++ show start ++ " and stop " ++ show stop
  | stopVal >= startVal = error $ "synthesizeRitardandoSpan target tempo " ++ show stop ++ " is not less  than source tempo " ++ show start
  | otherwise           = map tempoValueToTempo $ evalState (traverse carriedSum increments) (tempoToTempoValue start)
  where
    (startVal, stopVal, increments) = bindContinuousTempoValues start stop dur

startTempoControlContext :: MidiTempoControlContext
startTempoControlContext = MidiControlContext ControlBufferingNone (Tempo (Rhythm (1%4)) 120) (Dur 0) (Dur 0)

-- | Refactoring.
updateContTempoControlContext :: (Tempo -> Tempo -> Duration -> [Tempo]) -> (Tempo, Rhythm) -> MidiTempoControlContext -> (MidiTempoControlContext, EventList.T Event.ElapsedTime Event.T)
updateContTempoControlContext synth (stop, rhythm) (MidiControlContext _ start rest len) =
  (MidiControlContext ControlBufferingNone stop (rhythmToDuration rhythm) (Dur 0), synthTempoEventList)
  where
    len'                 = rhythmToDuration rhythm + len
    synthTempoSpan       = synth start stop len'
    synthDurSpan         = rest : synthesizeDurationSpan (length synthTempoSpan) len'
    synthTempoEventPairs = zipWith (\dur' tempo' -> (dur', genMidiTempoMetaEvent tempo')) synthDurSpan synthTempoSpan
    synthTempoEventList  = EventList.fromPairList $ map durEventToNumEvent synthTempoEventPairs

updateTempoControlContext :: (Tempo, Rhythm) -> MidiTempoControlContext -> (MidiTempoControlContext, EventList.T Event.ElapsedTime Event.T)
-- | New discrete tempo, not buffering => remember new tempo and  duration in context,
--   answer list with single event at rest dur from previous event for target tempo.
updateTempoControlContext (target@(Tempo (Rhythm _) _), rhythm) (MidiControlContext ControlBufferingNone _ rest _) =
  (MidiControlContext ControlBufferingNone target (rhythmToDuration rhythm) (Dur 0), EventList.singleton ((fromInteger . getDur) rest) (genMidiTempoMetaEvent target))
-- | New discrete tempo, buffering for accelerando => remember new tempo and length for rest in context,
--   answer list with incremental tempos for accelerando starting at tempo from context ending with target.
updateTempoControlContext (target@(Tempo _ _), rhythm) context@(MidiControlContext ControlBufferingUp _ _ _) =
  updateContTempoControlContext synthesizeAccelerandoSpan (target, rhythm) context
-- | New discrete tempo, buffering for ritardando => remember new tempo and length for rest in context,
--   answer list with incremental tempos for accelerando starting at tempo from context ending with target.
updateTempoControlContext (target@(Tempo _ _), rhythm) context@(MidiControlContext ControlBufferingDown _ _ _) =
  updateContTempoControlContext synthesizeRitardandoSpan (target, rhythm) context
-- | New ritardando, not buffering anything, start new buffering down state with length for duration.
updateTempoControlContext (Ritardando, rhythm) (MidiControlContext ControlBufferingNone tempo rest len) =
  (MidiControlContext ControlBufferingDown tempo rest (rhythmToDuration rhythm + len), EventList.empty)
-- | New accelerando, not buffering anything, start new buffering down state with length for duration.
updateTempoControlContext (Accelerando, dur) (MidiControlContext ControlBufferingNone tempo rest len) =
  (MidiControlContext ControlBufferingUp tempo rest (rhythmToDuration dur + len), EventList.empty)
-- | Error patterns indicate failed sequence of (Tempo, Rhythm), e.g. with successive Accelerando and/or
--   Ritardando without intervening Tempo events or Accelerando or Ritardando without starting Tempo.
updateTempoControlContext (Ritardando, _) (MidiControlContext direction _ _ _) =
  error $ "updateTempoControlContext Ritardando when already buffering " ++ show direction
updateTempoControlContext (Accelerando, _) (MidiControlContext direction _ _ _) =
  error $ "updateTempoControlContext Accelerando when already buffering " ++ show direction
  
-- | State is tuple with rest rhythm, current TempoValue, and single-item buffer with
--   continuous control valuel (if any), default:  Rhythm (0%4), (TempoValue (1%4) 120), Nothing.
--   Answer is Event for current (Tempo, Rhythm) pair.
--   Sequence of (Tempo, Rhythm) pairs requires Tempo values as terminators to continous
--   controls, e.g. [(Tempo, Rhythm), (Accelerando, Rhythm), (Tempo, Rhythm)] where third
--   item tells tempo at end of Accelerando, and the same for Ritardando.  It's an illegal
--   sequence to have back-to-back continuous controls.
--   Consequence:  encountering a continuous control starts a one-element buffer with the
--   current continuous control state (as a Maybe).  Encountering a Tempo means first to
--   check the (Maybe Tempo) isn't empty, and emitting the sequence of incremental Tempo
--   control events for the duration of the previous continuous control.  So the Maybe 
--   has to be for the (Tempo, Rhythm) pair.  I still need to carry the (Rational, Integer)
--   pair (create a local, TempoValue type) so I know where to start the continous control
--   from.
tempoToContinuousTempoEvents :: (Tempo, Rhythm) -> State MidiTempoControlContext (EventList.T Event.ElapsedTime Event.T)
tempoToContinuousTempoEvents pr =
  get >>= \ctrlCtxt -> let (ctrlCtxt', events) = updateTempoControlContext pr ctrlCtxt in put ctrlCtxt' >> return events

-- | Traverse notes accumulating and emiting rests, converting to Midi
--   traverse controls converting to Midi,
--   convert results to single event list merged in time.
midiVoiceToEventList :: MidiVoice -> EventList.T Event.ElapsedTime Event.T
midiVoiceToEventList (MidiVoice (Instrument instrName) channel notes)
  | EventList.empty == noteEvents = EventList.empty
  | otherwise                     = EventList.merge controlEvents progAndNoteEvents 
  where
    instr             = Data.Maybe.fromJust $ GeneralMidi.instrumentNameToProgram instrName
    noteEvents        = EventList.concat $ evalState (traverse (midiNoteToDiscreteEvents channel) notes) (Dur 0)
    contPanEvents     = EventList.concat $ evalState (traverse (midiNoteToContinuousPanEvents channel) notes) startPanControlContext
    contDynEvents     = EventList.concat $ evalState (traverse (midiNoteToContinuousDynamicEvents channel) notes) startDynamicControlContext
    controlEvents     = EventList.merge contPanEvents contDynEvents
    progAndNoteEvents = EventList.cons 0 (genEvent channel (VoiceMsg.ProgramChange instr)) noteEvents

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
  MidiFile.Cons MidiFile.Mixed standardTicks [EventList.merge metaEvents voiceEventList]
  where
    voiceEventList = midiVoiceToEventList midiVoice

-- | Tempo must have 1 as numerator, power of 2 as denominator in range 1/1, 1/2, 1/8, 1/16, 1/32, 1/64
--   At tick == 1/512, that leaves 8 ticks for span of continuous controls for shortest note, still
--   much faster than a listener would detect unless the tempo is extremely slow.
validateTempo :: Tempo -> Tempo
validateTempo tempo@(Tempo (Rhythm rhythm) _)
  | numerator rhythm /= 1                = error $ "validateTempo numerator of rhythm " ++ show rhythm ++ " is not 1"
  | denominator rhythm `notElem` twoPows = error $ "validateTempo denominator of rhythm " ++ show rhythm ++ " not acceptable power of 2 " ++ show twoPows
  | otherwise = tempo
  where
    twoPows = map (2^) [(0::Int)..6]
validateTempo continuousTempo = continuousTempo
    
validateTempoPair :: (Tempo, Rhythm) -> (Tempo, Rhythm)
validateTempoPair (tempo, rhythm) = (validateTempo tempo, rhythm)
    
-- | Generate meta events for start of file.
--   The first track of a Format 1 file is special, and is also known as the 'Tempo Map'.
--   It should contain all meta-events of the types Time Signature, and Set Tempo.
--   The meta-events Sequence/Track Name, Sequence Number, Marker, and SMTPE Offset
--   should also be on the first track of a Format 1 file.
scoreToMetaEvents :: Score -> EventList.T Event.ElapsedTime Event.T
scoreToMetaEvents (Score _ _ (ScoreControls keySignature timeSignature tempos) _) = 
  EventList.merge controlEventList tempoEventList
  where
    tempos'            = map validateTempoPair tempos
    prefixEvent        = (Dur 0, genMidiPrefixMetaEvent (ChannelMsg.toChannel 0))
    keySignatureEvent  = (Dur 0, genMidiKeySignatureMetaEvent keySignature)
    timeSignatureEvent = (Dur 0, genMidiTimeSignatureMetaEvent timeSignature)
    tempoEventList     = EventList.concat $ evalState (traverse tempoToContinuousTempoEvents tempos') startTempoControlContext
    controlEvents      = prefixEvent : keySignatureEvent : [timeSignatureEvent]
    controlEventList   = EventList.fromPairList $ map durEventToNumEvent controlEvents
    
-- | Given title, voice, and part number, generate
--   title, convert voice to create midi file byte
--   string and write file.
scoreAndMidiVoiceToMidiFile :: Score -> MidiVoice -> Int -> IO ()
scoreAndMidiVoiceToMidiFile score voice@(MidiVoice (Instrument instr) _ _) part =
  LazyByteString.writeFile fileName $ SaveFile.toByteString midiFile
  where
    title      = scoreTitle score
    fileName   = title ++ "-" ++ instr ++ "-" ++ show part ++ ".mid"
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
--   BUG:  Obsolete?
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

-- | Refactor:  always called when generating a single file with multiple channels.
bindScoreToVoicessAndChannelss :: Score -> ([Instrument], [[Voice]])
bindScoreToVoicessAndChannelss score =
  (notMidiInstruments, voicess)
  where
    voices             = scoreVoices score
    notMidiInstruments = voicesToNotMidiInstruments voices
    voicess            = collectVoicesByInstrumentWithPercussionLast voices
    
-- | Collect list of voices into list of list
--   of voices with same instrument, then
--   create a parallel list of list of Midi
--   channels for each voices and use the
--   title, list of list of voices, and list
--   of list of channels to create individual
--   Midi files, one per voice, with name e.ag.
--   "<title>-<instrument>-1.mid".
scoreToMidiFiles :: Score -> IO ()
scoreToMidiFiles score 
  | not (null notMidiInstruments) = error $ "convertScore, found non-midi instrument(s) " ++ show notMidiInstruments
  | otherwise                     = zipWithM_ (scoreVoicesAndChannelsToMidiFiles score) voicess channelss
  where
    channelss                     = mapVoicessToPercussionChannelss voicess
    (notMidiInstruments, voicess) = bindScoreToVoicessAndChannelss score

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
  | length voicess /= length channelss = error $ "scoreVoicessAndChannelssToByteString mismatched lengths voicess: " ++ show (length voicess) ++ " channelss: " ++ show (length channelss)
  | null channelss                     = error "scoreVoicessAndChannelssToByteString empty channelss"
  | null midiVoices                    = error "scoreVoicessAndChannelssToByteString empty midiVoicess"
  | null voiceEventLists               = error "scoreVoicessAndChannelssToByteString empty voiceEventLists"
  | null voicess                       = error "scoreVoicessAndChannelssToByteString empty voicess"
  | otherwise                          = SaveFile.toByteString midiFile
  where
    midiVoices      = concat $ (zipWith . zipWith) voiceAndChannelToMidiVoice voicess channelss
    voiceEventLists = map midiVoiceToEventList midiVoices
    metaEvents      = scoreToMetaEvents score 
    midiFile        = MidiFile.Cons MidiFile.Mixed standardTicks [EventList.merge metaEvents (foldl1 EventList.merge voiceEventLists)] 

scoreVoicessAndChannelssToOneMidiFile :: Score -> [[Voice]] -> [[ChannelMsg.Channel]] -> IO ()
scoreVoicessAndChannelssToOneMidiFile score voicess channelss =
  LazyByteString.writeFile fileName byteStream
  where
    title      = scoreTitle score
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
      
-- | Refactor:  convertScore always called when converting to single file.
convertScore :: (Score -> [[Voice]] -> [[ChannelMsg.Channel]] -> a) -> Score -> a
convertScore convert score 
  | countVoices > maxMidiTrack      = error $ "convertScore, count of voices: " ++ show countVoices ++ " exceeds count of Midi channels: " ++ show maxMidiTrack
  | not (null notMidiInstruments)   = error $ "convertScore, found non-midi instrument(s) " ++ show notMidiInstruments
  | otherwise                       = convert score voicess channelss
  where
    countVoices                   = length voicess
    channelss                     = mapVoicessToChannelss voicess
    (notMidiInstruments, voicess) = bindScoreToVoicessAndChannelss score
    
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
