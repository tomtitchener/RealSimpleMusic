{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Score
(
   PitchClass(..)
,  Motto(..)
,  Scale
,  majorScale
,  naturalMinorScale
,  Octave(..)
,  Pitch(..)
,  transposePitch
,  getPitch
,  PitchMotto
,  Dynamic(..)
,  Accent(..)
,  Rhythm(..)
,  PanDegrees(..)
,  ControlEvent(..)
,  NoteEvent(..)
,  transposeNoteEvent
,  noteEventToRhythm
,  NoteMotto
,  transposeNoteMotto
,  PercussionEvent(..)
,  PercussionMotto
,  Interval
,  Intervals
,  Chord(..)
,  Instrument(..)
,  PercussionInstrument(..)
,  Section(..)
,  PercussionInstrumentEvents(..)
,  PercussionSection(..)
,  Title
)
where

import           Control.Applicative
import           Data.List
import           Data.Maybe

-- | Pitch classes with one accidental only, enharmonic equivalents
data PitchClass = Bs | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F | Fs | Gf | G | Gs | Af | A | As | Bf | B | Cf deriving (Bounded, Enum, Show, Ord, Eq)

-- | Motto is just a list
newtype Motto a = Motto { getMotto :: [a] } deriving (Eq, Show, Functor, Applicative, Monad)

-- | Scale is a list of pitch classes.
type Scale = [PitchClass]

cycleOfFifths :: [PitchClass]
cycleOfFifths = [Ff, Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As, Es, Bs]

subrange :: Int -> Int -> [a] -> [a]
subrange lo hi xs =
  map (xs !!) [lo..hi]

swap :: (a -> Bool) -> [a] -> [a]
swap predicate xs =
  end ++ begin
  where
    (begin, end) = partition predicate xs

-- | Given a pitch class answer the major scale
majorScale :: PitchClass -> Scale
majorScale tonic =
  if idx < 1 || idx > 15
  then
    error $ "majorScale tonic " ++ show tonic  ++ " is out of range"
  else
    swap (< tonic) $ sort $ subrange lo hi cycleOfFifths
  where
    idx = fromJust $ elemIndex tonic cycleOfFifths
    lo = idx - 1
    hi = idx + 5

-- | Given a pitch class answer the natural minor scale
naturalMinorScale :: PitchClass -> Scale
naturalMinorScale tonic =
  if idx < 4 || idx > 18
  then
    error $ "naturalMinorScale tonic " ++ show tonic  ++ " is out of range"
  else
    drop 5 major ++ take 5 major
  where
    idx = fromJust $ elemIndex tonic cycleOfFifths
    major = majorScale $ cycleOfFifths !! (idx - 3)

-- | Octave covers signed range where 0 corresponds to span above middle C.
--   Octave is computed from count of items in scale.  Integer range vastly
--   exceeds all instrument ranges, unless guarded by minBound and maxBound.
newtype Octave = Octave { getOctave :: Int } deriving (Eq, Show, Num)

-- | Octave bounds roughly by piano range
instance Bounded Octave where
    minBound = Octave (-3) --  Piano minimum range, less B..A
    maxBound = Octave 3    --  Piano maximum range

-- | Pitch requires PitchClass and Octave.
data Pitch = Pitch PitchClass Octave deriving (Eq, Show)

-- | Given a scale, an interval, and a pitch,
--   answer the new Pitch "interval" steps away
--   from "pitch" using "scale".
scaleOffset :: Scale -> [PitchClass] -> Scale -> Int
scaleOffset _ pitchClasses relativeScale =
  length relativeScale - offset
  where
    matchRoot pitchClass = pitchClass `elem` pitchClasses
    offset = fromJust $ findIndex matchRoot relativeScale

-- | Given a scale, an interval, and a pitch, answer
--   a new pitch interval steps away from the old pitch
transposePitch :: Scale -> Interval -> Pitch -> Pitch                          
transposePitch scale interval (Pitch oldPitchClass (Octave oldOctave)) =
  Pitch (scale !! (target `mod` count)) (Octave (oldOctave + ((target + offset) `div` count)))
  where
    count = length scale
    index = fromJust $ elemIndex oldPitchClass scale
    target = index + interval
    offset = scaleOffset (majorScale C) [C, Cs, Df] scale

-- | Given a scale, an interval, and an octave answer 
--   the Pitch "interval" steps frome the first note of
--   "scale" and "octave".
getPitch :: Scale -> Octave -> Interval -> Pitch
getPitch scale octave step =
  transposePitch scale step $ Pitch (head scale) octave
  
-- | A PitchMotto is a list of pitches
type PitchMotto = Motto Pitch

-- | Dynamic
data Dynamic = Pianissimo | Piano | MezzoPiano | MezzoForte | Forte | Fortissimo deriving (Bounded, Enum, Show, Ord, Eq)

-- | Accent
data Accent = Softest | VerySoft | Soft | Normal | Hard | VeryHard | Hardest deriving (Bounded, Enum, Read, Show, Ord, Eq)

-- | Rhythm is just a ratio, 1:1 for whole note, 2:1 for breve, 1:8 for eighth node, etc.
newtype Rhythm = Rhythm { getRhythm :: Rational } deriving (Eq, Show)

-- | PanDegrees is integer from 0 to 359
newtype PanDegrees = PanDegrees { getPanDegrees :: Integer } deriving (Show, Ord, Eq, Num)

instance Bounded PanDegrees where
    minBound = PanDegrees 0    --  Mimic circle, maybe replace with -90?
    maxBound = PanDegrees 359  --  Mimic circle, maybe replace with +90?

-- | Control events map to Midi controllers, currently limited to Dynamic and Pan.
data ControlEvent = DynamicControl Dynamic Rhythm | PanControl PanDegrees Rhythm deriving (Show)

-- | A note is either a note with the default velocity, a note with a particular velocity, or a rest.
data NoteEvent = Note Pitch Rhythm | AccentedNote Pitch Rhythm Accent | Rest Rhythm deriving (Eq, Show)

-- | Fetch rhythm common to all NoteEvent
noteEventToRhythm :: NoteEvent -> Rhythm
noteEventToRhythm (Note _ rhythm)           = rhythm
noteEventToRhythm (AccentedNote _ rhythm _) = rhythm
noteEventToRhythm (Rest rhythm)             = rhythm

-- | Given a scale, an interval, and a NoteEvent,
--   answer the new NoteEvent with with transposed Pitch
transposeNoteEvent :: Scale -> Interval -> NoteEvent -> NoteEvent
transposeNoteEvent scale interval (Note pitch rhythm) =
  Note (transposePitch scale interval pitch) rhythm
transposeNoteEvent scale interval (AccentedNote pitch rhythm accent) =
  AccentedNote (transposePitch scale interval pitch) rhythm accent
transposeNoteEvent _ _ (Rest rhythm) =
  Rest rhythm
    
-- | A NoteMotto is a list of NoteEvents
type NoteMotto = Motto NoteEvent

-- | Given a scale, an interval, and a NoteMotto, answer
--   a new NoteMotto with all the Pitches transposed 
transposeNoteMotto :: Scale -> Interval -> NoteMotto -> NoteMotto
transposeNoteMotto scale interval noteMotto =
  Motto $ map (transposeNoteEvent scale interval) (getMotto noteMotto)
-- | A percussion event either a note, an accented note, or a rest.
data PercussionEvent = PercussionNote Rhythm | AccentedPercussionNote Rhythm Accent | PercussionRest Rhythm deriving (Eq, Show)

-- | A PercussionMotto is a list of PercussionEvents
type PercussionMotto = Motto PercussionEvent

-- | Intervals may be negative or positive and are computed as steps in a Scale
type Interval = Int

-- | List of intervals for a Chord.
type Intervals = [Interval]

-- | Data to render a Chord in many different ways.
data Chord = Chord Scale Intervals deriving (Show)

-- | Wrapped string
newtype Instrument = Instrument { getInstrument :: String } deriving (Show)

-- | Multiple NoteEvent, ControlEvent, each on an independent timeline, e.g. for a group of identical instruments.
--   For a solo voice, create just one element in the [[NoteEvent]] array.  Multiple ControlEvent
--   lists for convenience of separate tracks by Control.
--   This is really a Midi Track, which is restricted to one instrument--that can maybe play more
--   than one note at a time--with all controls acting on all pitches.
--   It's really more like a section, say the flute or French horn or soprano voice section,
--   where everybody plays the same instrument.
--   It's a way to squeeze more than 15 independent voices into a 15-track format.
data Section = Section Instrument [[NoteEvent]] [[ControlEvent]] deriving (Show)

-- | Wrapped string
newtype PercussionInstrument = PercussionInstrument { getPercussionInstrument :: String } deriving (Show)

-- | Events for one percussion instrument
data PercussionInstrumentEvents = PercussionInstrumentEvents PercussionInstrument [PercussionEvent] deriving (Show)

-- | Each PercussionInstrumentEvents identifies a percussion instrument, so the PercussionSection
--   is really more like a choir, with a range of instruments.
--   Midi-ism: multiple percussion instruments and controls all on the same channel.
data PercussionSection = PercussionSection [PercussionInstrumentEvents] [[ControlEvent]] deriving (Show)

-- | Synonym for String
type Title = String
