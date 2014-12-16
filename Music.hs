{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Music
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
,  Balance(..)
,  Pan(..)
,  Tempo(..)
,  KeySignature(..)
,  Articulation(..)
,  Accent(..)
,  Rhythm(..)
,  Instrument(..)
,  Control(..)
,  Note(..)
,  transposeNote
,  noteToRhythm
,  NoteMotto
,  transposeNoteMotto
,  Interval
,  Intervals
,  Chord(..)
,  Voice(..)
,  Title
,  Score(..)
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

-- | Cycle extending through a count of one flat or sharp only.
--   Trivially extend through double sharps and flats by adding
--   a second set of fifths on either end, with their pitch
--   classes
cycleOfFifths :: [PitchClass]
cycleOfFifths = [Ff, Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As, Es, Bs]

-- | Answer a subrange, or slice, from an array.
slice :: Int -> Int -> [a] -> [a]
slice from to xs =
  take (to - from + 1) (drop from xs)

-- | Rotate a list, taking element from start
--   and putting it at end, until you reach
--   the target.
rotateTo :: (Ord a, Show a) => a -> [a] -> [a]
rotateTo x xs =
  rotateTo' (elemIndex x xs)
  where
    rotateTo' Nothing = error $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    rotateTo' (Just n) = drop n xs ++ take n xs

-- | Given a pitch class answer the major scale
majorScale :: PitchClass -> Scale
majorScale tonic =
  majorScale' (elemIndex tonic cycleOfFifths)
  where
    majorScale' Nothing = error $ "majorScale tonic " ++ show tonic ++ " not found in cycle of fiths " ++ show cycleOfFifths
    majorScale' (Just idx)
      | idx - lo < 0 || idx + hi >= lenFifths = error $ "majorScale tonic " ++ show tonic  ++ " is out of range " ++ show loFifth ++ " to " ++ show hiFifth
      | otherwise = rotateTo tonic . sort $ slice (idx - lo) (idx + hi) cycleOfFifths
      where
        lo = 1
        hi = 5 
        lenFifths = length cycleOfFifths
        loFifth = cycleOfFifths !! lo
        hiFifth = cycleOfFifths !! (lenFifths - hi - 1)
                            
-- | Given a pitch class answer the natural minor scale
naturalMinorScale :: PitchClass -> Scale
naturalMinorScale tonic =
  naturalMinorScale' (elemIndex tonic cycleOfFifths)
  where
    naturalMinorScale' Nothing = error $ "naturalMinorScale tonic " ++ show tonic ++ " not found in cycle of fiths " ++ show cycleOfFifths
    naturalMinorScale' (Just idx)
      | idx - lo < 0 || idx + hi >= lenFifths = error $ "naturalMinorScale tonic " ++ show tonic  ++ " is out of range " ++ show loFifth ++ " to " ++ show hiFifth
      | otherwise = drop 5 major ++ take 5 major
      where
        major = majorScale $ cycleOfFifths !! (idx - 3)
        lenFifths = length cycleOfFifths
        lo = 4
        hi = 2 
        loFifth = cycleOfFifths !! lo
        hiFifth = cycleOfFifths !! (lenFifths - hi - 1)
    
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

-- | Given a list of pitch classes and a scale,
--   answer the offset to wrap the scale to
--   compute octave offsets given octaves are
--   measured absolutely with respect to the
--   scale that starts and ends on C.  So the
--   D major scale has C as its 6th value
--   starting from 0), so you want to compute
--   the octave offset using an index of 1.
scaleOffset :: [PitchClass] -> Scale -> Int
scaleOffset pitchClasses scale =
  (scaleLen - offset) `mod` scaleLen
  where
    scaleLen = length scale
    matchRoot pitchClass = pitchClass `elem` pitchClasses
    offset = fromJust $ findIndex matchRoot scale

-- | Given a scale, an interval, and a pitch, answer
--   a new pitch interval steps away from the old pitch
transposePitch :: Scale -> Interval -> Pitch -> Pitch                          
transposePitch scale interval (Pitch oldPitchClass (Octave oldOctave)) =
  Pitch (scale !! (target `mod` count)) (Octave (oldOctave + ((target + offset) `div` count)))
  where
    count = length scale
    index = fromJust $ elemIndex oldPitchClass scale
    target = index + interval
    offset = scaleOffset [C, Cs, Df] scale

-- | Given a scale, an interval, and an octave answer 
--   the Pitch "interval" steps frome the first note of
--   "scale" and "octave".
getPitch :: Scale -> Octave -> Interval -> Pitch
getPitch scale octave step =
  transposePitch scale step $ Pitch (head scale) octave
  
-- | A PitchMotto is a list of pitches
type PitchMotto = Motto Pitch

-- | Dynamic
data Dynamic = NoDynamic | Pianissimo | Piano | MezzoPiano | MezzoForte | Forte | Fortissimo deriving (Bounded, Enum, Show, Ord, Eq)

-- | Balance
data Balance = NoBalance | LeftBalance | MidLeftBalance | CenterBalance | MidRightBalance | RightBalance deriving (Bounded, Enum, Show, Ord, Eq)

-- | Pan
newtype Pan = Pan { getPan :: Int } deriving (Read, Show, Ord, Eq, Num)

instance Bounded Pan where
    minBound = 0
    maxBound = 127
    
-- | Tempo, beats per minute
newtype Tempo = Tempo { getTempo :: Int } deriving (Bounded, Enum, Show, Ord, Eq)

-- | Key Signature, negative for count flats, positive for count sharps
newtype KeySignature = KeySignature { accidentals :: Int } deriving (Bounded, Enum, Show, Ord, Eq)

-- | Articulation
data Articulation = NoArticulation | Legato | Marcato | Staccato deriving (Bounded, Enum, Show, Ord, Eq)

-- | Instruments.
newtype Instrument = Instrument { getInstrument :: String } deriving (Show, Ord, Eq)

-- | Controls with their durations as a rhythm.
data Control = DynamicControl Dynamic Rhythm
             | BalanceControl Balance Rhythm
             | PanControl Pan Rhythm
             | TempoControl Tempo Rhythm
             | KeySignatureControl KeySignature Rhythm
             | ArticulationControl Articulation Rhythm
             | InstrumentControl Instrument Rhythm deriving (Ord, Eq, Show)

-- | Accent
data Accent = Softest | VerySoft | Soft | Normal | Hard | VeryHard | Hardest deriving (Bounded, Enum, Read, Show, Ord, Eq)

-- | Rhythm is a ratio, 1:1 for whole note, 2:1 for breve, 1:8 for eighth node, etc.
newtype Rhythm = Rhythm { getRhythm :: Rational } deriving (Show, Ord, Eq)

-- | A note is either an ordinary note with pitch and rhythm,
--   an accented note with pitch, rhythm, and accent,
--   a rest with only a rhythm, a percussion note with
--   a rhtyhm, or an accented percussion note with rhythm
--   and accent.
data Note = Note Pitch Rhythm
          | AccentedNote Pitch Rhythm Accent
          | Rest Rhythm
          | PercussionNote Rhythm
          | AccentedPercussionNote Rhythm Accent deriving (Eq, Show)

-- | Parse rhythm common to all Notes.
noteToRhythm :: Note -> Rhythm
noteToRhythm (Note _ rhythm)           = rhythm
noteToRhythm (AccentedNote _ rhythm _) = rhythm
noteToRhythm (Rest rhythm)             = rhythm

-- | Given a scale, an interval, and a Note,
--   answer the new Note with with transposed Pitch
transposeNote :: Scale -> Interval -> Note -> Note
transposeNote scale interval (Note pitch rhythm) =
  Note (transposePitch scale interval pitch) rhythm
transposeNote scale interval (AccentedNote pitch rhythm accent) =
  AccentedNote (transposePitch scale interval pitch) rhythm accent
transposeNote _ _ (Rest rhythm) =
  Rest rhythm
    
-- | A NoteMotto is a list of Notes
type NoteMotto = Motto Note

-- | Given a scale, an interval, and a NoteMotto, answer
--   a new NoteMotto with all the Pitches transposed 
transposeNoteMotto :: Scale -> Interval -> NoteMotto -> NoteMotto
transposeNoteMotto scale interval noteMotto =
  Motto $ map (transposeNote scale interval) (getMotto noteMotto)
  
-- | Intervals may be negative or positive and are computed as steps in a Scale
type Interval = Int

-- | List of intervals for a Chord.
type Intervals = [Interval]

-- | Data to render a Chord in many different ways.
data Chord = Chord Scale Intervals deriving (Show)

-- | Voice has instrument, list of notes, and
--   list of list of controls organized by control,
--   e.g. one list for sequence of dynamics, tempos, etc.
data Voice = Voice { voiceInstrument :: Instrument
                   , voiceNotes :: [Note]
                   , voiceControls :: [[Control]]
                   } deriving (Show)

-- | Synonym for String
type Title = String

-- | Title and List of voices make up a score (composer and date, too?).
data Score = Score { scoreTitle :: Title
                   , scoreVoices :: [Voice]
                   } deriving (Show)

