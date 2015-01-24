{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Music.Data where

import Control.Applicative

-- | Pitch classes with two accidentals enharmonic equivalents
data PitchClass = Bs|C|Dff|Bss|Cs|Df|Css|D|Eff|Ds|Ef|Fff|Dss|E|Ff|Es|F|Gff|Ess|Fs|Gf|Fss|G|Aff|Gs|Af|Gss|A|Bff|As|Bf|Cff|Ass|B| Cf deriving (Bounded, Enum, Show, Ord, Eq) 

-- | Motto is just a list
--   TBD: is there any value in this?
newtype Motto a = Motto { getMotto :: [a] } deriving (Eq, Show, Functor, Applicative, Monad)

-- | Scale is two lists of pitch classes
data Scale = Scale { ascendingScale  :: [PitchClass]
                   , descendingScale :: [PitchClass]
                   } deriving (Eq, Show)

-- | Octave covers signed range where 0 corresponds to span above middle C.
--   Octave is computed from count of items in scale.  Integer range vastly
--   exceeds all instrument ranges, unless guarded by minBound and maxBound.
newtype Octave = Octave { getOctave :: Int } deriving (Eq, Show, Num, Enum)

-- | Octave bounds roughly by piano range
instance Bounded Octave where
    minBound = Octave (-3) --  Piano minimum range, less B..A
    maxBound = Octave 3    --  Piano maximum range

-- | Pitch requires PitchClass and Octave.
data Pitch = Pitch PitchClass Octave deriving (Eq, Show)

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
             | TextControl String Rhythm
             | InstrumentControl Instrument Rhythm deriving (Ord, Eq, Show)

-- | Accent
data Accent = Softest | VerySoft | Soft | Normal | Hard | VeryHard | Hardest deriving (Bounded, Enum, Read, Show, Ord, Eq)

-- | Rhythm is a ratio, 1:1 for whole note, 2:1 for breve, 1:8 for eighth node, etc.
newtype Rhythm = Rhythm { getRhythm :: Rational } deriving (Show, Ord, Eq, Num)

-- | A note is either an ordinary note with pitch and rhythm,
--   an accented note with pitch, rhythm, and accent,
--   a rest with only a rhythm, a percussion note with
--   a rhythm, or an accented percussion note with rhythm
--   and accent.
data Note = Note Pitch Rhythm
          | AccentedNote Pitch Rhythm Accent
          | Rest Rhythm
          | PercussionNote Rhythm
          | AccentedPercussionNote Rhythm Accent deriving (Eq, Show)

-- | A NoteMotto is a list of Notes
type NoteMotto = Motto Note

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
