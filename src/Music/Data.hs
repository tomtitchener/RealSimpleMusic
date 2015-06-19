{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Music.Data where

import           Data.List
import           Data.Word
import           Data.Maybe
import           Data.Ratio
import qualified Data.Set      as Set

-- | Pitch classes with two accidentals enharmonic equivalents, ordered in ascending fifths where order is significant.
data PitchClass = Fff | Cff | Gff | Dff | Aff | Eff | Bff | Ff | Cf | Gf | Df | Af | Ef | Bf | F | C | G | D | A | E | B | Fs | Cs | Gs | Ds | As | Es | Bs | Fss | Css | Gss | Dss | Ass | Ess | Bss deriving (Bounded, Enum, Show, Eq, Ord)

-- | List of list of enharmonically equivalent pitch classes, in chromatic order.
enhChromPitchClasses :: [[PitchClass]]
enhChromPitchClasses = [[Bs, C, Dff], [Bss, Cs, Df], [Css, D, Eff], [Ds, Ef, Fff], [Dss, E, Ff], [Es, F, Gff], [Ess, Fs, Gf], [Fss, G, Aff], [Gs, Af], [Gss, A, Bff], [As, Bf, Cff], [Ass, B, Cf]]

-- | Find index in list of enharmonically equivalent pitch classes.
pitchClass2EnhEquivIdx :: PitchClass -> Int
pitchClass2EnhEquivIdx pc =
  fromMaybe
    (error $ "pitchClass2Index no match for PitchClass " ++ show pc ++ " in " ++ show enhChromPitchClasses) -- enhChromPitchClasses must include all PitchClass
    (findIndex (elem pc) enhChromPitchClasses)

-- | Test pitch classes are enharmonically equivalent.
equivPitchClasses :: PitchClass -> PitchClass -> Bool
equivPitchClasses pc1 pc2 =
  pitchClass2EnhEquivIdx pc1  == pitchClass2EnhEquivIdx pc2

-- | Scale is two lists of pitch classes
data Scale =
  Scale { ascendingScale  :: [PitchClass]
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

instance Ord Pitch where
  (Pitch pc1 (Octave oct1)) `compare` (Pitch pc2 (Octave oct2)) = if oct1 == oct2 then pc1Idx `compare` pc2Idx else oct1 `compare` oct2
    where
      pc1Idx = pitchClass2EnhEquivIdx pc1
      pc2Idx = pitchClass2EnhEquivIdx pc2

-- | Indexed pitch requires positive only index (into a Scale, provided later) and an Octave
data IndexedPitch = IndexedPitch Word Octave deriving (Eq, Show)

-- | Dynamic (may be continuous).  Note: enum for discrete control must
--   be LT enum for continuous controls so that Lilypond rendering makes
--   sense.
data DiscreteDynamicValue =
  Pianissimo
  | Piano
  | MezzoPiano
  | MezzoForte
  | Forte
  | Fortissimo
  | Crescendo
  | Decrescendo deriving (Bounded, Enum, Show, Ord, Eq)

-- | Special rules for FractionalDynamic pairs:
--   * Int in second half is proportion, relative to
--     sum of all other proportions.  So a list 1,1,1,1
--     turns into 25% of total rhythm per discrete dynamic.
--   * Leading crescendo or decrescendo picks up running
--     dynamic.
--   * Penultimate crescendo or decrescendo must terminate
--     with zero-unit, explicit dynamic (Pianissimo..Fortissimo).
--   * Zero-unit initial dynamic overrides running dynamic to
--     start from a new dynamic.
--   Lilypond lacks encoding of trailing fractional dynamic.
--   You can't indicate a decrescendo or a crescendo to one
--   dynamic and have a new dynamic for the note that follows.
--   For now, the Lilypond conversion drops trailing dynamics.
data Dynamic =
  DiscreteDynamic DiscreteDynamicValue
  | FractionalDynamic [(DiscreteDynamicValue, Int)] deriving (Ord, Eq, Show)

-- | Balance (static)
data Balance = LeftBalance | MidLeftBalance | CenterBalance | MidRightBalance | RightBalance deriving (Bounded, Enum, Show, Ord, Eq)

-- | Pan (may be continuous).  Note: enum for discrete control must
--   be LT enum for continuous controls so that Lilypond rendering
--   makes sense.
data Pan =
  Pan { getPan :: PanVal }
  | PanUp
  | PanDown deriving (Read, Show, Ord, Eq)

newtype PanVal = PanVal { getPanVal :: Int } deriving (Eq, Show, Num, Enum, Ord, Read)

-- | Octave bounds roughly by piano range
instance Bounded PanVal where
    minBound = PanVal 0   --  Midi-ism
    maxBound = PanVal 127 --  Midi-ism

-- | Tempo data, unit is e.g. Rhythm (1%4), beats per minute.
--   Ritardando and Accelerando are continuous.
data Tempo =
  Tempo { tempoUnit :: Rhythm, beatsPerMinute :: Integer }
  | Ritardando 
  | Accelerando deriving (Show, Eq, Ord)

-- | Key Signature, negative for count flats, positive for count sharps
newtype KeySignature = KeySignature { accidentals :: Int } deriving (Show, Ord, Eq)

-- | Time Signature, numerator and denominator
data TimeSignature = TimeSignature { timeSigNum :: Integer , timeSigDenom :: Integer } deriving (Show)

timeSignatureToRatio :: TimeSignature -> Rational
timeSignatureToRatio ts = timeSigNum ts % timeSigDenom ts

instance Ord TimeSignature where
  x `compare` y = timeSignatureToRatio x `compare` timeSignatureToRatio y
  
instance Eq TimeSignature where
  (==) x y = timeSignatureToRatio x == timeSignatureToRatio y

-- | Articulation
data Articulation = NoArticulation | Tenuto | Portato | Marcato | Staccato | Staccatissimo deriving (Bounded, Enum, Show, Ord, Eq)

-- | Instruments.
newtype Instrument = Instrument { getInstrument :: String } deriving (Show, Ord, Eq)

-- | Accent
data Accent = Softest | VerySoft | Soft | Normal | Hard | VeryHard | Hardest deriving (Bounded, Enum, Read, Show, Ord, Eq)

-- | ScoreControls
data ScoreControls =
  ScoreControls {
    scoreKeySignature    :: KeySignature
    , scoreTimeSignature :: TimeSignature
    , scoreTempos        :: [(Tempo, Rhythm)]
    } deriving (Show)
           
-- | VoiceControls:  order dependent!  DynamicControl
--   must be last to satisfy Lilypond parse of fractional
--   dynamic.
data VoiceControl =
  BalanceControl Balance
  | PanControl Pan
  | ArticulationControl Articulation
  | TextControl String
  | InstrumentControl Instrument
  | AccentControl Accent
  | KeySignatureControl KeySignature
  | TimeSignatureControl TimeSignature
  | DynamicControl Dynamic
  deriving (Ord, Eq, Show)
                                  
-- | Rhythm is a ratio, 1:1 for whole note, 2:1 for breve, 1:8 for eighth node, etc.
--   TBD:  limit denominator to meaningful values, 1, 2, 4, 8, 16, 32, 64, 128.
newtype Rhythm = Rhythm { getRhythm :: Rational } deriving (Show, Ord, Eq, Num, Fractional)

-- | A note is either an ordinary note with pitch and rhythm,
--   an accented note with pitch, rhythm, and accent,
--   a rest with only a rhythm, a percussion note with
--   a rhythm, or an accented percussion note with rhythm
--   and accent.
--   TBD:  what value does Set provide?  Equality comparison
--   means VoiceControl DynamicControl Piano /= VoiceControl DynmicControl Forte
--   and I want it that way so I can have Piano and Crescendo together anyway.
--   The only space savings happens if I store DynamicControl Piano and
--   another DynamicControl Piano.
--   Items are stored in order, but I never rely on the ordering for anything,
--   (even though there's a comment in the enum declarations saying I do.)
--   Switch to list?  Set methods used (member, filter, toAscList, null, elemAt,
--   size) all have list equivalents.
--   Replace with List.
data Note =
  Note Pitch Rhythm (Set.Set VoiceControl)
  | Rest Rhythm (Set.Set VoiceControl)
  | PercussionNote Rhythm (Set.Set VoiceControl) deriving (Eq, Show)

-- TBD: remove?
-- | An indexed note follows the shape of a note but with
--   an indexed pitch replacing Pitch.
data IndexedNote =
  IndexedNote IndexedPitch Rhythm (Set.Set VoiceControl)
  | IndexedRest Rhythm (Set.Set VoiceControl)
  | IndexedPercussionNote Rhythm (Set.Set VoiceControl) deriving (Eq, Show)

-- | Intervals may be negative or positive and are computed as steps in a Scale.
type Interval = Int

-- | List of intervals for a Chord or a motive.
type Intervals = [Interval]

-- | Data to render a Chord in many different ways.
data Chord = Chord Scale Intervals deriving (Show)

-- | Voice has instrument, list of notes, and
--   list of list of controls organized by control,
--   e.g. one list for sequence of dynamics, tempos, etc.
data Voice =
  Voice { voiceInstrument :: Instrument
        , voiceNotes :: [Note]
        } deriving (Show)

-- | Synonym for String
type Title = String

-- | Title and List of voices make up a score.
data Score =
  Score { scoreTitle :: Title
        , scoreComposer :: String
        , scoreControls :: ScoreControls
        , scoreVoices   :: [Voice]
        } deriving (Show)
