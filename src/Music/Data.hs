{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Music.Data where

import           Data.List
import           Data.Maybe
import           Data.Ratio
import qualified Data.Set      as Set

-- | Pitch classes with two accidentals enharmonic equivalents
data PitchClass = Bs|C|Dff|Bss|Cs|Df|Css|D|Eff|Ds|Ef|Fff|Dss|E|Ff|Es|F|Gff|Ess|Fs|Gf|Fss|G|Aff|Gs|Af|Gss|A|Bff|As|Bf|Cff|Ass|B| Cf deriving (Bounded, Enum, Show, Eq, Ord)

-- | List of list of enharmonically equivalent pitch classes.
enhChromPitchClasses :: [[PitchClass]]
enhChromPitchClasses = [[Bs, C, Dff], [Bss, Cs, Df], [Css, D, Eff], [Ds, Ef, Fff], [Dss, E, Ff], [Es, F, Gff], [Ess, Fs, Gf], [Fss, G, Aff], [Gs, Af], [Gss, A, Bff], [As, Bf, Cff], [Ass, B, Cf]]

-- | Find index in list of enharmonically equivalent pitch classes.
pitchClass2EnhEquivIdx :: PitchClass -> [[PitchClass]] -> Int
pitchClass2EnhEquivIdx pc pcs =
  fromMaybe
    (error $ "pitchClass2Index no match for PitchClass " ++ show pc ++ " in " ++ show pcs)
    (findIndex (elem pc) pcs)

-- | Test pitch classes are enharmonically equivalent.
equivPitchClasses :: PitchClass -> PitchClass -> Bool
equivPitchClasses pc1 pc2 =
  pitchClass2EnhEquivIdx pc1 enhChromPitchClasses == pitchClass2EnhEquivIdx pc2 enhChromPitchClasses

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
      pc1Idx = pitchClass2EnhEquivIdx pc1 enhChromPitchClasses
      pc2Idx = pitchClass2EnhEquivIdx pc2 enhChromPitchClasses    

-- | Indexed pitch requires index into a Scale and Octave
data IndexedPitch = IndexedPitch Int Octave deriving (Eq, Show)

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

-- WIP:  toward fractional dynamic, Int in FractionalDynamic
-- is proportion summed across all elements in list, e.g.
-- [(Piano,1),(MezzoPiano,1)] is 50% Piano, 50% MezzoPiano, and
-- [(Piano,0),(Crescendo,1),(Forte,0),(Decrescendo,3),(Piano,0)]
-- is 25% crescendo from Piano to Forte followed by immediate 75% 
-- decrescendo to Piano.  Stage by first decomposing DynamicControl 
-- Dynamic into DiscreteDynamic value and processing as is and
-- giving an error for FractionalDynamic (done).  Then add error handling
-- to make sure Set VoiceControl doesn't contain multiple instances
-- of DiscreteDynamic or a mix of DiscreteDynamic and FractionalDynamic
-- seeing as Set will admit anything that doesn't answer EQ to compare.
-- Finally, interpret (single) FractionalDynamic by emitting stream
-- of Midi controls within span of single note.

data Dynamic =
  DiscreteDynamic DiscreteDynamicValue
  | FractionalDynamic [(DiscreteDynamicValue, Int)] deriving (Ord, Eq, Show)

-- | Balance (static)
data Balance = LeftBalance | MidLeftBalance | CenterBalance | MidRightBalance | RightBalance deriving (Bounded, Enum, Show, Ord, Eq)

-- | Pan (may be continuous).  Note: enum for discrete control must
--   be LT enum for continuous controls so that Lilypond rendering
--   makes sense.
data Pan =
  Pan { getPan :: Int }
  | PanUp
  | PanDown deriving (Read, Show, Ord, Eq)

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
           
-- | VoiceControls
data VoiceControl =
  DynamicControl Dynamic
  | BalanceControl Balance
  | PanControl Pan
  | ArticulationControl Articulation
  | TextControl String
  | InstrumentControl Instrument
  | AccentControl Accent
  | KeySignatureControl KeySignature
  | TimeSignatureControl TimeSignature
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

-- | An indexed note follows the shape of a note but with
--   an indexed pitch replacing Pitch.
data IndexedNote =
  IndexedNote IndexedPitch Rhythm (Set.Set VoiceControl)
  | IndexedRest Rhythm (Set.Set VoiceControl)
  | IndexedPercussionNote Rhythm (Set.Set VoiceControl) deriving (Eq, Show)

-- | Intervals may be negative or positive and are computed as steps in a Scale
type Interval = Int

-- | List of intervals for a Chord.
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
