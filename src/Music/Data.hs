{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Music.Data where

import           Data.Data
import           Data.List
import           Data.Maybe
import           Data.Ratio
import qualified Data.Set      as Set
import           Data.Word

-- | Pitch classes with two accidentals enharmonic equivalents, ordered in ascending fifths where order is significant.
data PitchClass = Fff | Cff | Gff | Dff | Aff | Eff | Bff | Ff | Cf | Gf | Df | Af | Ef | Bf | F | C | G | D | A | E | B | Fs | Cs | Gs | Ds | As | Es | Bs | Fss | Css | Gss | Dss | Ass | Ess | Bss deriving (Bounded, Enum, Show, Eq, Ord)

-- | Ordering of PitchClass for ordering comparisons, enharmonic equivalent tests.
pitchClassToEnhIdx :: PitchClass -> Integer
pitchClassToEnhIdx Bs  = 0
pitchClassToEnhIdx C   = 0
pitchClassToEnhIdx Dff = 0
pitchClassToEnhIdx Bss = 1
pitchClassToEnhIdx Cs  = 1
pitchClassToEnhIdx Df  = 1
pitchClassToEnhIdx Css = 2
pitchClassToEnhIdx D   = 2
pitchClassToEnhIdx Eff = 2
pitchClassToEnhIdx Ds  = 3
pitchClassToEnhIdx Ef  = 3
pitchClassToEnhIdx Fff = 3
pitchClassToEnhIdx Dss = 4
pitchClassToEnhIdx E   = 4
pitchClassToEnhIdx Ff  = 4
pitchClassToEnhIdx Es  = 5
pitchClassToEnhIdx F   = 5
pitchClassToEnhIdx Gff = 5
pitchClassToEnhIdx Ess = 6
pitchClassToEnhIdx Fs  = 6
pitchClassToEnhIdx Gf  = 6
pitchClassToEnhIdx Fss = 7
pitchClassToEnhIdx G   = 7
pitchClassToEnhIdx Aff = 7
pitchClassToEnhIdx Gs  = 8
pitchClassToEnhIdx Af  = 8
pitchClassToEnhIdx Gss = 9
pitchClassToEnhIdx A   = 9
pitchClassToEnhIdx Bff = 9
pitchClassToEnhIdx As  = 10
pitchClassToEnhIdx Bf  = 10
pitchClassToEnhIdx Cff = 10
pitchClassToEnhIdx Ass = 11
pitchClassToEnhIdx B   = 11
pitchClassToEnhIdx Cf  = 11

-- | Scale is two lists of pitch classes
data Scale =
  Scale { ascendingScale  :: [PitchClass]
        , descendingScale :: [PitchClass]
        } deriving (Eq, Show)

-- | Octave covers signed range where 0 corresponds to span above middle C.
--   Octave is computed from count of items in scale.  Integer range vastly
--   exceeds all instrument ranges, unless guarded by minBound and maxBound.
newtype Octave = Octave { getOctave :: Int } deriving (Eq, Show, Num, Enum, Ord, Data, Typeable)

-- | Octave bounds roughly by piano range
instance Bounded Octave where
    minBound = Octave (-3) --  Piano minimum range, less B..A
    maxBound = Octave 3    --  Piano maximum range

-- | Pitch requires PitchClass and Octave.
data Pitch = Pitch PitchClass Octave deriving (Eq, Show)
instance Ord Pitch where
  (Pitch pc1 (Octave oct1)) `compare` (Pitch pc2 (Octave oct2)) = if oct1 == oct2 then pitchClassToEnhIdx pc1 `compare` pitchClassToEnhIdx pc2 else oct1 `compare` oct2

-- | Indexed pitch requires index into a Scale and Octave
--   Positive values index into ascending scale.
--   Negative values index into descending scale.
data IndexedPitch = IndexedPitch Int Octave deriving (Eq, Show, Data, Typeable)

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
  | Decrescendo deriving (Bounded, Enum, Show, Ord, Eq, Data, Typeable)

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
  | FractionalDynamic [(DiscreteDynamicValue, Int)] deriving (Ord, Eq, Show, Data, Typeable)

-- | Balance (static)
data Balance = LeftBalance | MidLeftBalance | CenterBalance | MidRightBalance | RightBalance deriving (Bounded, Enum, Show, Ord, Eq, Data, Typeable)

-- | Pan (may be continuous).  Note: enum for discrete control must
--   be LT enum for continuous controls so that Lilypond rendering
--   makes sense.
data Pan =
  Pan { getPan :: PanVal }
  | PanUp
  | PanDown deriving (Read, Show, Ord, Eq, Data, Typeable)

newtype PanVal = PanVal { getPanVal :: Int } deriving (Eq, Show, Num, Enum, Ord, Read, Data, Typeable)

-- | Octave bounds roughly by piano range
instance Bounded PanVal where
    minBound = PanVal 0   --  Midi-ism
    maxBound = PanVal 127 --  Midi-ism

-- | Tempo data, unit is e.g. Rhythm (1%4), beats per minute.
--   Ritardando and Accelerando are continuous.
data Tempo =
  Tempo { tempoUnit :: Rhythm, beatsPerMinute :: Integer }
  | Ritardando 
  | Accelerando deriving (Show, Eq, Ord, Data, Typeable)

-- | Key Signature, negative for count flats, positive for count sharps
newtype KeySignature = KeySignature { accidentals :: Int } deriving (Show, Ord, Eq, Data, Typeable)

-- | Time Signature, numerator and denominator
data TimeSignature = TimeSignature { timeSigNum :: Integer , timeSigDenom :: Integer } deriving (Show, Data, Typeable)

timeSignatureToRatio :: TimeSignature -> Rational
timeSignatureToRatio ts = timeSigNum ts % timeSigDenom ts

instance Ord TimeSignature where
  x `compare` y = timeSignatureToRatio x `compare` timeSignatureToRatio y
  
instance Eq TimeSignature where
  (==) x y = timeSignatureToRatio x == timeSignatureToRatio y

-- | Articulation
data Articulation = NoArticulation | Tenuto | Portato | Marcato | Staccato | Staccatissimo deriving (Bounded, Enum, Show, Ord, Eq, Data, Typeable)

-- | Instruments.
newtype Instrument = Instrument { getInstrument :: String } deriving (Show, Ord, Eq, Data, Typeable)

-- | Accent
data Accent = Softest | VerySoft | Soft | Normal | Hard | VeryHard | Hardest deriving (Bounded, Enum, Read, Show, Ord, Eq, Data, Typeable)

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
  deriving (Ord, Eq, Show, Data, Typeable)

data RhythmDenom =
  Whole 
  | Half 
  | Quarter 
  | Eighth 
  | Sixteenth 
  | ThirtySecond 
  | SixtyFourth 
  | OneTwentyEighth
  deriving (Enum, Ord, Eq, Show, Bounded, Data, Typeable)

rhythmDenominatorToInt :: RhythmDenom -> Int
rhythmDenominatorToInt = (2 ^) . fromEnum

data Rhythm' = Rhythm' { rhythmNum :: Word, rhythmDenom :: RhythmDenom } deriving (Show, Ord, Eq, Data, Typeable)

mkRhythm :: Rational -> Either String Rhythm'
mkRhythm r
  | num < (toInteger (minBound::Word)) || num > (toInteger (maxBound::Word)) = Left rngerr
  | not (any (== den) okdens)                                                = Left $ "rhythm denominator not one of " ++ show okdens
  | otherwise                                                                = Right $ Rhythm' (fromIntegral num::Word) (alldens !! denidx)
  where
    rngerr  = "rhythm numerator " ++ show num ++ " < " ++ show (minBound::Word) ++ " or > " ++ show (maxBound::Word)
    alldens = [(minBound::RhythmDenom)..(maxBound::RhythmDenom)]
    denidx  = fromJust $ elemIndex den okdens
    okdens  = [1,2,4,8,16,32,64,128]
    den     = denominator r
    num     = numerator r

-- This approach can produce funny results!  Things you'd think woud fail, like "rhythm (20%5)" turn out to succeed,
-- because Data.Ratio is going to automatically reduce e.g. (20%5) to (4%1)!  So in fact the filtering only catches
-- ratios that don't reduce to a ratio with an invalid denominator, like (29%7).  Does this matter?  If I were to do
-- arithmetic over rhythms then I'd say the answer would be yes, and it's a good thing.  And if you do something silly
-- like specify a rhythm of (21%7), well, then it should be obvious what you're really saying is (3%1).  Note that
-- (1%(-1)) reduces to ((-1)%1), with the result that the Word range check above catches all Data.Ratio constructed 
-- with negative numbers.  And for ((-1)%(-1)) you get (1%1) also, so the arithmetic works out "right".

newtype Rhythm = Rhythm { getRhythm :: Rational } deriving (Show, Ord, Eq, Num, Fractional, Data, Typeable)

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
  | IndexedPercussionNote Rhythm (Set.Set VoiceControl) deriving (Eq, Show, Data, Typeable)

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
