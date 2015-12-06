-- | Data definitions for real simple music types.

module Music.Data where

import           Data.Data
import           Data.Ratio
import qualified Data.Set      as Set

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

-- | Scale is two lists of pitch classes, pitch class lists most not be empty!
--   Ensure by hiding data type in API.
data Scale =
  Scale { ascendingScale  :: [PitchClass]
        , descendingScale :: [PitchClass]
        } deriving (Eq, Show)

scaleToAscendingPitchClasses :: Scale -> [PitchClass]
scaleToAscendingPitchClasses = ascendingScale

scaleToDescendingPitchClasses :: Scale -> [PitchClass]
scaleToDescendingPitchClasses = descendingScale

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
  | FractionalDynamic [(DiscreteDynamicValue, Word)] deriving (Ord, Eq, Show, Data, Typeable)

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

-- | Pan bounds to Midi vals seeing as there's no simple score equivalent.
--   You could use values to mirror place of instruments in an ensemble.
instance Bounded PanVal where
    minBound = PanVal 0   --  Midi-ism
    maxBound = PanVal 127 --  Midi-ism

-- Hide normal constructor to restrict numerators to positive values, denominators to reasonable powers of two.
-- Maintain inner representation for ease of arithmetic.
newtype Rhythm = Rhythm { rhythmVal :: Rational } deriving (Show, Ord, Eq, Data, Typeable)

-- | Rhythms are easier to manipulate as ratios.
--   But in traditional notation, the unit is a
--   the first handful of powers of 2.  Hide the
--   Rhythm data type and export a constructor 
--   that accepts a ratio but checks for reasonable
--   denominator values.  Note automatic factoring
--   means values that appear illegal (21%14) may 
--   reduce to legal inputs (3%2).
rhythmDenominators :: [Integer]
rhythmDenominators = [1,2,4,8,16,32,64,128]

-- | Create rhythm with restricted values:
--   * numerator must be >= 0
--   * denominator can only be one of [1,2,4,8,16,32,64,128]
mkRhythm :: Rational -> Either String Rhythm
mkRhythm r
  | num < 0                           = Left $ "rhythm numerator " ++ show num ++ " < 0"
  | den `notElem` rhythmDenominators  = Left $ "rhythm denominator not one of " ++ show rhythmDenominators
  | otherwise                         = Right $ Rhythm r
  where
    den     = denominator r
    num     = numerator r

getRhythm :: Rhythm -> Rational
getRhythm = rhythmVal

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

rhythmDenomToInteger :: RhythmDenom -> Integer
rhythmDenomToInteger = ((2::Integer) ^) . fromEnum

data TempoVal = TempoVal { tempoUnit :: RhythmDenom, beatsPerMinute :: Integer } deriving (Show, Eq, Data, Typeable)

-- | Tempo data, unit is e.g. Rhythm (1%4), beats per minute.
--   Ritardando and Accelerando are continuous.
data Tempo =
  Tempo { tempoValue :: TempoVal } 
  | Ritardando 
  | Accelerando deriving (Show, Eq, Data, Typeable)

-- | Answer normalized first relative to second.
--   Purpose is to convert tempo values with different
--   RhythmDenom values into tempo values with the same
--   RhythmDenom values so their beats per minute are
--   directly comparable.  If RhythmDenom for first is
--   smaller than RhythmDenom for second, then answer
--   is first.  Otherwise, scale RhythmDenom and BPM
--   for first to match RhythmDenom of second.
normalizeTempoVal :: TempoVal -> TempoVal -> TempoVal
normalizeTempoVal valOne@(TempoVal denomOne bpmOne) (TempoVal denomTwo _)
  | one >= two = valOne
  | otherwise = TempoVal denomTwo (bpmOne * (two `div` one))
  where
    one  = rhythmDenomToInteger denomOne
    two  = rhythmDenomToInteger denomTwo

-- | Normalize first with second, second with first.
normalizeTempoVals :: TempoVal -> TempoVal -> (TempoVal, TempoVal)
normalizeTempoVals tempoOne tempoTwo = (normalizeTempoVal tempoOne tempoTwo, normalizeTempoVal tempoTwo tempoOne)

-- | Normalize values before comparing.
instance Ord TempoVal where
  one `compare` two = bpmOne `compare` bpmTwo where (TempoVal _ bpmOne, TempoVal _ bpmTwo) = normalizeTempoVals one two
                                                    
-- | A little silly for everything but + and -, which are used to generate continuous control span.
instance Num TempoVal where
  t1 + t2               = TempoVal un (b1n + b2n) where (TempoVal un b1n,TempoVal _ b2n) = normalizeTempoVals t1 t2
  t1 - t2               = TempoVal un (b1n - b2n) where (TempoVal un b1n,TempoVal _ b2n) = normalizeTempoVals t1 t2
  t1 * t2               = TempoVal un (b1n * b2n) where (TempoVal un b1n,TempoVal _ b2n) = normalizeTempoVals t1 t2
  abs (TempoVal u b)    = TempoVal u (abs b)
  signum (TempoVal _ b) = TempoVal Whole sign where sign = if b > 0 then 1 else (-1)
  fromInteger           = TempoVal Quarter 
  negate (TempoVal u b) = TempoVal u (-b)

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

-- | ScoreControls.  KeySignature and TimeSignature are self-explanatory.
--   Tempos is a list of Tempo, Rhythm pairs where Tempo is self-explanatory
--   and Rhythm is duration.  So if you want to start off at one tempo and
--   switch after 30 measures, you have two elements in the array and the
--   rhythm for the first is 30 measures long and the rhythm for the second
--   doesn't matter.  Note Tempo includes discrete and continuous controls,
--   e.g. Ritardando and Accelrando, where the continuous values have to
--   terminate in a discrete destination else you'll get one of the errors
--   from synthesize[Crescendo|Decrescendo]Span.
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
