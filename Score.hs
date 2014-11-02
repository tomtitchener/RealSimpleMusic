{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Data definitions for real simple music types.

module Score
(
   PitchClass(..)
,  Motto(..)
,  Scale
,  Octave(..)
,  Pitch(..)
,  PitchMotto
,  Dynamic(..)
,  Accent(..)
,  Rhythm(..)
,  PanDegrees(..)
,  ControlEvent(..)
,  NoteEvent(..)
,  NoteMotto
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
import           Data.Maybe()
import           Data.Monoid
import           Data.Ratio()

-- | Pitch classes with one accidental only, enharmonic equivalents
data PitchClass = Bs | C | Cs | Df | D | Ds | Ef | E | Es | Ff | F | Fs | Gf | G | Gs | Af | A | As | Bf | B | Cf deriving (Bounded, Enum, Show, Ord, Eq)

normPitchClass :: PitchClass -> Integer
normPitchClass pc =
  case pc of
    Bs -> 0
    C  -> 0
    Cs -> 1
    Df -> 1
    D  -> 2
    Ds -> 3
    Ef -> 3
    E  -> 4
    Es -> 5
    Ff -> 4
    F  -> 5
    Fs -> 6
    Gf -> 6
    G  -> 7
    Gs -> 8
    Af -> 8
    A  -> 9
    As -> 10
    Bf -> 10
    B  -> 11
    Cf -> 11

denormPitchClass :: Integer -> PitchClass
denormPitchClass x = what x
  where
    what 0  = C
    what 1  = Cs
    what 2  = D
    what 3  = Ef
    what 4  = E
    what 5  = F
    what 6  = Fs
    what 7  = G
    what 8  = Af
    what 9  = A
    what 10 = Bf
    what 11 = B
    what _ = error $ "denormalize value " ++ (show x) ++ " is out of range 0 <= " ++ (show x) ++ " <= 11"

-- | Motto is just a list
newtype Motto a = Motto { getMotto :: [a] } deriving (Eq, Show, Functor, Applicative, Monad)

-- | Lift Motto into Monoid using embedded list:  is this useful?
instance Monoid (Motto a) where
  mempty = Motto []
  mappend (Motto xs) (Motto ys) = Motto (xs ++ ys)

-- | Scale is a list of pitch classes.
type Scale = [PitchClass]

-- | Octave covers signed range where 0 corresponds to span above middle C.
--   Octave is computed from count of items in scale.  Integer range vastly
--   exceeds all instrument ranges, unless guarded by minBound and maxBound.
newtype Octave = Octave { getOctave :: Integer } deriving (Eq, Show, Num)

-- | Octave bounds roughly by piano range
instance Bounded Octave where
    minBound = Octave (-3) -- | Piano minimum range, less B..A
    maxBound = Octave 3    -- | Piano maximum range

-- | Pitch requires PitchClass and Octave.
data Pitch = Pitch PitchClass Octave deriving (Eq, Show)

normPitch :: Pitch -> Integer
normPitch (Pitch pc (Octave o)) = normPitchClass pc + (o * 12)

denormPitch :: Integer -> Pitch
denormPitch x  = Pitch (denormPitchClass (x `mod` 12)) (Octave (x `div` 12))

-- | Num instance for Pitch allows trivial derivation of Product and Sum monoids
instance Num Pitch where
    x + y  = denormPitch $ (normPitch x) + (normPitch y)
    x - y  = denormPitch $ (normPitch x) - (normPitch y)
    x * y  = denormPitch $ (normPitch x) * (normPitch y)
    abs p  = denormPitch $ abs (normPitch p)
    signum p
      | (normPitch p) < 0 = -1
      | (normPitch p) > 0 = 1
      | otherwise = 0
    fromInteger = denormPitch

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
    minBound = PanDegrees 0    -- | Mimic circle, maybe replace with -90?
    maxBound = PanDegrees 359  -- | Mimic circle, maybe replace with +90?

-- | Control events map to Midi controllers, currently limited to Dynamic and Pan.
data ControlEvent = DynamicControl Dynamic Rhythm | PanControl PanDegrees Rhythm deriving (Show)

-- | A note is either a note with the default velocity, a note with a particular velocity, or a rest.
data NoteEvent = Note Pitch Rhythm | AccentedNote Pitch Rhythm Accent | Rest Rhythm deriving (Eq, Show)

-- | A NoteMotto is a list of NoteEvents
type NoteMotto = Motto NoteEvent

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

{--
Sum and product for normalized Pitch.  Note that denormalization
may map to enharmonic equivalent outside a controlling Scale, to
be fixed with a remapping of PitchClass.

Ordinary arithmetic, and especially product, over normalized Pitch
can carry result out of range, assuming the resulting Pitch is ever
rendered.

*ScoreInstances> (Product (Pitch C (Octave 0))) `mappend` (Product (Pitch C (Octave 1)))
Product {getProduct = Pitch C (Octave {getOctave = 0})}
*ScoreInstances> (Product (Pitch C (Octave 1))) `mappend` (Product (Pitch C (Octave 1)))
Product {getProduct = Pitch C (Octave {getOctave = 12})}
*ScoreInstances> (Product (Pitch C (Octave 0))) `mappend` (Product (Pitch C (Octave 1)))
Product {getProduct = Pitch C (Octave {getOctave = 0})}
*ScoreInstances> (Product (Pitch Cs (Octave 0))) `mappend` (Product (Pitch C (Octave 1)))
Product {getProduct = Pitch C (Octave {getOctave = 1})}
*ScoreInstances> (Product (Pitch Cs (Octave 0))) `mappend` mempty
Product {getProduct = Pitch Cs (Octave {getOctave = 0})}
*ScoreInstances> (Product (Pitch Cs (Octave 1))) `mappend` mempty
Product {getProduct = Pitch Cs (Octave {getOctave = 1})}
*ScoreInstances> (Sum (Pitch Cs (Octave 1))) `mappend` mempty
Sum {getSum = Pitch Cs (Octave {getOctave = 1})}
*ScoreInstances> (Sum (Pitch Cs (Octave (-1)))) `mappend` mempty
Sum {getSum = Pitch Cs (Octave {getOctave = -1})}
*ScoreInstances> (Product (Pitch Cs (Octave (-1)))) `mappend` mempty
Product {getProduct = Pitch Cs (Octave {getOctave = -1})}

and, FWIW:

import qualified Data.Foldable as F

*ScoreInstances> getSum $ F.foldMap Sum [(Pitch D (Octave 0)), (Pitch E (Octave 0))]
Pitch Fs (Octave {getOctave = 0})
*ScoreInstances> getProduct $ F.foldMap Product [(Pitch D (Octave 0)), (Pitch E (Octave 0))]
Pitch Af (Octave {getOctave = 0})

Reducing a list of any of the list (Motto) based types to a
single value seems sort of pointless.
--}

