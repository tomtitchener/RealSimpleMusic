{-# LANGUAGE FlexibleContexts #-}

module ScoreToMidi.UtilsTest where

import           Control.Monad.State
import           Data.List
import           Data.Ratio
import           Data.Traversable
import           RealSimpleMusic
import           ScoreToMidi.Utils
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import           Test.HUnit
import           Test.QuickCheck

testMapVoicessToChannelss :: Assertion
testMapVoicessToChannelss =
  [[0,1], [2], [3], [4], [9]] @=? channels
  where
    voicess = 
      [[Voice (Instrument "trumpet") [],Voice (Instrument "trumpet") []],
       [Voice (Instrument "piano") []], [Voice (Instrument "flute") []], [Voice (Instrument "Marimba") []], [Voice (Instrument "Cowbell") []]]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToChannelss voicess

testMapManyVoicessToChannelss :: Assertion
testMapManyVoicessToChannelss =
  [[0,0,0,0], [1,1,1,1], [2,2,2,2], [3,3,3,3], [9,9,9,9]] @=? channels
  where
    voicess = 
      [replicate 4 (Voice (Instrument "trumpet") []),
       replicate 4 (Voice (Instrument "piano")   []),
       replicate 4 (Voice (Instrument "flute")   []),
       replicate 4 (Voice (Instrument "Marimba") []),
       replicate 4 (Voice (Instrument "Cowbell") [])]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToChannelss voicess

testMapVoicessToPercussionChannelss :: Assertion
testMapVoicessToPercussionChannelss =
  [[0, 0], [0], [0], [0], [9]] @=? channels 
  where
    voicess = 
      [[Voice (Instrument "trumpet") [],Voice (Instrument "trumpet") []],
       [Voice (Instrument "piano") []], [Voice (Instrument "flute") []], [Voice (Instrument "Marimba") []], [Voice (Instrument "Cowbell") []]]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToPercussionChannelss voicess

-- Reduce upper limit to avoid allocating enormous amounts of memory when
-- testing generation of duration span.  Raise lower limit to avoid zero
-- value Duration for spans.
instance Arbitrary Duration where
  arbitrary = elements [{--(minBound::Duration)--}1..10000{--(maxBound::Duration)--}]

-- Special-purpose generator to test duration span.
-- Duration can be anything reasonable for memory consumption.
-- Count has to be between 1 and Duration value.
data IntDurPair = IntDurPair { intPairDurX :: Int, intPairDurD :: Duration } deriving (Show)
instance Arbitrary IntDurPair where
  arbitrary =
    do
      d <- arbitrary
      x <- elements [1..(fromIntegral (getDur d))]
      return $ IntDurPair x d

-- Sum of durs in dur span must exactly equal input dur
testSynthesizeDurationSpan :: Property
testSynthesizeDurationSpan =
  forAll arbitrary $ \(IntDurPair cnt dur) -> dur == sum (synthesizeDurationSpan cnt dur)

-- Stop short of EndCrescendo, Decrescendo, and etc. to avoid errors when tryig to convert to Int.
instance Arbitrary Dynamic where
  arbitrary = elements [(Pianissimo)..(Fortissimo)]

-- Type signature requires FlexibleContexts
-- TBD:  refactor without do notation.
cmpFun :: (MonadState (Bool, t) m, Ord t) => Ordering -> t -> m Bool
cmpFun ord b =
  do
    (ok, a) <- get
    let ok' = ok && a `compare` b == ord
    put (ok', b)
    return ok'

-- TBD: refactor abstracting (possible for Tempo with conversions?)
--  * type (Dynamic, Pan, Tempo),
--  * translation to compare list (dynamicToVolume, id, id)
--  * ascending vs. descending (LT vs. GT)
--  * synthesize method (synthesizeCrescendoSpan, etc.)

-- Stop value must be equal to Volume translation for start and stop inputs,
-- intermediate values must be consistently increasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start less increment.)
testSynthesizeCrescendoSpan :: Dynamic -> Dynamic -> Duration -> Property
testSynthesizeCrescendoSpan start stop dur =
  start < stop ==>
    last vols == dynamicToVolume stop
    && isAscending volSingles
    && length vols == fromIntegral (getDur dur)
    where
      vols           = synthesizeCrescendoSpan start stop dur
      volSingles     = (map head . group) vols
      isAscending xs = and $ evalState (traverse (cmpFun LT) (tail xs)) (True, head xs)

-- Stop value must be equal to Volume translation for start and stop inputs,
-- intermediate values must be consistently decreasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start less increment.)
testSynthesizeDecrescendoSpan :: Dynamic -> Dynamic -> Duration -> Property
testSynthesizeDecrescendoSpan start stop dur =
  start > stop ==>
    last vols == dynamicToVolume stop
    && isDescending volSingles
    && length vols == fromIntegral (getDur dur)
    where
      vols            = synthesizeDecrescendoSpan start stop dur
      volSingles      = (map head . group) vols
      isDescending xs = and $ evalState (traverse (cmpFun GT) (tail xs)) (True, head xs)

-- Stop short of EndCrescendo, Decrescendo, and etc. to avoid errors when tryig to convert to Int.
instance Arbitrary Pan where
  arbitrary = elements $ map Pan [0..127]

-- Stop value must be equal to Volume translation for start and stop inputs,
-- intermediate values must be consistently increasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start less increment.)
testSynthesizeUpPanSpan :: Pan -> Pan -> Duration -> Property
testSynthesizeUpPanSpan start stop dur =
  start < stop ==>
    last pans == stop
    && isAscending panSingles
    && length pans == fromIntegral (getDur dur)
    where
      pans           = synthesizeUpPanSpan start stop dur
      panSingles     = (map head . group) pans
      isAscending xs = and  $ evalState (traverse (cmpFun LT) (tail xs)) (True, head xs)

-- Stop value must be equal to Volume translation for start and stop inputs,
-- intermediate values must be consistently decreasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start less increment.)
testSynthesizeDownPanSpan :: Pan -> Pan -> Duration -> Property
testSynthesizeDownPanSpan start stop dur =
  start > stop ==>
    last pans == stop
    && isDescending panSingles
    && length pans == fromIntegral (getDur dur)
    where
      pans            = synthesizeDownPanSpan start stop dur
      panSingles      = (map head . group) pans
      isDescending xs = and $ evalState (traverse (cmpFun GT) (tail xs)) (True, head xs)

instance Ord TempoValue where
  one `compare` two = bpmOne `compare` bpmTwo where (TempoValue _ bpmOne, TempoValue _ bpmTwo) = normalizeTempoValues one two

instance Arbitrary TempoValue where
  arbitrary =
    do
      num <- elements [1, 2, 4, 8, 16, 32, 64, 128, 256]
      bpm <- elements [1..200]
      return $ TempoValue (1%num) bpm

-- Construct pair pre-normalized
data NormalizedTempoValues = NormalizedTempoValues TempoValue TempoValue deriving (Show)
instance Arbitrary NormalizedTempoValues where
  arbitrary =
    do
      tempoOne <- arbitrary
      tempoTwo <- arbitrary
      let (tempoOne', tempoTwo') = normalizeTempoValues tempoOne tempoTwo
      return $ NormalizedTempoValues tempoOne' tempoTwo'

-- Stop value must be equal to Tempo translation for start and stop inputs,
-- intermediate values must be consistently increasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start less increment.)
testSynthesizeAccelerandoSpan :: NormalizedTempoValues -> Duration -> Property
testSynthesizeAccelerandoSpan (NormalizedTempoValues start stop) dur =
  start < stop ==>
    last tempos == stopTempo
    && isAscending tempoSingles
    && length tempos == fromIntegral (getDur dur)
    where
      (start', stop') = normalizeTempoValues start stop
      startTempo      = tempoValueToTempo start'
      stopTempo       = tempoValueToTempo stop'
      tempos          = synthesizeAccelerandoSpan startTempo stopTempo dur
      tempoSingles    = (map head . group) tempos
      isAscending xs  = and  $ evalState (traverse (cmpFun LT) (tail xs)) (True, head xs)

-- Stop value must be equal to Tempo translation for start and stop inputs,
-- intermediate values must be consistently decreasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start plus increment.)
testSynthesizeRitardandoSpan :: NormalizedTempoValues -> Duration -> Property
testSynthesizeRitardandoSpan (NormalizedTempoValues start stop) dur =
  start > stop ==>
    last tempos == stopTempo
    && isDescending tempoSingles
    && length tempos == fromIntegral (getDur dur)
    where
      startTempo      = tempoValueToTempo start
      stopTempo       = tempoValueToTempo stop
      tempos          = synthesizeRitardandoSpan startTempo stopTempo dur
      tempoSingles    = (map head . group) tempos
      isDescending xs = and  $ evalState (traverse (cmpFun GT) (tail xs)) (True, head xs)
