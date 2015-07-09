{-# LANGUAGE FlexibleContexts #-}

module ScoreToMidi.UtilsTest where

import           Control.Monad.State
import           Data.List
import           Data.Traversable
import           Music.Data
import           ScoreToMidi.Utils
import qualified Sound.MIDI.Message.Channel as ChannelMsg
import           Test.HUnit
import           Test.QuickCheck

testMapVoicessToDifferentChannelss :: Assertion
testMapVoicessToDifferentChannelss =
  [[0,1], [2], [3], [4], [9]] @=? channels
  where
    voicess = 
      [[Voice (Instrument "trumpet") [],Voice (Instrument "trumpet") []],
       [Voice (Instrument "piano") []], [Voice (Instrument "flute") []], [Voice (Instrument "Marimba") []], [Voice (Instrument "Cowbell") []]]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToDifferentChannelss voicess

testMapManyVoicessToDifferentChannelss :: Assertion
testMapManyVoicessToDifferentChannelss =
  [[0,0,0,0], [1,1,1,1], [2,2,2,2], [3,3,3,3], [9,9,9,9]] @=? channels
  where
    voicess = 
      [replicate 4 (Voice (Instrument "trumpet") []),
       replicate 4 (Voice (Instrument "piano")   []),
       replicate 4 (Voice (Instrument "flute")   []),
       replicate 4 (Voice (Instrument "Marimba") []),
       replicate 4 (Voice (Instrument "Cowbell") [])]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToDifferentChannelss voicess

testMapVoicessToUniformChannelss :: Assertion
testMapVoicessToUniformChannelss =
  [[0, 0], [0], [0], [0], [9]] @=? channels 
  where
    voicess = 
      [[Voice (Instrument "trumpet") [],Voice (Instrument "trumpet") []],
       [Voice (Instrument "piano") []], [Voice (Instrument "flute") []], [Voice (Instrument "Marimba") []], [Voice (Instrument "Cowbell") []]]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToUniformChannelss voicess

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
  forAll arbitrary $ \(IntDurPair cnt dur) -> dur == sum (synthesizeDurationSpan cnt (getDur dur))

-- Stop short of EndCrescendo, Decrescendo, and etc. to avoid errors when tryig to convert to Int.
instance Arbitrary DiscreteDynamicValue where
  arbitrary = elements [Pianissimo .. Fortissimo]

-- Type signature requires FlexibleContexts
cmpFun :: (MonadState (Bool, t) m, Ord t) => Ordering -> t -> m Bool
cmpFun ord b = get >>= \(ok, a) -> let ok' = ok && a `compare` b == ord in put (ok', b) >> return ok'

-- Intermediate values must be consistently increasing (repetitions allowed).
testSynthesizeSpan :: (Ord a, Ord b) => Ordering ->  (a -> a -> Duration -> ([b],[Duration])) -> (a -> b) -> a -> a -> Duration -> Property
testSynthesizeSpan cmp synth cnvt start stop dur =
  start `compare` stop == cmp ==>
    if cmp == LT
    then
      head vals >= cnvt start
      && last vals <= cnvt stop
    else
      cnvt start >= head vals 
      && cnvt stop <= last vals 
    && isUniform valSingles
    where
      vals           = fst $ synth start stop dur
      valSingles     = (map head . group) vals
      isUniform xs = and $ evalState (traverse (cmpFun cmp) (tail xs)) (True, head xs)

testSynthesizeCrescendoSpan :: DiscreteDynamicValue -> DiscreteDynamicValue -> Duration -> Property
testSynthesizeCrescendoSpan =
  testSynthesizeSpan LT synthesizeCrescendoSpan dynamicToVolume
  
testSynthesizeDecrescendoSpan :: DiscreteDynamicValue -> DiscreteDynamicValue -> Duration -> Property
testSynthesizeDecrescendoSpan =
  testSynthesizeSpan GT synthesizeDecrescendoSpan dynamicToVolume
      
-- Stop short of EndCrescendo, Decrescendo, and etc. to avoid errors when tryig to convert to Int.
instance Arbitrary Pan where
  arbitrary = elements $ map Pan [0..127]

testSynthesizeUpPanSpan :: Pan -> Pan -> Duration -> Property
testSynthesizeUpPanSpan = testSynthesizeSpan LT synthesizeUpPanSpan id 
  
testSynthesizeDownPanSpan :: Pan -> Pan -> Duration -> Property
testSynthesizeDownPanSpan = testSynthesizeSpan GT synthesizeDownPanSpan id

instance Arbitrary TempoVal where
  arbitrary =
    do
      den <- elements [(minBound::RhythmDenom)..(maxBound::RhythmDenom)]
      bpm <- elements [1..200]
      return $ TempoVal den bpm

-- Construct pair pre-normalized
data NormalizedTempoVals = NormalizedTempoVals TempoVal TempoVal deriving (Show)
instance Arbitrary NormalizedTempoVals where
  arbitrary =
    do
      tempoOne <- arbitrary
      tempoTwo <- arbitrary
      let (tempoOne', tempoTwo') = normalizeTempoVals tempoOne tempoTwo
      return $ NormalizedTempoVals tempoOne' tempoTwo'

testSynthesizeAccelerandoSpan :: NormalizedTempoVals -> Duration -> Property
testSynthesizeAccelerandoSpan (NormalizedTempoVals start stop) =
  testSynthesizeSpan LT synthesizeAccelerandoSpan id start stop
      
testSynthesizeRitardandoSpan :: NormalizedTempoVals -> Duration -> Property
testSynthesizeRitardandoSpan (NormalizedTempoVals start stop) =
  testSynthesizeSpan GT synthesizeRitardandoSpan id start stop
