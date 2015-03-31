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
  arbitrary = elements [Pianissimo .. Fortissimo]

-- Type signature requires FlexibleContexts
cmpFun :: (MonadState (Bool, t) m, Ord t) => Ordering -> t -> m Bool
cmpFun ord b = get >>= \(ok, a) -> let ok' = ok && a `compare` b == ord in put (ok', b) >> return ok'

-- End value must be equal to translation for start and stop inputs,
-- intermediate values must be consistently increasing (repetitions allowed).
-- Count of elements must equal to value of Duration.
-- (Start value is equal to start less increment.)
testSynthesizeSpan :: (Ord a, Ord b) => Ordering ->  (a -> a -> Duration -> [b]) -> (a -> b) -> a -> a -> Duration -> Property
testSynthesizeSpan cmp synth cnvt start stop dur =
  start `compare` stop == cmp ==>
    last vals == cnvt stop
    && isUniform valSingles
    && length vals == fromIntegral (getDur dur)
    where
      vals           = synth start stop dur
      valSingles     = (map head . group) vals
      isUniform xs = and $ evalState (traverse (cmpFun cmp) (tail xs)) (True, head xs)

testSynthesizeCrescendoSpan :: Dynamic -> Dynamic -> Duration -> Property
testSynthesizeCrescendoSpan =
  testSynthesizeSpan LT synthesizeCrescendoSpan dynamicToVolume
  
testSynthesizeDecrescendoSpan :: Dynamic -> Dynamic -> Duration -> Property
testSynthesizeDecrescendoSpan =
  testSynthesizeSpan GT synthesizeDecrescendoSpan dynamicToVolume
      
-- Stop short of EndCrescendo, Decrescendo, and etc. to avoid errors when tryig to convert to Int.
instance Arbitrary Pan where
  arbitrary = elements $ map Pan [0..127]

testSynthesizeUpPanSpan :: Pan -> Pan -> Duration -> Property
testSynthesizeUpPanSpan = testSynthesizeSpan LT synthesizeUpPanSpan id 
  
testSynthesizeDownPanSpan :: Pan -> Pan -> Duration -> Property
testSynthesizeDownPanSpan = testSynthesizeSpan GT synthesizeDownPanSpan id
      
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

-- | Refactor
bindTempos :: TempoValue -> TempoValue -> (Tempo, Tempo)
bindTempos start stop =
  (startTempo, stopTempo)
  where
    (start', stop') = normalizeTempoValues start stop
    startTempo      = tempoValueToTempo start'
    stopTempo       = tempoValueToTempo stop'

testSynthesizeAccelerandoSpan :: NormalizedTempoValues -> Duration -> Property
testSynthesizeAccelerandoSpan (NormalizedTempoValues start stop) =
  testSynthesizeSpan LT synthesizeAccelerandoSpan id startTempo stopTempo
  where
    (startTempo, stopTempo) = bindTempos start stop
      
testSynthesizeRitardandoSpan :: NormalizedTempoValues -> Duration -> Property
testSynthesizeRitardandoSpan (NormalizedTempoValues start stop) =
  testSynthesizeSpan GT synthesizeRitardandoSpan id startTempo stopTempo
  where
    (startTempo, stopTempo) = bindTempos start stop
