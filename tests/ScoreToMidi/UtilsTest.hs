
module ScoreToMidi.UtilsTest where

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

instance Arbitrary Duration where
  arbitrary = elements [(minBound::Duration)..(maxBound::Duration)]

{--
synthesizeDurationSpan :: Int -> Duration -> [Duration]
synthesizeDurationSpan cnt (Dur total) =
--}

-- Int is count of elements in list of synthesized controls.
-- Duration is from some Rhythm.
testSynthesizeDurationSpan :: Int -> Duration -> Property
testSynthesizeDurationSpan cnt dur =
  cnt > 0 && cnt < (2 * dur') ==>
    dur == sum (synthesizeDurationSpan cnt dur)
    where
      dur' = fromIntegral $ getDur dur
