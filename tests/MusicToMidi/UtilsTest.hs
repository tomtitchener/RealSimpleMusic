
module MusicToMidi.UtilsTest where

import           RealSimpleMusic
import           MusicToMidi.Utils
import           Test.HUnit
import qualified Sound.MIDI.Message.Channel       as ChannelMsg

testMapVoicessToChannelss :: Assertion
testMapVoicessToChannelss =
  [[0,1], [2], [3], [4], [9]] @=? channels
  where
    voicess = 
      [[Voice (Instrument "trumpet") [] [],Voice (Instrument "trumpet") [] []],
       [Voice (Instrument "piano") [] []], [Voice (Instrument "flute") [] []], [Voice (Instrument "Marimba") [] []], [Voice (Instrument "Cowbell") [] []]]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToChannelss voicess

testMapManyVoicessToChannelss :: Assertion
testMapManyVoicessToChannelss =
  [[0,0,0,0], [1,1,1,1], [2,2,2,2], [3,3,3,3], [9,9,9,9]] @=? channels
  where
    voicess = 
      [replicate 4 (Voice (Instrument "trumpet") [] []),
       replicate 4 (Voice (Instrument "piano")   [] []),
       replicate 4 (Voice (Instrument "flute")   [] []),
       replicate 4 (Voice (Instrument "Marimba") [] []),
       replicate 4 (Voice (Instrument "Cowbell") [] [])]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToChannelss voicess

testMapVoicessToPercussionChannelss :: Assertion
testMapVoicessToPercussionChannelss =
  [[0, 0], [0], [0], [0], [9]] @=? channels 
  where
    voicess = 
      [[Voice (Instrument "trumpet") [] [],Voice (Instrument "trumpet") [] []],
       [Voice (Instrument "piano") [] []], [Voice (Instrument "flute") [] []], [Voice (Instrument "Marimba") [] []], [Voice (Instrument "Cowbell") [] []]]
    channels =
      (map . map) ChannelMsg.fromChannel $ mapVoicessToPercussionChannelss voicess
    
