
module Main (
    main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Music.UtilsTest
import ScoreToMidi.UtilsTest
import Canon.UtilsTest
import ScoreToLilypond.UtilsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Example"
  [
    testCase           "mid slice"                         testSliceInMiddle
  , testProperty       "slice all is id"                   propSliceAllIsOriginalList
  , testProperty       "slice one is head"                 propSliceFirstIsHead
  , testCase           "rotateTo"                          testRotateTo
  , testProperty       "rotateTo head is id"               propRotateToFirstIsSame
  , testCase           "cycle of fifths length"            testCycleOfFifthsLength
  , testCase           "cycle of fifths vals"              testCycleOfFifthsValues
  , testCase           "enh chromatic pitch class vals"    testEnhChromPitchClassValues
  , testCase           "fifths enh degrees len"            testFifthsEnhDegreesLen
  , testCase           "fifths enh degrees vals"           testFifthsEnhDegreesValues
  , testCase           "chromatic scale index"             testGetChromaticScaleIndex
  , testCase           "major scale interval"              testMajorScaleInterval
  , testProperty       "major scale intervals"             propMajorScaleIntervals
  , testCase           "transpose pitch"                   testTransposePitch
  , testProperty       "transpose pitch id"                propTransposePitchId
  , testProperty       "major scale"                       propMajorScaleId
  , testProperty       "natural minor scale"               propNaturalMinorScaleId
  -- ScoreToMidi
  , testCase           "mapVoicessToChannelss"             testMapVoicessToChannelss
  , testCase           "mapManyVoicessToChannelss"         testMapManyVoicessToChannelss
  , testCase           "mapVoicessToPercussionChannelss"   testMapVoicessToPercussionChannelss
  -- ScoreToLilypond
  , testCase           "equiv pitch class vals"            testEquivPitchClassValues
  , testCase           "testPitchNames"                    testPitchNames
  , testCase           "testAccidentalNames"               testAccidentalNames
  , testCase           "testAccentNames"                   testAccentNames
  , testCase           "testRenderPitchOctaves"            testRenderPitchOctaves
  , testCase           "testRenderPitchAccidentals"        testRenderPitchAccidentals
  , testCase           "testRenderRhythmBase"              testRenderRhythmBase
  , testCase           "testRenderRhythmDots"              testRenderRhythmDots
  , testCase           "testRenderRhythmTies"              testRenderRhythmTies
  , testCase           "testRenderNote"                    testRenderNote
  , testCase           "testRenderAccentedNote"            testRenderAccentedNote
  , testCase           "testRenderRest"                    testRenderRest
  , testCase           "testRenderPercussionNote"          testRenderPercussionNote
  , testCase           "testRenderAccentedPercussionNote"  testRenderAccentedPercussionNote
  , testCase           "testRenderTiedNote"                testRenderTiedNote
  , testCase           "testRenderNotes"                   testRenderNotes
  -- canons
  , testCase           "simple canon"                      testSimpleCanon
  , testCase           "scales canon"                      testScalesCanon
  , testCase           "canon"                             testCanon
  ]


