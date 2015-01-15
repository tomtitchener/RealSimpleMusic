
module Main (
    main
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Music.UtilsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "Example"
  [
    testCase           "mid slice"              testSliceInMiddle
  , testProperty       "slice all is id"        propSliceAllIsOriginalList
  , testProperty       "slice one is head"      propSliceFirstIsHead        
  , testCase           "rotateTo"               testRotateTo
  , testProperty       "rotateTo head is id"    propRotateToFirstIsSame
  , testCase           "chromatic scale index"  testGetChromaticScaleIndex
  , testCase           "major scale interval"   testMajorScaleInterval
  , testProperty       "major scale intervals"  propMajorScaleIntervals
  , testCase           "transpose pitch"        testTransposePitch
  ]


