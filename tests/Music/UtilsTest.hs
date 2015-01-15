module Music.UtilsTest where

import Data.List  (findIndex, elemIndex)
import Data.Maybe (fromJust)

import Test.HUnit
import Test.QuickCheck

import Music.Data
import Music.Utils

testSliceInMiddle :: Assertion
testSliceInMiddle =
  [4::Integer,5] @=? slice 4 5 [0..10]
  
testSliceLow :: Assertion
testSliceLow =
  [] @=? slice (-100) (-300) [0::Integer,1..10]
  
testSliceHigh :: Assertion
testSliceHigh =
  [] @=? slice 100 300 [0::Integer,1..10]

propSliceAllIsOriginalList :: [Int] -> Property
propSliceAllIsOriginalList xs =
    not (null xs) ==>
        slice 0 (length xs) xs == xs
        
propSliceFirstIsHead :: [Int] -> Property
propSliceFirstIsHead xs =
    not (null xs) ==>
        slice 0 0 xs == [head xs]

testRotateTo :: Assertion
testRotateTo =
  [5::Integer,6..10] ++ [1..4] @=?  rotateTo 5 [1..10]

propRotateToFirstIsSame :: [Int] -> Property
propRotateToFirstIsSame xs =
  not (null xs) ==>
    rotateTo (head xs) xs == xs

getChromaticScaleIndex :: PitchClass -> Int
getChromaticScaleIndex pc  =
  fromJust $ findIndex (\pcs -> pc `elem` pcs) scale
  where
    scale = [[Bs,C],[Cs,Df],[D],[Ds,Ef],[E,Ff],[Es,F],[Fs,Gf],[G],[Gs,Af],[A],[As,Bf],[B,Cf]]

testGetChromaticScaleIndex :: Assertion
testGetChromaticScaleIndex =
  [0, 0, 1, 11] @=? [getChromaticScaleIndex C, getChromaticScaleIndex Bs, getChromaticScaleIndex Df, getChromaticScaleIndex B]

pitchClassPairsToChromaticInterval :: (PitchClass, PitchClass) -> Interval
pitchClassPairsToChromaticInterval (pc1, pc2) 
  | pc1Idx < pc2Idx = pc2Idx - pc1Idx
  | otherwise       = octIdx - (pc1Idx - pc2Idx)
  where
    pc1Idx = getChromaticScaleIndex pc1
    pc2Idx = getChromaticScaleIndex pc2
    octIdx = 12

testMajorScaleIntervals :: [PitchClass] -> Bool
testMajorScaleIntervals scale =
  map pitchClassPairsToChromaticInterval pitchClassPairs == majorScaleIntervals
  where
    pitchClassPairs = zip scale $ tail scale
    majorScaleIntervals = [2,2,1,2,2,2]
    
testMajorScaleInterval :: Assertion
testMajorScaleInterval =
  True @=? testMajorScaleIntervals (majorScale C)
  
instance Arbitrary PitchClass where
  arbitrary = arbitraryBoundedEnum

pitchClassInSingleAccidentalRange :: PitchClass -> Bool
pitchClassInSingleAccidentalRange tonic =
  case pitchClass2MaybeOneAccidentalScaleIndex tonic of
    Nothing -> False
    Just _ -> True

propMajorScaleIntervals :: PitchClass -> Property
propMajorScaleIntervals tonic = 
  pitchClassInSingleAccidentalRange tonic ==>
    testMajorScaleIntervals (majorScale tonic)

getIntervalForScale :: Scale -> Pitch -> Pitch -> Interval
getIntervalForScale scale (Pitch pc1 _) (Pitch pc2 _) =
  if p2Idx > p1Idx
  then
    p2Idx - p1Idx
  else
    scaleLen + p1Idx - p2Idx
  where
    p1Idx    = fromJust $ elemIndex pc1 scale
    p2Idx    = fromJust $ elemIndex pc2 scale
    scaleLen = length scale

testTransposePitch :: Assertion    
testTransposePitch =
  transposeInterval @=? getIntervalForScale scale startPitch transposedPitch
  where
    pitchClass        = C 
    transposeInterval = 5
    scale             = majorScale pitchClass
    startPitch        = Pitch pitchClass (Octave 5)
    transposedPitch   = transposePitch scale transposeInterval startPitch
               
-- Next, property that says when you transpose
-- an interval away and back you get the same
-- pitch.
-- Need Arbitrary instance for Pitch, which means
-- composition of Arbitrary instances for PitchClass
-- and Octave.
-- Need Arbitrary instance for Octave, which should
-- obey bounds.
