module Music.UtilsTest where

import Data.List  (findIndex, sort, elemIndex)
import Data.Maybe (fromJust)

import Test.HUnit
import Test.QuickCheck

import Music.Data
import Music.Utils

testSliceInMiddle :: Assertion
testSliceInMiddle =
  [4,5] @=? slice 4 5 [0..10]
  
testSliceLow :: Assertion
testSliceLow =
  [] @=? slice (-100) (-300) [0..10]
  
testSliceHigh :: Assertion
testSliceHigh =
  [] @=? slice 100 300 [0..10]

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
  [5..10] ++ [1..4] @=?  rotateTo 5 [1..10]

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

-- given pair of pitch classes where pc1 < pc2, answer Interval
-- that is the distance between the two in half-steps
-- pair may wrap octave, e.g. (Bf, C), in which case Interval
-- octave length (12) less distance (10), e.g. 2.
pitchClassPairsToInterval :: (PitchClass, PitchClass) -> Interval
pitchClassPairsToInterval (pc1, pc2) 
  | pc1Idx < pc2Idx = pc2Idx - pc1Idx
  | otherwise       = octIdx - (pc1Idx - pc2Idx)
  where
    pc1Idx = getChromaticScaleIndex pc1
    pc2Idx = getChromaticScaleIndex pc2
    octIdx = 12

-- verify all major scales have sequence of half-steps that goes [2,2,1,2,2,2]
-- order notes in list of pairs, e.g. [(Ef,F),(F,G),(G,Af),(Af,Bf),(Bf,C),(C,D)]
-- map note pairs into list of intervals, e.g. [2,2,1,2,2,2]
testMajorScaleIntervals :: [PitchClass] -> Bool
testMajorScaleIntervals scale =
  map pitchClassPairsToInterval pitchClassPairs == majorScaleIntervals
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
