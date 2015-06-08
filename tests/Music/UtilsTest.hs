module Music.UtilsTest where

import Control.Monad
import Data.List (findIndex, elemIndex, sort, group)
import Data.Maybe (fromJust, isJust)
import Data.Either.Combinators
import Music.Data
import Music.Utils
import Test.HUnit
import Test.QuickCheck

allPitchClasses :: [PitchClass]
allPitchClasses = [(minBound::PitchClass)..(maxBound::PitchClass)]

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

testRotateToErrContains :: [Int] -> Property
testRotateToErrContains xs =
  not (null xs) ==>
    and (map (isRight . flip rotateToErr xs) xs)
    
testRotateToErrContainsNot :: [Int] -> Property
testRotateToErrContainsNot xs =
  not (null xs) ==>
    and (map (isLeft . flip rotateToErr ys) zs)
    where
      pivot = xs !! 0
      ys = filter (< pivot) xs
      zs = filter (>= pivot) xs

testIsSharpErrSuccess :: Assertion
testIsSharpErrSuccess = True @=? fromRight False (isSharpErr Cs)

testIsSharpErrFail :: Assertion
testIsSharpErrFail = False @=? fromRight True (isSharpErr Cf)
    
propRotateToFirstIsSame :: [Int] -> Property
propRotateToFirstIsSame xs =
  not (null xs) ==>
    rotateTo (head xs) xs == xs

testCycleOfFifthsLength :: Assertion
testCycleOfFifthsLength =
  length allPitchClasses @=? length cycleOfFifths
  
testCycleOfFifthsValues :: Assertion
testCycleOfFifthsValues =
  map (`elem` cycleOfFifths) allPitchClasses @=? replicate (length allPitchClasses) True

testEnhChromPitchClassValues :: Assertion
testEnhChromPitchClassValues =
 allPitchClasses @=? (sort . concat) enhChromPitchClasses

testFifthsEnhDegreesLen :: Assertion 
testFifthsEnhDegreesLen =
  length cycleOfFifths @=? length fifthsEnhDegrees

testFifthsEnhDegreesValues :: Assertion 
testFifthsEnhDegreesValues =
  map head (group fifthsEnhDegrees) @=? [-2,-1,0,1,2]
    
getChromaticScaleIndex :: PitchClass -> Int
getChromaticScaleIndex pc  =
  fromJust $ findIndex (\pcs -> pc `elem` pcs) scale
  where
    scale = [[Bs,C,Dff],[Bss,Cs,Df],[Css,D,Eff],[Ds,Ef,Fff],[Dss,E,Ff],[Es,F,Gff],[Ess,Fs,Gf],[Fss,G,Aff],[Gs,Af],[Gss,A,Bff],[As,Bf,Cff],[Ass,B,Cf]]

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

testMajorScaleIntervals :: Scale -> Bool
testMajorScaleIntervals scale =
  map pitchClassPairsToChromaticInterval pitchClassPairs == majorScaleIntervals
  where
    scale' = ascendingScale scale
    pitchClassPairs = zip scale' $ tail scale'
    majorScaleIntervals = [2,2,1,2,2,2]
    
testMajorScaleInterval :: Assertion
testMajorScaleInterval =
  True @=? testMajorScaleIntervals (majorScale C)
  
instance Arbitrary PitchClass where
  arbitrary = arbitraryBoundedEnum

pitchClassInMajorScaleRange :: PitchClass -> Bool
pitchClassInMajorScaleRange tonic =
  isJust $ pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic

propMajorScaleIntervals :: PitchClass -> Property
propMajorScaleIntervals tonic = 
  pitchClassInMajorScaleRange tonic ==>
    testMajorScaleIntervals (majorScale tonic)

getIntervalForScale :: Scale -> Pitch -> Pitch -> Interval
getIntervalForScale scale (Pitch pc1 _) (Pitch pc2 _) =
  if p2Idx > p1Idx
  then
    p2Idx - p1Idx
  else
    scaleLen + p1Idx - p2Idx
  where
    scale'   = ascendingScale scale
    p1Idx    = fromJust $ elemIndex pc1 scale'
    p2Idx    = fromJust $ elemIndex pc2 scale'
    scaleLen = length scale'

testTransposePitch :: Assertion    
testTransposePitch =
  transposeInterval @=? getIntervalForScale scale startPitch transposedPitch
  where
    pitchClass        = C 
    transposeInterval = 5
    scale             = majorScale pitchClass
    startPitch        = Pitch pitchClass (Octave 5)
    transposedPitch   = transposePitch scale transposeInterval startPitch

instance Arbitrary Octave where
  arbitrary = elements [(minBound::Octave)..(maxBound::Octave)]

instance Arbitrary Pitch where
  arbitrary = liftM2 Pitch arbitrary arbitrary

-- For transposing up and transposing down to land on the
-- same scale degree there cannot be any duplicates in the
-- scale.  
instance Arbitrary Scale where
  arbitrary =
    do
      pitchClasses <- listOf1 arbitrary
      rotN <- arbitrary
      let
        rmdups = map head . group . sort
        pcs = rotate (rotN `mod` length pitchClasses) (rmdups pitchClasses)
        in
          return $ Scale pcs $ reverse pcs

propTransposePitchId :: Scale -> Pitch -> Interval -> Property
propTransposePitchId scale pitch@(Pitch pc _) interval =
  length (ascendingScale scale) >= 2 && interval > 0 && elem pc (ascendingScale scale) ==>
    pitch == pitch''
  where
    pitch'  = transposePitch scale interval pitch
    pitch'' = transposePitch scale (-interval) pitch'

-- | Given a pitch class answer the major scale, up to two accidentals.
majorScaleFromCycleOfFifths :: PitchClass -> Scale
majorScaleFromCycleOfFifths tonic =
  case pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic of
    Nothing -> error $ "majorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMajorScalePitchClass ++ " to " ++ show highestMajorScalePitchClass ++ " in cycle of fifths " ++ show cycleOfFifths
    Just idx -> Scale ascending $ (rotate (-1) . reverse) ascending
      where
        start     = idx - lowestMajorScaleOffset
        stop      = idx + highestMajorScaleOffset
        pcs       = slice start stop cycleOfFifths
        ascending = rotateTo tonic $ sort pcs

propMajorScaleId :: PitchClass -> Property
propMajorScaleId tonic =
  pitchClassInMajorScaleRange tonic ==>
    majorScale tonic == majorScaleFromCycleOfFifths tonic

-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScaleFromCycleOfFifths :: PitchClass -> Scale
naturalMinorScaleFromCycleOfFifths tonic =
  case pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic of
    Nothing -> error $ "naturalMinorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMinorScalePitchClass ++ " to " ++ show highestMinorScalePitchClass ++ " in cycle of fifths " ++ show cycleOfFifths
    Just idx -> Scale ascending $ (rotate (-1) . reverse) ascending
      where
        major = majorScaleFromCycleOfFifths $ cycleOfFifths !! (idx - 3)
        ascending = rotate 5 $ ascendingScale major
  
pitchClassInNaturalMinorScaleRange :: PitchClass -> Bool
pitchClassInNaturalMinorScaleRange tonic =
  isJust $ pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic
  
propNaturalMinorScaleId :: PitchClass -> Property
propNaturalMinorScaleId tonic =
  pitchClassInNaturalMinorScaleRange tonic ==>
    naturalMinorScale tonic == naturalMinorScaleFromCycleOfFifths tonic
