module Music.UtilsTest where

import Control.Monad
import Data.List (findIndex, elemIndex, sort, group)
import Data.Maybe (fromJust)
import Data.Either.Combinators (isRight, fromRight')
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

rotateTo :: (Ord a, Show a) => a -> [a] -> [a]
rotateTo x xs =
  case elemIndex x xs of
    Nothing -> error $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    Just i  -> rotate i xs
    
testRotateTo :: Assertion
testRotateTo =
  [5::Integer,6..10] ++ [1..4] @=?  rotateTo 5 [1..10]

propRotateToFirstIsSame :: [Int] -> Property
propRotateToFirstIsSame xs =
  not (null xs) ==>
    rotateTo (head xs) xs == xs

testEnhChromPitchClassValues :: Assertion
testEnhChromPitchClassValues =
 allPitchClasses @=? (sort . concat) enhChromPitchClasses

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
  True @=? testMajorScaleIntervals (fromRight' (majorScale C))
  
instance Arbitrary PitchClass where
  arbitrary = arbitraryBoundedEnum

pitchClassInMajorScaleRange :: PitchClass -> Bool
pitchClassInMajorScaleRange tonic =
  isRight $ testScaleTonicErr tonic "test" lowestMajorScalePitchClass highestMajorScalePitchClass

propMajorScaleIntervals :: PitchClass -> Property
propMajorScaleIntervals tonic = 
  pitchClassInMajorScaleRange tonic ==>
    testMajorScaleIntervals (fromRight' (majorScale tonic))

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

-- | Given a pitch class answer the major scale, up to two accidentals.
--   Sort is by order in enum of pcs as cycle of fifths order whereaas
--   I need stepwise order.  Cycle concatenated fifths order by twos:
--   [Ff,Cf,Gf,Df,Af,Ef,Bff, Ff,Cf,Gf,Df,Af,Ef,Bff]
--     X     X     X     X       X     X     X
--     0     2     4     6       8    10    12
majorScaleFromCycleOfFifths :: PitchClass -> Scale
majorScaleFromCycleOfFifths tonic =
  case testScaleTonicErr tonic "test" lowestMajorScalePitchClass highestMajorScalePitchClass of
    Left err -> error err
    Right _ -> Scale ascending $ (rotate (-1) . reverse) ascending
      where
        start     = fromEnum tonic - 1
        stop      = fromEnum tonic + 5
        pcs       = slice start stop cycleOfFifths
        ascendingFifths = rotateTo tonic $ sort pcs
        ascending = map (\x -> (ascendingFifths ++ ascendingFifths) !! x) [0,2..12] 

propMajorScaleId :: PitchClass -> Property
propMajorScaleId tonic =
  pitchClassInMajorScaleRange tonic ==>
    majorScale tonic == Right (majorScaleFromCycleOfFifths tonic)

-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScaleFromCycleOfFifths :: PitchClass -> Scale
naturalMinorScaleFromCycleOfFifths tonic =
  case testScaleTonicErr tonic "test" lowestMinorScalePitchClass highestMinorScalePitchClass of
    Left err -> error err
    Right _ -> Scale ascending $ (rotate (-1) . reverse) ascending
      where
        major = majorScaleFromCycleOfFifths $ toEnum ((fromEnum tonic) - 3)
        ascending = rotate 5 $ ascendingScale major
  
pitchClassInNaturalMinorScaleRange :: PitchClass -> Bool
pitchClassInNaturalMinorScaleRange tonic =
  isRight $ testScaleTonicErr tonic "test" lowestMinorScalePitchClass highestMinorScalePitchClass

propNaturalMinorScaleId :: PitchClass -> Property
propNaturalMinorScaleId tonic =
  pitchClassInNaturalMinorScaleRange tonic ==>
    naturalMinorScale tonic == Right (naturalMinorScaleFromCycleOfFifths tonic)
