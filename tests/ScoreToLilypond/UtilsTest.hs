module ScoreToLilypond.UtilsTest where

import Data.List  (sort)

import Test.HUnit
-- import Test.QuickCheck

import RealSimpleMusic
import ScoreToLilypond.Utils

allPitchClasses :: [PitchClass]
allPitchClasses = [(minBound::PitchClass)..(maxBound::PitchClass)]

testEquivPitchClassValues :: Assertion
testEquivPitchClassValues =
 allPitchClasses @=? (sort . concat) equivPitchClasses

testPitchNames :: Assertion
testPitchNames =
   length pitchNames @=? length equivPitchClasses

testAccidentalNames :: Assertion
testAccidentalNames =
  replicate (length equivPitchClasses) (length accidentalNames) @=? map length equivPitchClasses

  
