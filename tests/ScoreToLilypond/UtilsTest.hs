module ScoreToLilypond.UtilsTest where

import Data.List  (sort)
import Data.Ratio
import Data.ByteString.Builder
  
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

testRenderPitchOctaves :: Assertion
testRenderPitchOctaves =
  map (toLazyByteString . stringEncoding) ["c'", "c''", "c'''", "c", "c,", "c,,"] @=? map (toLazyByteString . renderPitch) [Pitch C (Octave octave) | octave <- [0, 1, 2, -1, -2, -3]]

testRenderPitchAccidentals :: Assertion
testRenderPitchAccidentals =
  map (toLazyByteString . stringEncoding) ["c", "ces", "ceses", "cis", "cisis"] @=? map (toLazyByteString . renderPitch) [Pitch pc (Octave (-1)) | pc <- [C, Cf, Cff, Cs, Css]]

testRenderRhythmBase :: Assertion
testRenderRhythmBase =
  map (toLazyByteString . stringEncoding) ["1", "2", "4", "8", "16", "32", "64"] @=? map toLazyByteString (concatMap (renderRhythm . Rhythm) [1%1, 1%2, 1%4, 1%8, 1%16, 1%32, 1%64])

testRenderRhythmDots :: Assertion
testRenderRhythmDots =
  map (toLazyByteString . stringEncoding) ["1.", "2.", "4.", "8.", "16.", "32.", "64."] @=? map toLazyByteString (concatMap (renderRhythm . Rhythm) [3%2, 3%4, 3%8, 3%16, 3%32, 3%64, 3%128])

testRenderRhythmTies :: Assertion
testRenderRhythmTies =
  undefined
