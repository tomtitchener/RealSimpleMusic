module ScoreToLilypond.UtilsTest where

import           Data.List  (sort)
import           Data.Ratio
import qualified Data.Set                  as Set
import           Data.ByteString.Builder
  
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

testAccentNames :: Assertion
testAccentNames = 
  fromEnum (maxBound::Accent) @=? (length accentValues)

testRenderPitchOctaves :: Assertion
testRenderPitchOctaves =
  map (toLazyByteString . stringEncoding) ["c'", "c''", "c'''", "c", "c,", "c,,"] @=? map (toLazyByteString . renderPitch) [Pitch C (Octave octave) | octave <- [0, 1, 2, -1, -2, -3]]

testRenderPitchAccidentals :: Assertion
testRenderPitchAccidentals =
  map (toLazyByteString . stringEncoding) ["c", "ces", "ceses", "cis", "cisis"] @=? map (toLazyByteString . renderPitch) [Pitch pc (Octave (-1)) | pc <- [C, Cf, Cff, Cs, Css]]

testRenderRhythmBase :: Assertion
testRenderRhythmBase =
  (map . map) (toLazyByteString . stringEncoding) [["1"], ["2"], ["4"], ["8"], ["16"], ["32"], ["64"]] @=? (map . map) toLazyByteString (map (renderRhythm . Rhythm) [1%1, 1%2, 1%4, 1%8, 1%16, 1%32, 1%64])

testRenderRhythmDots :: Assertion
testRenderRhythmDots =
  (map . map) (toLazyByteString . stringEncoding) [["1."], ["2."], ["4."], ["8."], ["16."], ["32."], ["64."]] @=? (map . map) toLazyByteString (map (renderRhythm . Rhythm) [3%2, 3%4, 3%8, 3%16, 3%32, 3%64, 3%128])

testRenderRhythmTies :: Assertion
testRenderRhythmTies =
  (map . map) (toLazyByteString . stringEncoding) [["1", "4"], ["1", "2."], ["1", "1", "4"], ["2", "8"], ["2."]] @=? (map . map) toLazyByteString (map (renderRhythm . Rhythm) [5%4, 7%4, 9%4, 5%8, 6%8])

testRenderNote :: Assertion
testRenderNote =
  (toLazyByteString . stringEncoding) "c'64"  @=? (toLazyByteString . renderNote') (Note' (Pitch C (Octave 0)) (Rhythm (1%64)) Set.empty)
  
testRenderAccentedNote :: Assertion
testRenderAccentedNote =
  (toLazyByteString . stringEncoding) "d'32\\verysoft"  @=? (toLazyByteString . renderNote') (Note' (Pitch D (Octave 0)) (Rhythm (1%32)) (Set.singleton (AccentControl' VerySoft)))

testRenderRest :: Assertion
testRenderRest =
  (toLazyByteString . stringEncoding) "r64"  @=? (toLazyByteString . renderNote') (Rest' (Rhythm (1%64)) Set.empty)
  
testRenderPercussionNote :: Assertion
testRenderPercussionNote =
  (toLazyByteString . stringEncoding) "c64"  @=? (toLazyByteString . renderNote') (PercussionNote' (Rhythm (1%64)) Set.empty)
  
testRenderAccentedPercussionNote :: Assertion
testRenderAccentedPercussionNote =
  (toLazyByteString . stringEncoding) "c32\\hard"  @=? (toLazyByteString . renderNote') (PercussionNote' (Rhythm (1%32)) (Set.singleton (AccentControl' Hard)))

testRenderTiedNote :: Assertion
testRenderTiedNote =
  (toLazyByteString . stringEncoding) "c'1~ c'4" @=? (toLazyByteString . renderNote') (Note' (Pitch C (Octave 0)) (Rhythm (5%4)) Set.empty)

testRenderNotes :: Assertion
testRenderNotes =
  (toLazyByteString . stringEncoding) "c8 d16 e32 f64 g128 a64 b32" @=? (toLazyByteString . renderNotes') (zipWith (\pc dur -> Note' (Pitch pc (Octave (-1))) (Rhythm dur) Set.empty) (ascendingScale (majorScale C)) [1%8, 1%16, 1%32, 1%64, 1%128, 1%64, 1%32])

