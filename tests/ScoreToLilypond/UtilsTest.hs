module ScoreToLilypond.UtilsTest where

import           Data.ByteString.Builder
import           Data.Either.Combinators (fromRight')
import           Data.Ratio
import qualified Data.Set as Set
import           Music.RealSimpleMusic.Music.Data
import           Music.RealSimpleMusic.Music.Utils
import           Music.RealSimpleMusic.ScoreToLilypond.Utils
import           Test.HUnit

testAccentNames :: Assertion
testAccentNames = 
  fromEnum (maxBound::Accent) @=? length accentValues

testRenderPitchOctaves :: Assertion
testRenderPitchOctaves =
  map (toLazyByteString . stringEncoding) ["c'", "c''", "c'''", "c", "c,", "c,,"] @=? map (toLazyByteString . renderPitch) [Pitch C (Octave octave) | octave <- [0, 1, 2, -1, -2, -3]]

testRenderPitchAccidentals :: Assertion
testRenderPitchAccidentals =
  map (toLazyByteString . stringEncoding) ["c", "ces", "ceses", "cis", "cisis"] @=? map (toLazyByteString . renderPitch) [Pitch pc (Octave (-1)) | pc <- [C, Cf, Cff, Cs, Css]]

testRenderRhythmBase :: Assertion
testRenderRhythmBase =
  (map . map) (toLazyByteString . stringEncoding) [["1"], ["2"], ["4"], ["8"], ["16"], ["32"], ["64"]] @=? map (map toLazyByteString . renderRhythm . fromRight' . mkRhythm) [1%1, 1%2, 1%4, 1%8, 1%16, 1%32, 1%64]

testRenderRhythmDots :: Assertion
testRenderRhythmDots =
  (map . map) (toLazyByteString . stringEncoding) [["1."], ["2."], ["4."], ["8."], ["16."], ["32."], ["64."]] @=? map (map toLazyByteString . renderRhythm . fromRight' . mkRhythm) [3%2, 3%4, 3%8, 3%16, 3%32, 3%64, 3%128]

testRenderRhythmTies :: Assertion
testRenderRhythmTies =
  (map . map) (toLazyByteString . stringEncoding) [["1", "4"], ["1", "2."], ["1", "1", "4"], ["2", "8"], ["2."]] @=? map (map toLazyByteString . renderRhythm . fromRight' . mkRhythm) [5%4, 7%4, 9%4, 5%8, 6%8]

renderNote' :: Note -> Builder
renderNote' note = renderedNote where (_, _, renderedNote) = renderNote (False, False) note

testRenderNote :: Assertion
testRenderNote =
  (toLazyByteString . stringEncoding) "c'64"  @=? (toLazyByteString . renderNote') (Note (Pitch C (Octave 0)) ((fromRight' . mkRhythm) (1%64)) Set.empty)
  
testRenderAccentedNote :: Assertion
testRenderAccentedNote =
  (toLazyByteString . stringEncoding) "d'32\\verysoft"  @=? (toLazyByteString . renderNote') (Note (Pitch D (Octave 0)) ((fromRight'. mkRhythm) (1%32)) (Set.singleton (AccentControl VerySoft)))

testRenderRest :: Assertion
testRenderRest =
  (toLazyByteString . stringEncoding) "r64"  @=? (toLazyByteString . renderNote') (Rest (fromRight' (mkRhythm (1%64))) Set.empty)
  
testRenderPercussionNote :: Assertion
testRenderPercussionNote =
  (toLazyByteString . stringEncoding) "c64"  @=? (toLazyByteString . renderNote') (PercussionNote (fromRight' (mkRhythm (1%64))) Set.empty)
  
testRenderAccentedPercussionNote :: Assertion
testRenderAccentedPercussionNote =
  (toLazyByteString . stringEncoding) "c32\\hard"  @=? (toLazyByteString . renderNote') (PercussionNote (fromRight' (mkRhythm (1%32))) (Set.singleton (AccentControl Hard)))

testRenderTiedNote :: Assertion
testRenderTiedNote =
  (toLazyByteString . stringEncoding) "c'1~ c'4" @=? (toLazyByteString . renderNote') (Note (Pitch C (Octave 0)) (fromRight' (mkRhythm (5%4))) Set.empty)

testRenderNotes :: Assertion
testRenderNotes =
  (toLazyByteString . stringEncoding) "c8 d16 e32 f64 g128 a64 b32" @=? (toLazyByteString . renderNotes) (zipWith (\pc dur -> Note (Pitch pc (Octave (-1))) (fromRight' (mkRhythm dur)) Set.empty) (ascendingScale (fromRight' (majorScale C))) [1%8, 1%16, 1%32, 1%64, 1%128, 1%64, 1%32])

-- Fractional Dynamic Voice.  Mix of 
-- a) tied whole notes of varying durations with fractional dynamics that include crescendo and decrescendo
-- b) quarter note with discrete dyanmic
-- c) tied whole notes of varying durations with fractional dynamics that consist of a series of discrete dynamics
-- d) instrument should be a sustained instrument

-- TBD: when units add up to 3 things go badly wrong, e.g. "s3" in Lilypond, where the duration has to be power of 2.

d1, d2, d3, d4 :: Dynamic
d1 = FractionalDynamic [(Crescendo,1),(Fortissimo,0),(Decrescendo,1),(Piano,0)] -- must not be first dynamic, needs dynamic to start crescendo
d2 = FractionalDynamic [(Fortissimo,1),(Piano,1),(MezzoForte,1),(MezzoPiano,1)]
d3 = DiscreteDynamic MezzoPiano
d4 = DiscreteDynamic MezzoForte

p1, p2, p3, p4 :: Pitch
p1 = Pitch C (Octave 0)
p2 = Pitch A (Octave 0)
p3 = Pitch D (Octave (-1))
p4 = Pitch G (Octave (-2))

rhythm :: Rhythm
rhythm = fromRight' (mkRhythm (4%1))

np1d1, np1d3, np1d2, np2d1, np2d2, np2d3, np2d4, np3d3, np3d2, np3d4, np4d1, np4d2, np4d4 :: Note
np1d1 = Note p1 rhythm (Set.singleton (DynamicControl d1))  
np1d3 = Note p1 rhythm (Set.singleton (DynamicControl d3))
np1d2 = Note p1 rhythm (Set.singleton (DynamicControl d2))
np2d1 = Note p2 rhythm (Set.singleton (DynamicControl d1))
np2d2 = Note p2 rhythm (Set.singleton (DynamicControl d2))
np2d3 = Note p2 rhythm (Set.singleton (DynamicControl d3))
np2d4 = Note p2 rhythm (Set.singleton (DynamicControl d4))
np3d3 = Note p3 rhythm (Set.singleton (DynamicControl d3))
np3d4 = Note p3 rhythm (Set.singleton (DynamicControl d4))
np3d2 = Note p3 rhythm (Set.singleton (DynamicControl d2))
np4d4 = Note p4 rhythm (Set.singleton (DynamicControl d4))
np4d1 = Note p4 rhythm (Set.singleton (DynamicControl d1))
np4d2 = Note p4 rhythm (Set.singleton (DynamicControl d2))
  
-- Fractional Dynamics:  validate by inspection of rendered results.
-- For Midi, open in editor and check alignment of dynamic events.
-- For Lilypond view score and verify alignment of dynamic events.
genFractDyn :: Score
genFractDyn =
  Score "GenFractDynTest" "Test" controls voices
  where
    controls = ScoreControls (KeySignature 0) (TimeSignature 4 4) [(Tempo (TempoVal Quarter 120), fromRight' (mkRhythm (0%4)))]
    voices = [
        Voice (Instrument "Trombone") [np1d3,np1d1,np1d2]
      , Voice (Instrument "Trombone") [np2d3,np2d2,np3d4]
      , Voice (Instrument "Trombone") [np3d3,np3d2,np4d2]
      , Voice (Instrument "Trombone") [np4d4,np4d1,np2d4]]
