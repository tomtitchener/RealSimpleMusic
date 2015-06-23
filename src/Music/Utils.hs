module Music.Utils where

import           Control.Error.Safe
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import qualified Data.Vector as DV
import           Music.Data

-- | Answer a subrange, or slice, from an array
--   expressed as range [from-to], inclusive
slice :: Int -> Int -> [a] -> [a]
slice from to =
  DV.toList . DV.slice from (to - from + 1) . DV.fromList

rotate :: Int -> [a] -> [a]
rotate x xs =
  drop x' xs ++ take x' xs
  where
    x' = x `mod` length xs

-- | Cycle extending through a count of two flats and sharps.
cycleOfFifths :: [PitchClass]
cycleOfFifths = [(minBound::PitchClass)..(maxBound::PitchClass)]

-- | Given PitchClass enum is ordered by fifths, count of flats vs. sharps maps directly from number of named pitches (7)
--   and enum value in range.
--                                                  <-- flat                      sharp -->
--     0   1   2   3   4   5   6   7   8   9  10  11  12  13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
--   [-2, -2, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2]  
isSharp :: PitchClass -> Bool
isSharp pc =
  fromEnum pc < 14
  
isFlat :: PitchClass -> Bool
isFlat pc =
  fromEnum pc > 20

fifthsDistance :: PitchClass -> PitchClass -> Int
fifthsDistance pc1 pc2 =
  abs $ idx2 - idx1
  where
    idx1 = fromEnum pc1
    idx2 = fromEnum pc2

-- | Offset from start of cycle of fifths for pitch class
--   from which you can form a major scale while remaining
--   within the range of pitch classes in a uniform cycle of
--   fifths, i.e. one with the same count of flats and sharps
--   in either direction (2, above).  Try to form a major scale
--   from a pitch class at an index smaller than this and you
--   wind up with a scale with n+1 flats, or more exactly, with
--   an index out of the range of pitch classes in the cycle of
--   fifths.
lowestMajorScaleOffset :: Int
lowestMajorScaleOffset  = 1

-- | Offset from the end of cycle of fifths for pitch class
--   for which you can form a major scale (see above).  Try
--   to form a major scale from a pitch class index larger
--   than this and you wind up with a n+1 sharps, or more
--   exactly, with an index out of the range of pitch classes
--   in the cycle of fifths.
highestMajorScaleOffset :: Int
highestMajorScaleOffset = 5

lowestMinorScaleOffset :: Int
lowestMinorScaleOffset  = 4

highestMinorScaleOffset :: Int
highestMinorScaleOffset = 2

lowestMajorScalePitchClass :: PitchClass
lowestMajorScalePitchClass = cycleOfFifths !! lowestMajorScaleOffset

highestMajorScalePitchClass :: PitchClass
highestMajorScalePitchClass = cycleOfFifths !! (length cycleOfFifths - highestMajorScaleOffset - 1)

lowestMinorScalePitchClass :: PitchClass
lowestMinorScalePitchClass = cycleOfFifths !! lowestMinorScaleOffset

highestMinorScalePitchClass :: PitchClass
highestMinorScalePitchClass = cycleOfFifths !! (length cycleOfFifths - highestMinorScaleOffset - 1)

pitchClass2MaybeCycleOfFifthsIndex :: PitchClass -> Int -> Int -> Maybe Int
pitchClass2MaybeCycleOfFifthsIndex tonic low high =
  elemIndex tonic cycleOfFifths >>= testIdx
  where
    testIdx idx = if idx - low < 0 || idx + high >= length cycleOfFifths then Nothing else Just idx
    
pitchClass2MaybeCycleOfFifthsMajorScaleIndex :: PitchClass -> Maybe Int
pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic =
  pitchClass2MaybeCycleOfFifthsIndex tonic lowestMajorScaleOffset highestMajorScaleOffset
  
pitchClass2MaybeCycleOfFifthsMinorScaleIndex :: PitchClass -> Maybe Int
pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic =
  pitchClass2MaybeCycleOfFifthsIndex tonic lowestMinorScaleOffset highestMinorScaleOffset

testScaleTonicErr :: Int -> Int -> PitchClass -> String -> PitchClass -> PitchClass -> Either String Int
testScaleTonicErr low high tonic name lowpc highpc =
  let
    elemerrmsg  = "pitchClass2CycleOfFifthsIndexErr no pitch class " ++ show tonic ++ " in " ++ show cycleOfFifths
    rangerrmsg  = name ++ " scale tonic " ++ show tonic ++ " out of range " ++ show  lowpc ++ " to " ++ show highpc ++ " in " ++ show cycleOfFifths
    testIdx idx = if idx - low < 0 || idx + high >= length cycleOfFifths then Left rangerrmsg else Right idx
  in
    justErr elemerrmsg (elemIndex tonic cycleOfFifths) >>= testIdx 

testMajorScaleTonicErr :: PitchClass -> String -> PitchClass -> PitchClass -> Either String Int
testMajorScaleTonicErr = testScaleTonicErr lowestMajorScaleOffset highestMajorScaleOffset

testMinorScaleTonicErr :: PitchClass -> String -> PitchClass -> PitchClass -> Either String Int
testMinorScaleTonicErr = testScaleTonicErr lowestMinorScaleOffset highestMinorScaleOffset

findAdjByFifths :: PitchClass -> [PitchClass]  -> Either String PitchClass
findAdjByFifths pc pcs =
  let 
    prs      = zip pcs $ map (fifthsDistance pc) pcs
    sorted   = sortBy (\(_, d1) (_, d2) -> compare d1 d2) prs
    emptyerr = "findAdjByFifths empty pcs " ++ show pcs ++ " or sorted " ++ show sorted
    uniquerr = "findAdjByFifths single item pcs " ++ show pcs ++ " or sorted " ++ show sorted
  in
    do
      (fstPc,fstDst) <- headErr emptyerr sorted
      (sndPc,sndDst) <- atErr uniquerr sorted 1
      return $ if fstDst < sndDst || (isFlat pc && isFlat fstPc) then fstPc else sndPc

-- | Construct list of ascending chromatic scale in enharmonic equivalents from PitchClass, e.g. this
--  [[Dff,C,Bs],[Df,Cs,Bss],[Eff,D,Css],[Fff,Ef,Ds],[Ff,E,Dss],[Gff,F,Es],[Gf,Fs,Ess],[Aff,G,Fss],[Af,Gs],[Bff,A,Gss],[Cff,Bf,As],[Cf,B,Ass]]
enhChromPitchClasses :: [[PitchClass]]
enhChromPitchClasses = (map . map) snd pairs
  where
    pairs = groupBy (\ (x1, _) (x2, _) -> x1 == x2) $
                            sortBy (\ (x1, _) (x2, _) -> x1 `compare` x2) $
                                           map (\pc -> (pitchClassToEnhIdx pc,pc)) [(minBound::PitchClass)..(maxBound::PitchClass)]

transposeByAdjFifths :: PitchClass -> Interval -> Either String PitchClass
transposeByAdjFifths pc interval =
  let
    idx1   = fromInteger $ pitchClassToEnhIdx pc
    idx2   = (idx1 + interval) `mod` length enhChromPitchClasses
    errmsg = "transposeByAdjFifths index " ++ show idx2 ++ "out of range for list " ++ show enhChromPitchClasses
  in
    atErr errmsg enhChromPitchClasses idx2 >>= findAdjByFifths pc
      
-- | Given a starting pitch class (tonic), a list of ascending chromatic
--   intervals, and a list of descending chromatic intervals, answer a
--   scale by successively transposing the intervals, picking among
--   enharmonic equivalents the target closest by the cycle of fifths.
scaleFromEnhChromaticScale :: PitchClass -> [Int] -> [Int] -> Either String Scale
scaleFromEnhChromaticScale tonic ups downs =
  let  
    accUp scale int = transposeByAdjFifths (last scale) int >>= \pc -> return $ scale ++ [pc]
    accDown scale int = transposeByAdjFifths (last scale) int >>= \pc -> return $ scale ++ [pc]
  in
    do
      ups'   <- foldM accUp [tonic] ups
      downs' <- foldM accDown [tonic] downs
      return $ Scale ups' downs'
      
genScale :: PitchClass -> String -> [Int] -> [Int] -> (PitchClass -> String -> PitchClass -> PitchClass -> Either String Int) -> PitchClass -> PitchClass -> Either String Scale
genScale tonic name up down testTonic low high =
  testTonic tonic name low high >> scaleFromEnhChromaticScale tonic up down >>= \scale -> return scale
                                                                                           
-- | Given a pitch class answer the major scale, up to two accidentals.
majorScale :: PitchClass -> Either String Scale
majorScale tonic =
  genScale tonic "major" ups downs test low high
  where
    ups   = [2,2,1,2,2,2]
    downs = [-1,-2,-2,-2,-1,-2]
    test  = testMajorScaleTonicErr
    low   = lowestMajorScalePitchClass 
    high  = highestMajorScalePitchClass

-- | Refactored 
commonMinorScale :: String -> [Int] -> PitchClass -> Either String Scale
commonMinorScale name downs tonic =
  genScale tonic name ups downs test low high
  where
    ups  = [2,1,2,2,1,2]
    test = testMinorScaleTonicErr
    low  = lowestMinorScalePitchClass
    high = highestMinorScalePitchClass
    
-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScale :: PitchClass -> Either String Scale
naturalMinorScale = commonMinorScale "natural minor" [-2,-2,-1,-2,-2,-1]

-- | Given a pitch class answer the melodic minor scale, up to two accidentals.
melodicMinorScale :: PitchClass -> Either String Scale
melodicMinorScale = commonMinorScale "melodic minor" [-2,-2,-1,-2,-2,-1]

-- | Given a scale, an interval, and an indexed pitch, 
--   answer a new indexed pitch interval steps away from
--   the old indexed pitch.
transposeIndexedPitch :: Scale -> Interval -> IndexedPitch -> IndexedPitch
transposeIndexedPitch scale interval (IndexedPitch ix (Octave oct)) =
  IndexedPitch ix' (Octave (oct' + oct))
  where
    len  = if ix > 0 then length (ascendingScale scale) else length (descendingScale scale)
    ix'  = (interval + ix) `mod` len
    oct' = (interval + ix) `div` len

transposeIndexedNote :: Scale -> Interval -> IndexedNote -> IndexedNote
transposeIndexedNote _ _ rest@(IndexedRest _ _)                       = rest
transposeIndexedNote _ _ perc@(IndexedPercussionNote _ _)             = perc
transposeIndexedNote scale interval (IndexedNote pitch rhythm cntrls) = IndexedNote (transposeIndexedPitch scale interval pitch) rhythm cntrls

-- | Parse rhythm common to all Notes.
noteToRhythm :: Note -> Rhythm
noteToRhythm (Note _ rhythm _)         = rhythm
noteToRhythm (Rest rhythm _)           = rhythm
noteToRhythm (PercussionNote rhythm _) = rhythm

-- | Insert control common to all Notes
addControlToNote :: Note -> VoiceControl -> Note
addControlToNote (Note pitch rhythm controls)     control = Note pitch rhythm (Set.insert control controls)
addControlToNote (Rest rhythm controls)           control = Rest rhythm (Set.insert control controls)
addControlToNote (PercussionNote rhythm controls) control = PercussionNote rhythm (Set.insert control controls)

-- | Given a list of ascending PitchClass from a scale, find the lowest 
--   PitchClass in the chromatic scale starting at C (or it's enharmonic
--   equivalent).  Used to organize the ascending scale within the range
--   [C..C'] to know when to transition octave boundary, which is absolute
--   based on PitchClass C.
findLowestChromaticIndex :: [PitchClass] -> Int
findLowestChromaticIndex pitches =
  head $ elemIndices lowestIndex chromaticIndices     -- head [2] -> 2 (safe) unsafe if pitches is empty
  where
    chromaticIndices = map pitchClassToEnhIdx pitches -- [Af,Bf,C,Df,Ef,F,G] -> [8,10,0,1,3,5,7]
    lowestIndex = minimum chromaticIndices            -- head [0,1,3,5,7,8,10] -> 0

-- | Put scale in "octave order", i.e. starting with pitch class closest to C,
--   [Af,Bf,C,Df,Ef,F,G] -> [C,Df,Ef,F,G,Af,Bf] or
--   [G,F,Ef,Df,C,Bf,Af] -> [C,Df,Ef,F,G,Af,Bf]
octaveOrder :: [PitchClass] -> [PitchClass]
octaveOrder pitches = rotate (findLowestChromaticIndex pitches) pitches

-- | Given the ascending part or descending list of pitch classes for a scale,
--   an index for a pitch in that scale, and an octave relative to the tonic of
--   that scale, answer the absolute  octave, e.g. for the major scale starting
--   at G and index 3 and relative octave 0, the answer should be Octave 1.
adjustOctave :: [PitchClass] -> Int -> Octave -> Octave
adjustOctave pcs ix (Octave octave) =
  Octave $ ((ix + off) `div` len) + octave
  where
    len   = length pcs
    off   = fromJust $ elemIndex (head pcs) (octaveOrder pcs) -- unsafe if pcs is empty

-- | Given an indexed pitch and a scale, answer a Pitch.
--   Map virtual octave in IndexedPitch to physical octave
--   in Pitch.  Virtual octave in IndexedPitch starts in
--   middle C and above.  Physical octave in Pitch is rooted
--   on middle C.
ixPitchToPitch :: IndexedPitch -> Scale -> Pitch
ixPitchToPitch (IndexedPitch ix (Octave oct)) (Scale up down) 
  | ix >= 0   = let ix' = ix `mod` length up      in Pitch (up !! ix')   (adjustOctave up   ix'    (Octave (oct + (ix `div` length up))))
  | otherwise = let ix' = (-ix) `mod` length down in Pitch (down !! ix') (adjustOctave down (-ix') (Octave (oct - ((-ix) `div` length down))))

indexedNoteToNote :: Scale -> IndexedNote -> Note
indexedNoteToNote scale (IndexedNote ixPitch rhythm controls)   = Note (ixPitchToPitch ixPitch scale) rhythm controls
indexedNoteToNote _     (IndexedRest rhythm controls)           = Rest rhythm controls
indexedNoteToNote _     (IndexedPercussionNote rhythm controls) = PercussionNote rhythm controls

indexedNotesToNotes :: Scale -> [IndexedNote] -> [Note]
indexedNotesToNotes scale = map (indexedNoteToNote scale)
