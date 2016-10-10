module Music.RealSimpleMusic.Music.Utils where

import           Control.Error.Safe
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Set as Set
import           Music.RealSimpleMusic.Music.Data

rotate :: Int -> [a] -> [a]
rotate x xs =
  drop x' xs ++ take x' xs
  where
    x' = x `mod` length xs

chromaticScale :: Scale
chromaticScale = Scale [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] [C, B, Bf, A, Af, G, Gf, F, E, Ef, D, Df]

-- | Cycle extending through a count of two flats and sharps.
cycleOfFifths :: [PitchClass]
cycleOfFifths = [(minBound::PitchClass)..(maxBound::PitchClass)]

-- | Given PitchClass enum is ordered by fifths, count of flats vs. sharps maps directly from number of named pitches (7)
--   and enum value in range.
--                                                  <-- flat                      sharp -->
--     0   1   2   3   4   5   6   7   8   9  10  11  12  13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
--   [-2, -2, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2]

countNamedPitches :: Int
countNamedPitches = 7

isSharp :: PitchClass -> Bool
isSharp pc =
  fromEnum pc < 2 * countNamedPitches
  
isFlat :: PitchClass -> Bool
isFlat pc =
  fromEnum pc > (3 * countNamedPitches) - 1

fifthsDistance :: PitchClass -> PitchClass -> Int
fifthsDistance pc1 pc2 =
  abs $ idx2 - idx1
  where
    idx1 = fromEnum pc1
    idx2 = fromEnum pc2

lowestMajorScalePitchClass :: PitchClass
lowestMajorScalePitchClass = Cff

highestMajorScalePitchClass :: PitchClass
highestMajorScalePitchClass =  Css

lowestMinorScalePitchClass :: PitchClass
lowestMinorScalePitchClass = Aff
 
highestMinorScalePitchClass :: PitchClass
highestMinorScalePitchClass = Ass

pitchClass2MaybeCycleOfFifthsIndex :: PitchClass -> Int -> Int -> Maybe Int
pitchClass2MaybeCycleOfFifthsIndex tonic low high =
  elemIndex tonic cycleOfFifths >>= testIdx
  where
    testIdx idx = if idx - low < 0 || idx + high >= length cycleOfFifths then Nothing else Just idx
    
testScaleTonicErr :: PitchClass -> String -> PitchClass -> PitchClass -> Either String PitchClass
testScaleTonicErr tonic name lowpc highpc =
  if tonic < lowpc || tonic >= highpc then Left rangerrmsg else Right tonic
  where
    rangerrmsg = name ++ " scale tonic " ++ show tonic ++ " out of range " ++ show  lowpc ++ " to " ++ show highpc ++ " in " ++ show cycleOfFifths

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
      
genScale :: PitchClass -> String -> [Int] -> [Int] -> (PitchClass -> String -> PitchClass -> PitchClass -> Either String PitchClass) -> PitchClass -> PitchClass -> Either String Scale
genScale tonic name up down testTonic low high =
  testTonic tonic name low high >> scaleFromEnhChromaticScale tonic up down >>= \scale -> return scale
                                                                                           
-- | Given a pitch class answer the major scale, up to two accidentals.
majorScale :: PitchClass -> Either String Scale
majorScale tonic =
  genScale tonic "major" ups downs test low high
  where
    ups   = [2,2,1,2,2,2]
    downs = [-1,-2,-2,-2,-1,-2]
    test  = testScaleTonicErr
    low   = lowestMajorScalePitchClass 
    high  = highestMajorScalePitchClass

-- | Refactored 
commonMinorScale :: String -> [Int] -> PitchClass -> Either String Scale
commonMinorScale name downs tonic =
  genScale tonic name ups downs test low high
  where
    ups  = [2,1,2,2,1,2]
    test = testScaleTonicErr
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
    len  = length (if ix > 0 then ascendingScale scale else descendingScale scale)
    ix'  = (interval + ix) `mod` len
    oct' = (interval + ix) `div` len

transposeIndexedNote :: Scale -> Interval -> IndexedNote -> IndexedNote
transposeIndexedNote _ _ rest@(IndexedRest _ _)                       = rest
transposeIndexedNote _ _ perc@(IndexedPercussionNote _ _)             = perc
transposeIndexedNote scale interval (IndexedNote pitch rhythm cntrls) = IndexedNote (transposeIndexedPitch scale interval pitch) rhythm cntrls

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
  head $ elemIndices lowestIndex chromaticIndices     -- head [2] -> 2 (safe) unsafe only if pitches is empty
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
