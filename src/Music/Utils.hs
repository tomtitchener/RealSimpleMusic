
module Music.Utils where

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

findAdjByFifths :: PitchClass -> [PitchClass] -> PitchClass
findAdjByFifths pc pcs =
  if fstDst < sndDst || (isFlat pc && isFlat fstPc) then fstPc else sndPc
  where
    prs            = zip pcs $ map (fifthsDistance pc) pcs
    sorted         = sortBy (\(_, d1) (_, d2) -> compare d1 d2) prs
    (fstPc,fstDst) = head sorted
    (sndPc,sndDst) = sorted !! 1

transposeByAdjFifths :: PitchClass -> Interval -> PitchClass
transposeByAdjFifths pc interval =
  findAdjByFifths pc enhPcs
  where
    idx1   = pitchClass2EnhEquivIdx pc
    idx2   = (idx1 + interval) `mod` length enhChromPitchClasses
    enhPcs = enhChromPitchClasses !! idx2

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

-- | Given a starting pitch class (tonic), a list of ascending chromatic
--   intervals, and a list of descending chromatic intervals, answer a
--   scale by successively transposing the intervals, picking among
--   enharmonic equivalents the target closest by the cycle of fifths.
scaleFromEnhChromaticScale :: PitchClass -> [Int] -> [Int] -> Scale
scaleFromEnhChromaticScale tonic up down =
  Scale up' down'
  where
    accUp scale int = scale ++ [transposeByAdjFifths (last scale) int]
    accDown scale int = scale ++ [transposeByAdjFifths (last scale) int]
    up' = foldl accUp [tonic] up
    down' = foldl accDown [tonic] down

genScale :: PitchClass -> String -> [Int] -> [Int] -> (PitchClass -> Maybe Int) -> PitchClass -> PitchClass -> Either String Scale
genScale tonic name up down genInt low high =
  maybe (Left message) (const $ Right success) (genInt tonic)
  where
    message = name ++ " scale tonic " ++ show tonic ++ " is out of range " ++ show  low ++ " to " ++ show high ++ " in cycle of fifths " ++ show cycleOfFifths
    success = scaleFromEnhChromaticScale tonic up down
  
--  if isJust $ genInt tonic
--  then
--    scaleFromEnhChromaticScale tonic up down
--  else
--    error $ name ++ " scale tonic " ++ show tonic ++ " is out of range " ++ show  low ++ " to " ++ show high ++ " in cycle of fifths " ++ show cycleOfFifths
                  
-- | Given a pitch class answer the major scale, up to two accidentals.
majorScale :: PitchClass -> Either String Scale
majorScale tonic =
  genScale tonic "major" up down genInt low high
  where
    up     = [2,2,1,2,2,2]
    down   = [-1,-2,-2,-2,-1,-2]
    genInt = pitchClass2MaybeCycleOfFifthsMajorScaleIndex
    low    = lowestMajorScalePitchClass 
    high   = highestMajorScalePitchClass

-- | Refactored 
commonMinorScale :: String -> [Int] -> PitchClass -> Either String Scale
commonMinorScale name down tonic =
  genScale tonic name up down genInt low high
  where
    up     = [2,1,2,2,1,2]
    genInt = pitchClass2MaybeCycleOfFifthsMinorScaleIndex
    low    = lowestMinorScalePitchClass
    high   = highestMinorScalePitchClass

-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScale :: PitchClass -> Either String Scale
naturalMinorScale  =
  commonMinorScale "natural minor" down
  where
    down = [-2,-2,-1,-2,-2,-1]

-- | Given a pitch class answer the melodic minor scale, up to two accidentals.
melodicMinorScale :: PitchClass -> Either String Scale
melodicMinorScale =
  commonMinorScale "melodic minor" down
  where
    down = [-2,-2,-1,-2,-2,-1]

-- | Given a list of ascending PitchClass from a scale, find the lowest 
--   PitchClass in the chromatic scale starting at C (or it's enharmonic
--   equivalent).  Used to organize the ascending scale within the range
--   [C..C'] to know when to transition octave boundary, which is absolute
--   based on PitchClass C.
findLowestChromaticIndex :: [PitchClass] -> Int
findLowestChromaticIndex pitches =
  head $ elemIndices lowestIndex chromaticIndices -- head [2] -> 2 (safe)
  where
    chromaticIndices = map pitchClass2EnhEquivIdx pitches -- [Af,Bf,C,Df,Ef,F,G] -> [8,10,0,1,3,5,7]
    lowestIndex = head $ sort chromaticIndices            -- head [0,1,3,5,7,8,10] -> 0
    
octaveOrder :: [PitchClass] -> [PitchClass]
octaveOrder pitches = rotate (findLowestChromaticIndex pitches) pitches

-- | Given a scale, an interval, and a pitch, answer
--   a new pitch interval steps away from the old pitch.
transposePitch :: Scale -> Interval -> Pitch -> Pitch                          
transposePitch scale interval
  | interval >= 0 = transpose' (ascendingScale scale) 
  | otherwise     = transpose' (reverse (descendingScale scale)) 
  where
    transpose' pcs (Pitch pc (Octave oct)) =
      case elemIndex pc pcs of
        Nothing -> error $ "transposePitch scale " ++ show pcs ++ " does not contain pitch class " ++ show pc
        Just idx -> Pitch pc' (Octave oct')
          where
            len  = length pcs
            pc'  = pcs !! ((idx + interval) `mod` len)
            idx' = fromJust $ elemIndex pc (octaveOrder pcs)
            oct' = oct + ((idx' + interval) `div` len)
        
-- | Given a scale, an interval, and an octave answer 
--   the Pitch "interval" steps frome the first note of
--   "scale" at "octave".  Used to map a list of intervals
--   to a list of pitches given the same scale and starting
--   octave.    
getPitch :: Scale -> Octave -> Interval -> Pitch
getPitch scale octave step =
  transposePitch scale step $ Pitch (head (ascendingScale scale)) octave
  
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

-- | Given a scale, an interval, and a Note,
--   answer the new Note with with transposed Pitch
transposeNote :: Scale -> Interval -> Note -> Note
transposeNote scale interval (Note pitch rhythm controls) =
  Note (transposePitch scale interval pitch) rhythm controls
transposeNote _ _ rest@(Rest _ _) = rest
transposeNote _ _ note@(PercussionNote _ _) = note
  
-- | Given a scale, an interval, and a list of Notes, answer
--   a new list of Notes with all the Pitches transposed 
transposeNotes :: Scale -> Interval -> [Note] -> [Note]
transposeNotes scale interval = map (transposeNote scale interval)

-- Problem is with pitch classes in fifths I can't rely on a sort of pitch class 
-- to answer a "C-normal" ordering--it'll answer a "Fifth-normal" ordering instead.
-- What I need is for the scale to be arranged with Bs, C, Dff, Cs, or Df first.

-- Given the ascending part of a scale, an index for a pitch in that scale,
-- and an octave relative to the tonic of that scale, answer the absolute 
-- octave, e.g. for the major scale starting at G and index 3 and relative
-- octave 0, the answer should be Octave 1.
adjustOctave :: [PitchClass] -> Int -> Octave -> Octave
adjustOctave up ix (Octave octave) =
  Octave $ ((ix + off) `div` len) + octave
  where
    len   = length up
    off   = fromJust $ elemIndex (head up) (octaveOrder up)

ixPitchToPitch :: IndexedPitch -> Scale -> Pitch
ixPitchToPitch (IndexedPitch ix octave) scale
  | length up /= length down = error $ "ixPitchToPitch ascending and descending scales of different lengths " ++ show scale
  | ix < 0 || ix > length up = error $ "ixPitchToPitch index " ++ show ix ++ " out of range for scale " ++ show scale
  | otherwise = Pitch (up !! ix) octave'
  where
    up      = ascendingScale scale
    down    = descendingScale scale
    octave' = adjustOctave up ix octave

indexedNoteToNote :: Scale -> IndexedNote -> Note
indexedNoteToNote scale (IndexedNote ixPitch rhythm controls)   = Note (ixPitchToPitch ixPitch scale) rhythm controls
indexedNoteToNote _     (IndexedRest rhythm controls)           = Rest rhythm controls
indexedNoteToNote _     (IndexedPercussionNote rhythm controls) = PercussionNote rhythm controls

indexedNotesToNotes :: Scale -> [IndexedNote] -> [Note]
indexedNotesToNotes scale = map (indexedNoteToNote scale)
