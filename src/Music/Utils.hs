
module Music.Utils where

import           Data.List
import           Data.Maybe
import           Music.Data
import qualified Data.Vector as DV

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

-- | Rotate a list, taking element from start
--   and putting it at end, until you reach
--   the target.
rotateTo :: (Ord a, Show a) => a -> [a] -> [a]
rotateTo x xs =
  case elemIndex x xs of
    Nothing -> error $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    Just i  -> rotate i xs

equivPitchClasses :: [[PitchClass]]
equivPitchClasses = [[Cff, Cf, C, Cs, Css], [Dff, Df, D, Ds, Dss], [Eff, Ef, E, Es, Ess], [Fff, Ff, F, Fs, Fss], [Gff, Gf, G, Gs, Gss], [Aff, Af, A, As, Ass], [Bff, Bf, B, Bs, Bss]]

enhChromPitchClasses :: [[PitchClass]]
enhChromPitchClasses = [[Bs, C, Dff], [Bss, Cs, Df], [Css, D, Eff], [Ds, Ef, Fff], [Dss, E, Ff], [Es, F, Gff], [Ess, Fs, Gf], [Fss, G, Aff], [Gs, Af], [Gss, A, Bff], [As, Bf, Cff], [Ass, B, Cf]]

pitchClass2EnhEquivIdx :: PitchClass -> [[PitchClass]] -> Int
pitchClass2EnhEquivIdx pc pcs =
  fromMaybe
    (error $ "pitchClass2Index no match for PitchClass " ++ show pc ++ " in " ++ show pcs)
    (findIndex (elem pc) pcs)
    
-- | Cycle extending through a count of two flats and sharps.
cycleOfFifths :: [PitchClass]
cycleOfFifths = [Fff, Cff, Gff, Dff, Aff, Eff, Bff, Ff, Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As, Es, Bs, Fss, Css, Gss, Dss, Ass, Ess, Bss]

-- | Count of flats (<0) or sharps (>0) for matching PitchClass in cycleOfFifths.
fifthsEnhDegrees :: [Int]
fifthsEnhDegrees = [-2, -2, -2, -2, -2, -2, -2, -1, -1, -1, -1, -1, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2]  

cycleOfFifthsIdx :: PitchClass -> Int
cycleOfFifthsIdx pc =
  fromMaybe
    (error $ "cycleOfFifths pitch class " ++ show pc ++ " not an elment of cycle of fifths " ++ show cycleOfFifths)
    (elemIndex pc cycleOfFifths)

pitchClass2AccidentalDegree :: PitchClass -> Int
pitchClass2AccidentalDegree pc =
  fifthsEnhDegrees !! cycleOfFifthsIdx pc

isSharp :: PitchClass -> Bool
isSharp pc =
  pitchClass2AccidentalDegree pc  > 0
  
isFlat :: PitchClass -> Bool
isFlat pc =
  pitchClass2AccidentalDegree pc  < 0

fifthsDistance :: PitchClass -> PitchClass -> Int
fifthsDistance pc1 pc2 =
  abs $ idx2 - idx1
  where
    idx1 = cycleOfFifthsIdx pc1
    idx2 = cycleOfFifthsIdx pc2

findAdjByFifths :: PitchClass -> [PitchClass] -> PitchClass
findAdjByFifths pc pcs =
  if fstDst < sndDst || (isFlat pc && isFlat fstPc) then fstPc else sndPc
  where
    prs            = zip pcs $ map (fifthsDistance pc) pcs
    sorted         = sortBy (\(_, d1) (_, d2) -> compare d1 d2) prs
    (fstPc,fstDst) = sorted !! 0
    (sndPc,sndDst) = sorted !! 1

transposeByAdjFifths :: PitchClass -> Interval -> PitchClass
transposeByAdjFifths pc interval =
  findAdjByFifths pc enhPcs
  where
    idx1   = pitchClass2EnhEquivIdx pc enhChromPitchClasses
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

scaleFromEnhChromaticScale :: PitchClass -> [Int] -> [Int] -> Scale
scaleFromEnhChromaticScale tonic up down =
  Scale up' down'
  where
    accUp scale int = scale ++ [transposeByAdjFifths (last scale) int]
    accDown scale int = scale ++ [transposeByAdjFifths (last scale) int]
    up' = foldl accUp [tonic] up
    down' = foldl accDown [tonic] down
                  
-- | Given a pitch class answer the major scale, up to two accidentals.
majorScale :: PitchClass -> Scale
majorScale tonic =
  if isJust $ pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic
  then
    scaleFromEnhChromaticScale tonic ascendingMajorScaleIntervals descendingMajorScaleIntervals
  else
    error $ "majorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMajorScalePitchClass ++ " to " ++ show highestMajorScalePitchClass ++ " in cycle of fifths " ++ show cycleOfFifths
  where
    ascendingMajorScaleIntervals = [2,2,1,2,2,2]
    descendingMajorScaleIntervals = [-1,-2,-2,-2,-1,-2]
    
-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScale :: PitchClass -> Scale
naturalMinorScale tonic =
  if isJust $ pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic
  then
    scaleFromEnhChromaticScale tonic ascendingNaturalMinorScaleIntervals descendingNaturalMinorScaleIntervals
  else
    error $ "naturalMinorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMinorScalePitchClass ++ " to " ++ show highestMinorScalePitchClass ++ " in cycle of fifths " ++ show cycleOfFifths
  where
    ascendingNaturalMinorScaleIntervals = [2,1,2,2,1,2]
    descendingNaturalMinorScaleIntervals = [-2,-2,-1,-2,-2,-1]

-- | Given a pitch class answer the melodic minor scale, up to two accidentals.
melodicMinorScale :: PitchClass -> Scale
melodicMinorScale tonic =
  if isJust $ pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic
  then
    scaleFromEnhChromaticScale tonic ascendingMelodicScaleIntervals descendingMelodicScaleIntervals
  else
    error $ "naturalMinorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMinorScalePitchClass ++ " to " ++ show highestMinorScalePitchClass ++ " in cycle of fifths " ++ show cycleOfFifths
  where
    ascendingMelodicScaleIntervals = [2,1,2,2,2,2]
    descendingMelodicScaleIntervals = [-2,-2,-1,-2,-2,-1]

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
            idx' = fromJust $ elemIndex pc (sort pcs)
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
noteToRhythm (Note _ rhythm)                   = rhythm
noteToRhythm (AccentedNote _ rhythm _)         = rhythm
noteToRhythm (Rest rhythm)                     = rhythm
noteToRhythm (PercussionNote rhythm)           = rhythm
noteToRhythm (AccentedPercussionNote rhythm _) = rhythm

-- | Given a scale, an interval, and a Note,
--   answer the new Note with with transposed Pitch
transposeNote :: Scale -> Interval -> Note -> Note
transposeNote scale interval (Note pitch rhythm) =
  Note (transposePitch scale interval pitch) rhythm
transposeNote scale interval (AccentedNote pitch rhythm accent) =
  AccentedNote (transposePitch scale interval pitch) rhythm accent
transposeNote _ _ (Rest rhythm) =
  Rest rhythm
transposeNote _ _ (PercussionNote rhythm) =
  PercussionNote rhythm
transposeNote _ _ (AccentedPercussionNote rhythm accent) =
  AccentedPercussionNote rhythm accent
    
-- | Given a scale, an interval, and a NoteMotto, answer
--   a new NoteMotto with all the Pitches transposed 
transposeNoteMotto :: Scale -> Interval -> NoteMotto -> NoteMotto
transposeNoteMotto scale interval noteMotto =
  Motto $ map (transposeNote scale interval) (getMotto noteMotto)
