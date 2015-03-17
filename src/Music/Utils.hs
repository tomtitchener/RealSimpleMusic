
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

-- | Rotate a list, taking element from start
--   and putting it at end, until you reach
--   the target.
rotateTo :: (Ord a, Show a) => a -> [a] -> [a]
rotateTo x xs =
  case elemIndex x xs of
    Nothing -> error $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    Just i  -> rotate i xs
    
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
    (fstPc,fstDst) = head sorted
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

genScale :: PitchClass -> String -> [Int] -> [Int] -> (PitchClass -> Maybe Int) -> PitchClass -> PitchClass -> Scale
genScale tonic name up down genInt low high =
  if isJust $ genInt tonic
  then
    scaleFromEnhChromaticScale tonic up down
  else
    error $ name ++ " scale tonic " ++ show tonic ++ " is out of range " ++ show  low ++ " to " ++ show high ++ " in cycle of fifths " ++ show cycleOfFifths
                  
-- | Given a pitch class answer the major scale, up to two accidentals.
majorScale :: PitchClass -> Scale
majorScale tonic =
  genScale tonic "major" up down genInt low high
  where
    up     = [2,2,1,2,2,2]
    down   = [-1,-2,-2,-2,-1,-2]
    genInt = pitchClass2MaybeCycleOfFifthsMajorScaleIndex
    low    = lowestMajorScalePitchClass 
    high   = highestMajorScalePitchClass

-- | Refactored 
commonMinorScale :: String -> [Int] -> PitchClass -> Scale
commonMinorScale name down tonic =
  genScale tonic name up down genInt low high
  where
    up     = [2,1,2,2,1,2]
    genInt = pitchClass2MaybeCycleOfFifthsMinorScaleIndex
    low    = lowestMinorScalePitchClass
    high   = highestMinorScalePitchClass

-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScale :: PitchClass -> Scale
naturalMinorScale  =
  commonMinorScale "natural minor" down
  where
    down = [-2,-2,-1,-2,-2,-1]

-- | Given a pitch class answer the melodic minor scale, up to two accidentals.
melodicMinorScale :: PitchClass -> Scale
melodicMinorScale =
  commonMinorScale "melodic minor" down
  where
    down = [-2,-2,-1,-2,-2,-1]

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

-- Given the ascending part of a scale, an index for a pitch in that scale,
-- and an octave relative to the tonic of that scale, answer the absolute 
-- octave, e.g. for the major scale starting at G and index 3 and relative
-- octave 0, the answer should be Octave 1.
adjustOctave :: [PitchClass] -> Int -> Octave -> Octave
adjustOctave up ix (Octave octave) =
  Octave $ ((ix + off) `div` len) + octave
  where
    len   = length up
    off   = fromJust $ elemIndex (head up) (sort up)

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
