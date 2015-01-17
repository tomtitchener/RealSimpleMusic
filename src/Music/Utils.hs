
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

-- | Rotate a list, taking element from start
--   and putting it at end, until you reach
--   the target.
rotateTo :: (Ord a, Show a) => a -> [a] -> [a]
rotateTo x xs =
  rotateTo' (elemIndex x xs)
  where
    rotateTo' Nothing = error $ "rotateTo element " ++ show x ++ " is not in list " ++ show xs
    rotateTo' (Just n) = drop n xs ++ take n xs

-- | Cycle extending through a count of one flat or sharp only.
--   Trivially extend through double sharps and flats by adding
--   a second set of fifths on either end, with their pitch
--   classes
cycleOfFifths :: [PitchClass]
cycleOfFifths = [Ff, Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As, Es, Bs]

lowSingleAccidentalScaleIndex :: Int
lowSingleAccidentalScaleIndex  = 1

highSingleAccidentalScaleIndex :: Int
highSingleAccidentalScaleIndex = 5

-- | Factor this out so it can be used to filter 
--   Arbitrary PitchClass during property testing.
pitchClass2MaybeOneAccidentalScaleIndex :: PitchClass -> Maybe Int
pitchClass2MaybeOneAccidentalScaleIndex tonic =
  case elemIndex tonic cycleOfFifths of
    Nothing -> Nothing
    Just idx -> if idx - lowSingleAccidentalScaleIndex < 0
                   || idx + highSingleAccidentalScaleIndex >= length cycleOfFifths
                then
                  Nothing
                else
                  Just idx

-- | Given a pitch class answer the major scale.
--   Limited to scales with notes with one accidental,
--   i.e. no scales with double sharps or double flats.
majorScale :: PitchClass -> Scale
majorScale tonic =
  case pitchClass2MaybeOneAccidentalScaleIndex tonic of
   Nothing -> error $ "majorScale tonic " ++ show tonic ++ " is out of range " ++ show loFifth ++ " to " ++ show hiFifth
   Just idx -> Scale $ rotateTo tonic . sort $ slice (idx - lowSingleAccidentalScaleIndex) (idx + highSingleAccidentalScaleIndex) cycleOfFifths
  where
    lenFifths = length cycleOfFifths
    loFifth = cycleOfFifths !! lowSingleAccidentalScaleIndex
    hiFifth = cycleOfFifths !! (lenFifths - highSingleAccidentalScaleIndex - 1)

-- | Given a pitch class answer the natural minor scale
naturalMinorScale :: PitchClass -> Scale
naturalMinorScale tonic =
  naturalMinorScale' (elemIndex tonic cycleOfFifths)
  where
    naturalMinorScale' Nothing = error $ "naturalMinorScale tonic " ++ show tonic ++ " not found in cycle of fiths " ++ show cycleOfFifths
    naturalMinorScale' (Just idx)
      | idx - lo < 0 || idx + hi >= lenFifths = error $ "naturalMinorScale tonic " ++ show tonic  ++ " is out of range " ++ show loFifth ++ " to " ++ show hiFifth
      | otherwise = Scale $ drop 5 (getScale major) ++ take 5 (getScale major)
      where
        major = majorScale $ cycleOfFifths !! (idx - 3)
        lenFifths = length cycleOfFifths
        lo = 4
        hi = 2 
        loFifth = cycleOfFifths !! lo
        hiFifth = cycleOfFifths !! (lenFifths - hi - 1)
        
-- | Given a scale, an interval, and a pitch, answer
--   a new pitch interval steps away from the old pitch
transposePitch :: Scale -> Interval -> Pitch -> Pitch                          
transposePitch scale interval (Pitch pc (Octave oct)) =
  let scale' = getScale scale in
    case elemIndex pc scale' of
      Nothing -> error $ "transposePitch scale " ++ show scale ++ " does not contain pitch class " ++ show pc
      Just idx -> Pitch pc' (Octave oct')
        where
          len  = length scale'
          pc'  = scale' !! ((idx + interval) `mod` len)
          idx' = fromJust $ elemIndex pc (sort scale')
          oct' = oct + ((idx' + interval) `div` len)
      
-- | Given a scale, an interval, and an octave answer 
--   the Pitch "interval" steps frome the first note of
--   "scale" at "octave".  Used to map a list of intervals
--   to a list of pitches given the same scale and starting
--   octave.    
getPitch :: Scale -> Octave -> Interval -> Pitch
getPitch scale octave step =
  transposePitch scale step $ Pitch (head (getScale scale)) octave
  
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
  
