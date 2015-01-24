
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
rotate x xs = drop x xs ++ take x xs

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

equivEnh :: PitchClass -> PitchClass -> Bool
equivEnh pc1 pc2 =
  idx1 == idx2
  where
    idx1 = pitchClass2EnhEquivIdx pc1 equivPitchClasses
    idx2 = pitchClass2EnhEquivIdx pc2 equivPitchClasses

equivEnhIdx :: PitchClass -> [PitchClass] -> Maybe Int
equivEnhIdx pc  = findIndex (equivEnh pc) 

transpose'' :: PitchClass -> Interval -> PitchClass
transpose'' pc int =  -- D 1
  maybe (head pcs) (\idx' -> pcs !! (idx' + 1)) $ findIndex (equivEnh pc) pcs
  where
    idx  = pitchClass2EnhEquivIdx pc enhChromPitchClasses
    pcs  = enhChromPitchClasses !! ((idx + int) `mod` length enhChromPitchClasses)

-- | Cycle extending through a count of two flats and sharps.
cycleOfFifths :: [PitchClass]
cycleOfFifths = [Fff, Cff, Gff, Dff, Aff, Eff, Bff, Ff, Cf, Gf, Df, Af, Ef, Bf, F, C, G, D, A, E, B, Fs, Cs, Gs, Ds, As, Es, Bs, Fss, Css, Gss, Dss, Ass, Ess, Bss]

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

-- | Factor this out so it can be used to filter
--   Arbitrary PitchClass during property testing.
pitchClass2MaybeCycleOfFifthsIndex :: PitchClass -> Int -> Int -> Maybe Int
pitchClass2MaybeCycleOfFifthsIndex tonic low high =
  maybe Nothing testIdx $ elemIndex tonic cycleOfFifths
  where
    testIdx idx = if idx - low < 0 || idx + high >= length cycleOfFifths then Nothing else Just idx

pitchClass2MaybeCycleOfFifthsMajorScaleIndex :: PitchClass -> Maybe Int
pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic =
  pitchClass2MaybeCycleOfFifthsIndex tonic lowestMajorScaleOffset highestMajorScaleOffset
  
pitchClass2MaybeCycleOfFifthsMinorScaleIndex :: PitchClass -> Maybe Int
pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic =
  pitchClass2MaybeCycleOfFifthsIndex tonic lowestMinorScaleOffset highestMinorScaleOffset
                  
majorScaleFromEnhChromaticScale :: PitchClass -> Scale
majorScaleFromEnhChromaticScale tonic =
  if isJust $ pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic
  then
    Scale up down
  else
    error $ "majorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMajorScalePitchClass ++ " to " ++ show highestMajorScalePitchClass ++ "in cycle of fifths " ++ show cycleOfFifths
  where
    ints = [2,2,1,2,2,2]
    up = foldl (\scale int -> let next = transpose'' (last scale) int in scale ++ [next]) [tonic] ints
    down = reverse $ rotate 1 up
    
naturalMinorScaleFromEnhChromaticScale :: PitchClass -> Scale
naturalMinorScaleFromEnhChromaticScale tonic =
  if isJust $ pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic
  then
    Scale up down
  else
    error $ "naturalMinorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMajorScalePitchClass ++ " to " ++ show highestMajorScalePitchClass ++ "in cycle of fifths " ++ show cycleOfFifths
  where
    ints = [2,1,2,2,1,2]
    up = foldl (\scale int -> let next = transpose'' (last scale) int in scale ++ [next]) [tonic] ints
    down = reverse $ rotate 1 up

-- | Given a pitch class answer the major scale, up to two accidentals.
majorScale :: PitchClass -> Scale
majorScale tonic =
  case pitchClass2MaybeCycleOfFifthsMajorScaleIndex tonic of
    Nothing -> error $ "majorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMajorScalePitchClass ++ " to " ++ show highestMajorScalePitchClass ++ "in cycle of fifths " ++ show cycleOfFifths
    Just idx -> Scale ascending $ reverse ascending
      where
        start     = idx - lowestMajorScaleOffset
        stop      = idx + highestMajorScaleOffset
        pcs       = slice start stop cycleOfFifths
        ascending = rotateTo tonic $ sort pcs

-- | Given a pitch class answer the natural minor scale, up to two accidentals.
naturalMinorScale :: PitchClass -> Scale
naturalMinorScale tonic =
  case pitchClass2MaybeCycleOfFifthsMinorScaleIndex tonic of
    Nothing -> error $ "naturalMinorScale tonic " ++ show tonic ++ " is out of range " ++ show  lowestMinorScalePitchClass ++ " to " ++ show highestMinorScalePitchClass ++ "in cycle of fifths " ++ show cycleOfFifths
    Just idx -> Scale ascending $ reverse ascending
      where
        major = majorScale $ cycleOfFifths !! (idx - 3)
        ascending = rotate 5 $ ascendingScale major
  
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
  
