-- | Canons to explore RealSimpleMusic

module Canon.Utils where

import           Canon.Data
import           Data.List ()
import           Data.Ratio
import qualified Data.Set as Set
import           RealSimpleMusic

-- | Generalized converter for all Canon types to Score.
--   Assumes:  lengths of ascending and descending notes of Scale are the same.
--   TBD: get pan settings associated with non-empty values.
--   Generally, eliminate crutch of zero-duration rest.
--   Means adding initial rests only to tail of tunes,
--   adding ending rests only to all tunes except the last.
--   Once that's built, then insert pan control to first
--   rest or note of all voices uniformly.
commonCanonToScore ::  Title -> [[IndexedNote]] -> [Scale] -> [Rhythm] -> [Octave] -> [Instrument] -> Int -> Score
commonCanonToScore title ixNotess scales rhythms octaves instruments repetitions =
  Score title "folk" (Tempo (Rhythm (1%4)) 60) (TimeSignature 4 4) (KeySignature (-1))  voices
  where
    lenScale   = length $ ascendingScale $ head scales
    notess     = zipWith indexedNotesToNotes scales ixNotess
    intervals  = map ((* lenScale) . getOctave) octaves
    xpNotes    = zipWith3 (\scale interval notes -> map (transposeNote scale interval) notes) scales intervals notess
    tunes      = map (concat . replicate repetitions) xpNotes
    incr       = getPan (maxBound::Pan) `div` (length instruments)
    pans       = map (\i -> PanControl (Pan (incr * i))) [0,1..]
    durs       = scanl (+) (getRhythm (head rhythms)) $ map getRhythm (tail rhythms)
    rests      = map (\dur -> (Rest (Rhythm dur) Set.empty)) durs
    tunes'     = head tunes : zipWith (:) rests (tail tunes) -- leading rests
    tunes''    = reverse $ head rtunes : zipWith (\r t -> t ++ [r]) rests (tail rtunes) where rtunes = reverse tunes' -- trailing rests
    tunes'''   = zipWith (\tune pan -> addControlToNote (head tune) pan : tail tune) tunes'' pans -- pans
    voices     = zipWith (\instrument tune -> Voice instrument tune) instruments tunes'''

-- | Converting most general Canon to Score is just a call to most general conversion function.
canonToScore :: Canon -> Score
canonToScore (Canon title ixNotess scales rhythms octaves instruments repetitions) =
  commonCanonToScore title ixNotess scales rhythms octaves instruments repetitions

-- | To convert scales canon to a canon, replicate notes and imitative distances.
scalesCanonToCanon :: ScalesCanon -> Canon
scalesCanonToCanon scalesCanon =
  Canon {
  cTitle         = scTitle scalesCanon
  ,cIxNotess     = replicate countVoices $ scIxNotes scalesCanon
  ,cScales       = scScales scalesCanon
  ,cDistances    = replicate countVoices $ scDistance scalesCanon
  ,cOctaves      = scOctaves scalesCanon
  ,cInstruments  = scInstruments scalesCanon
  ,cRepetitions  = scRepetitions scalesCanon
  } 
  where
    countVoices = length $ scInstruments scalesCanon

-- | To convert scales canon to score, first convert to canon and then call general conversion function.
scalesCanonToScore :: ScalesCanon -> Score
scalesCanonToScore scalesCanon = canonToScore $ scalesCanonToCanon scalesCanon

-- | To convert simple canon to a scales canon, replicate scales, octaves, and instruments.
simpleCanonToScalesCanon :: SimpleCanon -> ScalesCanon
simpleCanonToScalesCanon simpleCanon =
  ScalesCanon {
  scTitle        = sTitle simpleCanon
  ,scIxNotes     = sIxNotes simpleCanon
  ,scScales      = replicate countVoices $ sScale simpleCanon
  ,scDistance    = sDistance simpleCanon
  ,scOctaves     = replicate countVoices (Octave 0)
  ,scInstruments = replicate countVoices $ sInstrument simpleCanon
  ,scRepetitions = sRepetitions simpleCanon
  } 
  where
    countVoices = sCountVoices simpleCanon
  
-- | To convert simple canon to score, first convert to canon and then call general conversion function.
simpleCanonToScore :: SimpleCanon -> Score
simpleCanonToScore simpleCanon = canonToScore $ (scalesCanonToCanon . simpleCanonToScalesCanon) simpleCanon
