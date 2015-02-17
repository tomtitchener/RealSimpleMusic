-- | Canons to explore RealSimpleMusic

module Canon.Utils where

import           Data.Ratio
import           Data.List
import           RealSimpleMusic
import           Canon.Data

-- | Generalized converter for all Canon types to Score.
--   Assumes:  lengths of ascending and descending notes of Scale are the same.
commonCanonToScore ::  Title -> [IndexedNoteMotto] -> [Scale] -> [Rhythm] -> [Octave] -> [Instrument] -> Int -> Score
commonCanonToScore title mottos scales rhythms octaves instruments repetitions =
  Score title voices
  where
    lenScale  = length $ ascendingScale $ head scales
    notess    = zipWith indexedNotesToNotes scales $ map (\(Motto ixNotes) -> ixNotes) mottos
    intervals = map ((* lenScale) . getOctave) octaves
    xpNotes   = zipWith3 (\scale interval notes -> map (transposeNote scale interval) notes) scales intervals notess
    tunes     = map (concat . replicate repetitions) xpNotes
    numVoices = length instruments
    rests     = map (Rest . Rhythm) $ scanl (+) (0%1) $ map getRhythm rhythms
    incr      = getPan (maxBound::Pan) `div` numVoices
    pans      = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]
    voices    = zipWith4 (\instrument rest pan tune -> Voice instrument (rest : tune) [[pan]]) instruments rests pans tunes

-- | Converting most general Canon to Score is just a call to most general conversion function.
canonToScore :: Canon -> Score
canonToScore (Canon title ixNoteMottos scales rhythms octaves instruments repetitions) =
  commonCanonToScore title ixNoteMottos scales rhythms octaves instruments repetitions

-- | To convert scales canon to a canon, replicate notes and imitative distances.
scalesCanonToCanon :: ScalesCanon -> Canon
scalesCanonToCanon scalesCanon =
  Canon {
  cTitle         = scTitle scalesCanon
  ,cIxNoteMottos = replicate countVoices $ scIxNoteMotto scalesCanon
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
  ,scIxNoteMotto = sIxNoteMotto simpleCanon
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
