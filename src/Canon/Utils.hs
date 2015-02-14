-- | Canons to explore RealSimpleMusic

module Canon.Utils where

import           Data.Ratio
import           Data.List
import           RealSimpleMusic
import           Canon.Data

-- Bug:  transpose behavior failing octave threshold.
-- Point to octave transposition is to be absolute.

commonCanonToScore ::  Title -> [IndexedNoteMotto] -> [Scale] -> [Rhythm] -> [Octave] -> [Instrument] -> Int -> Score
commonCanonToScore title mottos scales rhythms octaves instruments repetitions =
  Score title voices
  where
    lenScale  = length $ ascendingScale $ head scales
    notess    = zipWith indexedNotesToNotes scales $ map (\(Motto ixNotes) -> ixNotes) mottos
    intervals = map ((* lenScale) . getOctave) octaves
    xpNotes   = zipWith3 (\interval notes scale -> map (transposeNote scale interval) notes) intervals notess scales
    tunes     = map (concat . replicate repetitions) xpNotes
    numVoices = length instruments
    rests     = map (Rest . Rhythm) $ scanl (+) (0%1) $ map getRhythm rhythms
    incr      = getPan (maxBound::Pan) `div` numVoices
    pans      = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]
    voices    = zipWith4 (\instrument rest pan tune -> Voice instrument (rest : tune) [[pan]]) instruments rests pans tunes

canonToScore :: Canon -> Score
canonToScore (Canon title ixNoteMottos scales rhythms octaves instruments repetitions) =
  commonCanonToScore title ixNoteMottos scales rhythms octaves instruments repetitions

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
  
scalesCanonToScore :: ScalesCanon -> Score
scalesCanonToScore scalesCanon = canonToScore $ scalesCanonToCanon scalesCanon
  
simpleCanonToCanon :: SimpleCanon -> Canon
simpleCanonToCanon simpleCanon =
  Canon {
  cTitle         = sTitle simpleCanon
  ,cIxNoteMottos = replicate countVoices $ sIxNoteMotto simpleCanon
  ,cScales       = replicate countVoices $ sScale simpleCanon
  ,cDistances    = replicate countVoices $ sDistance simpleCanon
  ,cOctaves      = replicate countVoices (Octave 0)
  ,cInstruments  = replicate countVoices $ sInstrument simpleCanon
  ,cRepetitions  = sRepetitions simpleCanon
  } 
  where
    countVoices = sCountVoices simpleCanon

simpleCanonToScore :: SimpleCanon -> Score
simpleCanonToScore simpleCanon = canonToScore $ simpleCanonToCanon simpleCanon

