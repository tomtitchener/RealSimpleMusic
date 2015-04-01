-- | Canons to explore RealSimpleMusic

module Canon.Utils where

import           Canon.Data
import           Data.List ()
import qualified Data.Set as Set
import           RealSimpleMusic

-- | Generalized converter for all Canon types to Score.
--   Assumes:  lengths of ascending and descending notes of all [Scale] are the same.
commonCanonToScore ::  Title -> KeySignature -> TimeSignature -> [(Tempo,Rhythm)] -> [[IndexedNote]] -> [Scale] -> [Rhythm] -> [Octave] -> [Instrument] -> Int -> Score
commonCanonToScore title keySignature timeSignature tempos ixNotess scales rhythms octaves instruments repetitions =
  Score title "folk" scoreControl voices
  where
    lenScale     = length $ ascendingScale $ head scales
    notess       = zipWith indexedNotesToNotes scales ixNotess
    intervals    = map ((* lenScale) . getOctave) octaves
    xpNotes      = zipWith3 (\scale interval notes -> map (transposeNote scale interval) notes) scales intervals notess
    tunes        = map (concat . replicate repetitions) xpNotes
    incr         = 127 `div` length instruments
    pans         = map (\i -> PanControl (Pan (incr * i))) [0,1..]
    durs         = map getRhythm rhythms                   -- [2%1, 1%4, 1%8]
    leadDurs     = scanl (+) (head durs) (tail durs)       -- [2%1, 9%4, 19%8]
    leadRests    = map (\dur -> Rest (Rhythm dur) Set.empty) leadDurs
    revDurs      = reverse durs                            -- [1%8, 1%4, 2%1]
    tailDurs     = scanl (+) (head revDurs) (tail revDurs) -- [1%8, 3%8, 19%8]
    tailRests    = map (\dur -> Rest (Rhythm dur) Set.empty) tailDurs
    tunes'       = head tunes : zipWith (:) leadRests (tail tunes)
    tunes''      = reverse $ head rtunes : zipWith (\r t -> t ++ [r]) tailRests (tail rtunes) where rtunes = reverse tunes' -- trailing rests
    tunes'''     = zipWith (\tune pan -> addControlToNote (head tune) pan : tail tune) tunes'' pans -- pans
    voices       = zipWith Voice instruments tunes'''
    scoreControl = ScoreControls keySignature timeSignature tempos

-- | Converting most general Canon to Score is just a call to most general conversion function.
canonToScore :: Canon -> Score
canonToScore (Canon title key time tempos ixNotess scales rhythms octaves instruments repetitions) =
  commonCanonToScore title key time tempos ixNotess scales rhythms octaves instruments repetitions

-- | To convert scales canon to a canon, replicate notes and imitative distances.
scalesCanonToCanon :: ScalesCanon -> Canon
scalesCanonToCanon scalesCanon =
  Canon {
  cTitle          = scTitle scalesCanon
  ,cKeySignature  = scKeySignature scalesCanon
  ,cTimeSignature = scTimeSignature scalesCanon
  ,cTempos        = scTempos scalesCanon           
  ,cIxNotess      = replicate countVoices $ scIxNotes scalesCanon
  ,cScales        = scScales scalesCanon
  ,cDistances     = replicate countVoices $ scDistance scalesCanon
  ,cOctaves       = scOctaves scalesCanon
  ,cInstruments   = scInstruments scalesCanon
  ,cRepetitions   = scRepetitions scalesCanon
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
  scTitle          = sTitle simpleCanon
  ,scKeySignature  = sKeySignature simpleCanon                   
  ,scTimeSignature = sTimeSignature simpleCanon                   
  ,scTempos        = sTempos simpleCanon                   
  ,scIxNotes       = sIxNotes simpleCanon
  ,scScales        = replicate countVoices $ sScale simpleCanon
  ,scDistance      = sDistance simpleCanon
  ,scOctaves       = replicate countVoices (Octave 0)
  ,scInstruments   = replicate countVoices $ sInstrument simpleCanon
  ,scRepetitions   = sRepetitions simpleCanon
  } 
  where
    countVoices = sCountVoices simpleCanon
  
-- | To convert simple canon to score, first convert to canon and then call general conversion function.
simpleCanonToScore :: SimpleCanon -> Score
simpleCanonToScore simpleCanon = canonToScore $ (scalesCanonToCanon . simpleCanonToScalesCanon) simpleCanon
