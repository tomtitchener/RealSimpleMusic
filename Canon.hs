-- | Canons to explore RealSimpleMusic

module Canon where

import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Ratio
import           Score
import           ScoreToMidi
import qualified Sound.MIDI.File.Save as SaveFile

-- | First, simplest of all Canons.
--   Imitation at unison, all voices
--   playing the same instrument,
--   parameterized by
--   title, tune, imitative distance,
--   instrument, count of voices,
--   count of repetitions.
data SimpleCanon = SimpleCanon
                   {sTitle       :: Title
                   ,sNotes       :: NoteMotto
                   ,sDistance    :: Rhythm
                   ,sInstrument  :: Instrument
                   ,sVoices      :: Int
                   ,sRepetitions :: Int }
                 deriving (Show)

-- | Render a simple canon as a Midi voices score
--  (no percussion).  Count of voices less than
--  Midi max (16).  Skip standard statement of
--  theme in unison voices before imitation.
simpleCanonToScore :: SimpleCanon -> Score
simpleCanonToScore (SimpleCanon title (Motto notes) (Rhythm dist) instrument voices repetitions) =
  midiVoicesScore title sections
  where
    tune = concat $ replicate repetitions notes
    rests = take voices $ map (Rest . Rhythm) [(0%1)*dist, (1%1)*dist..]
    sections = [Section instrument [rest : tune] [] | rest <- rests]

-- | Next, parameterize additionally by
--   imitative interval, list of instruments
data TransposingCanon = TransposingCanon
                        {xpTitle       :: Title
                        ,xpNotes       :: NoteMotto
                        ,xpDistance    :: Rhythm
                        ,xpScale       :: Scale
                        ,xpIntervals   :: [Interval]
                        ,xpInstruments :: [Instrument]
                        ,xpRepetitions :: Int}
                      deriving (Show)

-- | Refactor for common behavior.
assembleSections :: Rhythm -> Int -> [[NoteEvent]] -> [Instrument] -> [Section]
assembleSections (Rhythm dist) repetitions tuness instruments =
  zipWith makeSection instruments restTuness
  where
    makeSection instrument tune = Section instrument [tune] []
    rests = map (Rest . Rhythm) [(0%1)*dist, (1%1)*dist..]
    countVoices = length instruments
    repTuness =  map (concat . replicate repetitions) tuness
    restTuness = zipWith (:) rests repTuness

-- | Render a transposing canon as a Midi voices score (no percussion).
transposingCanonToScore :: TransposingCanon -> Score
transposingCanonToScore (TransposingCanon title noteMotto dist scale intervals instruments repetitions)
  | length intervals /= length instruments =
      error $ "transposingCanonToScore mismatched intervals " ++ show intervals ++ " vs. instruments " ++ show instruments
  | otherwise =
      midiVoicesScore title sections
      where
        xposeTune interval = getMotto (transposeNoteMotto scale interval noteMotto)
        tunes = map xposeTune intervals
        sections = assembleSections dist repetitions tunes instruments

-- | Next, abstract over scale, reduces tune to intervals
--   for projection through scale.
data ScalesCanon = ScalesCanon
                   {scTitle       :: Title
                   ,scIntervals   :: [Interval]
                   ,scRhythms     :: [Rhythm]
                   ,scDistance    :: Rhythm
                   ,scScales      :: [Scale]
                   ,scOctaves     :: [Octave]
                   ,scInstruments :: [Instrument]
                   ,scRepetitions :: Int}
                 deriving (Show)

-- | Render a scales canon as a Midi voices score (no percussion).
scalesCanonToScore :: ScalesCanon -> Score
scalesCanonToScore (ScalesCanon title intervals rhythms dist scales octaves instruments repetitions)
  | length scales /= length instruments =
      error $ "scalesCanonToScore mismatched length of scales " ++ show scales ++ " vs. length of instruments " ++ show instruments
  | length scales /= length octaves =
      error $ "scalesCanonToScore mismatched length of scales " ++ show scales ++ " vs. length of octaves " ++ show octaves
  | length intervals /= length rhythms =
      error $ "scalesCanonToScore mismatched length of intervals " ++ show intervals ++ "vs. lengths of rhythms " ++ show rhythms
  | not (all (== length (head scales)) (map length (tail scales))) = 
      error $ "scalesCanonToScore scales not all equal length" ++ show scales
  | otherwise =
      midiVoicesScore title sections
      where
        genPitches scale octave = map (getPitch scale octave) intervals
        genTune scale octave = zipWith Note (genPitches scale octave) rhythms
        tunes = zipWith genTune scales octaves
        sections = assembleSections dist repetitions tunes instruments
