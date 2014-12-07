-- | Canons to explore RealSimpleMusic

module Canon where

import           Data.Ratio
import           Music
import           MusicToMidi()

-- | Simplest of all Canons.  Imitation at unison, all voices
--   playing the same instrument.  Parameterized by title,
--   tune, imitative distance, instrument, count of voices,
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
simpleCanonToScore (SimpleCanon title (Motto notes) (Rhythm dist) instrument countVoices repetitions) =
  Score title voices
  where
    tune = (concat . replicate repetitions) notes
    rests = take countVoices $ map (Rest . Rhythm) [(0%1)*dist, (1%1)*dist..]
    voices = zipWith (\rest balance -> Voice instrument (rest : tune) [[balance]]) rests balances
    balances = map (\b -> BalanceControl b (Rhythm (0%4))) $ cycle [LeftBalance, MidLeftBalance, CenterBalance, MidRightBalance, RightBalance]

-- | Additionally parameterize by imitative interval, list of instruments
data TransposingCanon = TransposingCanon
                        {xpTitle       :: Title
                        ,xpNotes       :: NoteMotto
                        ,xpDistance    :: Rhythm
                        ,xpScale       :: Scale
                        ,xpIntervals   :: [Interval]
                        ,xpInstruments :: [Instrument]
                        ,xpRepetitions :: Int}
                      deriving (Show)

-- | Refactor for common behavior between transposingCanonToScore and scaleCanonToScore.
assembleVoices :: Rhythm -> Int -> [[Note]] -> [Instrument] -> [Voice]
assembleVoices (Rhythm dist) repetitions tuness instruments =
  zipWith3 makeVoice instruments restTuness balances
  where
    makeVoice instrument tunes balance = Voice instrument tunes [[balance]]
    rests = map (Rest . Rhythm) [(0%1)*dist, (1%1)*dist..]
    repTuness =  map (concat . replicate repetitions) tuness
    restTuness = zipWith (:) rests repTuness
    balances = map (\b -> BalanceControl b (Rhythm (0%4))) $ cycle [LeftBalance, MidLeftBalance, CenterBalance, MidRightBalance, RightBalance]

-- | Render a transposing canon as a Midi voices score (no percussion).
transposingCanonToScore :: TransposingCanon -> Score
transposingCanonToScore (TransposingCanon title noteMotto dist scale intervals instruments repetitions)
  | length intervals /= length instruments =
      error $ "transposingCanonToScore mismatched intervals " ++ show intervals ++ " vs. instruments " ++ show instruments
  | otherwise =
      Score title voices
      where
        xposeTune interval = getMotto (transposeNoteMotto scale interval noteMotto)
        tuness = map xposeTune intervals
        voices = assembleVoices dist repetitions tuness instruments

-- | Additionally parameterize by scale.  Tune becomes lists of rhythms 
--   and of intervals for mapping over scale.  Parameterize by lists 
--   of scale, octave  tranposition for scale root, and instruments
--   per voice.
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
  | lenScales /= length instruments || lenScales /= length octaves =
      error $ "scalesCanonToScore mismatched length of scales " ++ show scales ++ " vs. length of instruments " ++ show instruments ++ " vs. length of octaves " ++ show octaves
  | length intervals /= length rhythms =
      error $ "scalesCanonToScore mismatched length of intervals " ++ show intervals ++ "vs. lengths of rhythms " ++ show rhythms
  | not (all (== lenHeadScale) lensTailScales) = 
      error $ "scalesCanonToScore scales not all equal length" ++ show scales
  | otherwise =
      Score title voices
      where
        lenScales = length scales
        lenHeadScale = length (head scales)
        lensTailScales = map length (tail scales)
        genPitches scale octave = map (getPitch scale octave) intervals
        genTune scale octave = zipWith Note (genPitches scale octave) rhythms
        tuness = zipWith genTune scales octaves
        voices = assembleVoices dist repetitions tuness instruments
