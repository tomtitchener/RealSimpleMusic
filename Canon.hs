-- | Canons to explore RealSimpleMusic

module Canon where

import           Control.Applicative
import           Data.Ratio
import           Data.List
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
                   ,sRepetitions :: Int
                   } deriving (Show)

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
    voices = zipWith (\rest pan -> Voice instrument (rest : tune) [[pan]]) rests pans
    incr = getPan (maxBound::Pan) `div` countVoices
    pans = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]

-- | Additionally parameterize by imitative interval, list of instruments
data TransposingCanon = TransposingCanon
                        {xpTitle       :: Title
                        ,xpNotes       :: NoteMotto
                        ,xpDistance    :: Rhythm
                        ,xpScale       :: Scale
                        ,xpIntervals   :: [Interval]
                        ,xpInstruments :: [Instrument]
                        ,xpRepetitions :: Int
                        } deriving (Show)

-- | Refactored for common behavior between transposingCanonToScore and commonCanonToScore.
assembleVoices :: [Rhythm] -> Int -> [[Note]] -> [Instrument] -> [Voice]
assembleVoices rhythms repetitions tuness instruments =
  zipWith3 makeVoice instruments restTuness pans
  where
    makeVoice instrument tunes pan = Voice instrument tunes [[pan]]
    dists = map getRhythm rhythms
    rests = map (Rest . Rhythm) [sum (take x dists) | x <- [0,1..]]
    repTuness =  map (concat . replicate repetitions) tuness
    restTuness = zipWith (:) rests repTuness
    incr = getPan (maxBound::Pan) `div` length instruments
    pans = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]

-- | Render a transposing canon as a Midi voices score (no percussion).
transposingCanonToScore :: TransposingCanon -> Score
transposingCanonToScore (TransposingCanon title noteMotto dist scale intervals instruments repetitions)
  | lenIntervals /= length instruments =
      error $ "transposingCanonToScore mismatched intervals " ++ show intervals ++ " vs. instruments " ++ show instruments
  | otherwise =
      Score title voices
      where
        lenIntervals = length intervals
        xposeTune interval = getMotto (transposeNoteMotto scale interval noteMotto)
        tuness = map xposeTune intervals
        dists = replicate lenIntervals dist
        voices = assembleVoices dists repetitions tuness instruments
  
-- Refactored for common behavior between scalesCanonToScore and rhythmCanonToScore
commonCanonToScore :: Title -> [Interval] -> [Rhythm] -> [Rhythm] -> [Scale] -> [Octave] -> [Instrument] -> Int -> Score
commonCanonToScore title intervals rhythms distances scales octaves instruments repetitions 
  | lenScales /= length instruments || lenScales /= length octaves || lenScales /= length distances =
      error $ "commonCanonToScore mismatched length of scales " ++ show scales ++ " vs. length of instruments " ++ show instruments ++ " vs. length of octaves " ++ show octaves ++ " vs. length of distances " ++ show distances
  | length intervals /= length rhythms =
      error $ "commonCanonToScore mismatched length of intervals " ++ show intervals ++ "vs. lengths of rhythms " ++ show rhythms
  | not (all (== lenHeadScale) lensTailScales) = 
      error $ "commonCanonToScore scales not all equal length" ++ show scales
  | otherwise =
      Score title voices
      where
        lenScales = length scales
        lenHeadScale = length (head scales)
        lensTailScales = map length (tail scales)
        genPitches scale octave = map (getPitch scale octave) intervals
        genTune scale octave = zipWith Note (genPitches scale octave) rhythms
        tuness = zipWith genTune scales octaves
        voices = assembleVoices distances repetitions tuness instruments

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
                   ,scRepetitions :: Int
                   } deriving (Show)

-- | Render a scales canon as a Midi voices score (no percussion).
scalesCanonToScore :: ScalesCanon -> Score
scalesCanonToScore (ScalesCanon title intervals rhythms dist scales octaves instruments repetitions) =
  commonCanonToScore title intervals rhythms dists scales octaves instruments repetitions
  where
    lenScales = length scales
    dists = replicate lenScales dist

-- | Additionally parameterize by imitative distance.
data RhythmCanon = RhythmCanon
                   {rcTitle       :: Title
                   ,rcIntervals   :: [Interval]
                   ,rcRhythms     :: [Rhythm]
                   ,rcDistances   :: [Rhythm]
                   ,rcScales      :: [Scale]
                   ,rcOctaves     :: [Octave]
                   ,rcInstruments :: [Instrument]
                   ,rcRepetitions :: Int
                   } deriving (Show)

rhythmCanonToScore :: RhythmCanon -> Score
rhythmCanonToScore (RhythmCanon title intervals rhythms dists scales octaves instruments repetitions) =
  commonCanonToScore title intervals rhythms dists scales octaves instruments repetitions

-- | Instead of repeating same parameters for each repetition,
--   map each repetition by new parameters.  Simplify name, as
--   all previous canons can be rewritten using this.
data Canon = Canon
             {canTitle        :: Title
             ,canIntervals    :: [[Interval]]
             ,canRhythms      :: [[Rhythm]]
             ,canDistancess   :: [[Rhythm]]
             ,canScaless      :: [[Scale]]
             ,canOctavess     :: [[Octave]]
             ,canInstrumentss :: [[Instrument]]
             } deriving (Show)

notesToRhythm :: [Note] -> Rhythm
notesToRhythm notes = Rhythm total
  where
    rhythms = map noteToRhythm notes
    ratios = map getRhythm rhythms
    total = sum ratios

addInstrumentControlToVoice :: Voice -> Voice
addInstrumentControlToVoice (Voice instrument notes controlss) =
  Voice instrument notes (controlss ++ [[instrumentControl]])
  where
    instrumentControl = InstrumentControl instrument (notesToRhythm notes)
    
appendVoice :: Voice -> Voice -> Voice
appendVoice (Voice i1 ns1 ctrlss1) (Voice i2 ns2 ctrlss2) =
  Voice i1 (ns1 ++ ns2) ctrlss
  where
    instrumentControl = InstrumentControl i2 (notesToRhythm ns2)
    ctrlss = zipWith (++) (sort ctrlss1) (sort (ctrlss2 ++ [[instrumentControl]]))

-- | Collapse a list of scores into one score.
--   Make title of first score title of returned score.
--   Then append voices in each of tail of list of scores
--   to voices in head of list of score. 
scoresToScore :: [Score] -> Score
scoresToScore scores =
  Score title voices
  where
    (Score title vs) = head scores
    vs' = map addInstrumentControlToVoice vs
    vss = map scoreVoices $ tail scores
    voices = foldl (zipWith appendVoice) vs' vss

canonToScore :: Canon -> Score
canonToScore (Canon title intervalss rhythmss distss scaless octavess instrumentss) =
  scoresToScore scores
  where
    scores = getZipList $
             commonCanonToScore title
             <$> ZipList intervalss
             <*> ZipList rhythmss
             <*> ZipList distss
             <*> ZipList scaless
             <*> ZipList octavess
             <*> ZipList instrumentss
             <*> ZipList [1,1..]
