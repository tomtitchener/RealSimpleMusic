-- | Canons to explore RealSimpleMusic

module Canon where

import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Ratio
import           Data.List
import           Data.Maybe
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
                   {title       :: Title
                   ,notes       :: NoteMotto
                   ,distance    :: Rhythm
                   ,instrument  :: Instrument
                   ,voices      :: Int
                   ,repetitions :: Int }
                 deriving (Show)

-- | Render simple canon as a Midi voices score
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
--   Note interval should be capable of a
--   map to to a new key.
data TransposingCanon = TransposingCanon
                        {xpTitle       :: Title
                        ,xpNotes       :: NoteMotto
                        ,xpDistance    :: Rhythm
                        ,xpScale       :: Scale
                        ,xpIntervals   :: [Interval]
                        ,xpInstruments :: [Instrument]
                        ,xpRepetitions :: Int}
                      deriving (Show)

-- | Given a scale, an interval, and a pitch,
--   answer the new Pitch "interval" steps away
--   from "pitch" using "scale".
transposePitch :: Scale -> Interval -> Pitch -> Pitch                          
transposePitch scale interval (Pitch oldPitchClass oldOctave) =
  Pitch (scale !! (target `mod` count)) (Octave (target `div` count))
  where
    count = length scale
    index = fromJust $ elemIndex oldPitchClass scale
    target = index + interval

-- | Given a scale, an interval, and a NoteEvent,
--   answer the new NoteEvent with with transposed Pitch
transposeNoteEvent :: Scale -> Interval -> NoteEvent -> NoteEvent
transposeNoteEvent scale interval (Note pitch rhythm) =
  Note (transposePitch scale interval pitch) rhythm
transposeNoteEvent scale interval (AccentedNote pitch rhythm accent) =
  AccentedNote (transposePitch scale interval pitch) rhythm accent
transposeNoteEvent scale interval (Rest rhythm) =
  Rest rhythm
    
-- | Given a scale, an interval, and a NoteMotto, answer
--   a new NoteMotto with all the Pitches transposed 
transposeNotes :: Scale -> Interval -> [NoteEvent] -> [NoteEvent]
transposeNotes scale interval =
  map (transposeNoteEvent scale interval) 

transposingCanonToScore :: TransposingCanon -> Score
transposingCanonToScore (TransposingCanon title (Motto notes) (Rhythm dist) scale intervals instruments repetitions)
  | length intervals /= length instruments =
      error $ "transposingCanonToScore mismatched intervals " ++ show intervals ++ " vs. instruments " ++ show instruments
  | otherwise =
      midiVoicesScore title sections
      where
        tunes = map (\interval -> concat $ replicate repetitions (transposeNotes scale interval notes)) intervals
        rests = take (length instruments) $ map (Rest . Rhythm) [(0%1)*dist, (1%1)*dist..]
        sections = zipWith3 Section instruments [[t] | t <- zipWith (:) rests tunes] [[]]
