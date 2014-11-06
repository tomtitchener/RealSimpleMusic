
module Util where

import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Ratio
import           Score
import           ScoreToMidi
import qualified Sound.MIDI.File.Save as SaveFile

-- | Low-level convert for choir of voices and control events to Midi file.
uY :: Title -> String -> IO [[NoteEvent]] -> [[ControlEvent]] -> IO ()
uY title instr notess controlss =
  do
    let score = MidiVoicesScore title [Section (Instrument instr) notes controlss]
    LazyByteString.writeFile (title ++ ".mid") (SaveFile.toByteString (scoreToMidiFile score))

-- | Low-level convert for single voice and control event lists to Midi file.
uX :: Title -> String -> IO [NoteEvent] -> [ControlEvent] -> IO ()
uX title instr notes controls =
  do
    let score = MidiVoicesScore title [Section (Instrument instr) [notes] [controls]]
    LazyByteString.writeFile (title ++ ".mid") (SaveFile.toByteString (scoreToMidiFile score))

-- | Assemble defaults into score with single Section, call
--   scoreToMidiFile to create Sound.MIDI.File.T, then 
--   then Sound.MIDI.File.Save.toByteString to convert
--   to byte string, then Data.ByteString.Lazy.writeFile
--   to save.
u0 :: String -> String -> [NoteEvent] -> [ControlEvent] -> IO ()
u0 title instr notes controls =
  do
    let score = MidiVoicesScore title [Section (Instrument instr) [notes] [controls]]
    LazyByteString.writeFile (title ++ ".mid") (SaveFile.toByteString (scoreToMidiFile score))

-- | Assemble default pitch and rhythm into single-array of single NoteEvent,
--   empty array of ControlEvent.
u1 :: String -> String -> Pitch -> Rhythm -> IO ()
u1 title instr pitch rhythm =
   u0 title instr [Note pitch rhythm] []

-- | Using default rhythm, pitch, and instrument specify default title.
u2 :: String -> Pitch -> Rhythm -> IO ()
u2 = u1 "test"

-- | Using default rhythm and pitch specify default instrument.
u3 :: Pitch -> Rhythm -> IO ()
u3 = u2 "Acoustic Grand Piano"

-- | Using default rhythm specify default pitch.
u4 :: Rhythm -> IO ()
u4 = u3 (Pitch C 0)

-- | Build single pitch single voice example with defaults:
--   Rhythm: whole note (1%1)
--   Pitch:  middle C (Pitch C 0)
--   Instrument:  piano (Acoustic Grand Piano)
--   Title:  test.mid
u5 :: IO ()
u5 = u4 (Rhythm (1%1))

main :: IO ()
main = u5


