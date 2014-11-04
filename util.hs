
module Util where

import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Ratio
import           Score
import           ScoreToMidi
import qualified Sound.MIDI.File.Save as SaveFile

uY :: Title -> String -> IO [[NoteEvent]] -> [[ControlEvent]] -> IO ()
uY title instr notess controlss =
  do
    nss <- notess
    let score = MidiVoicesScore title [Section (Instrument instr) nss controlss]
    LazyByteString.writeFile (title ++ ".mid") (SaveFile.toByteString (scoreToMidiFile score))

uX :: Title -> String -> IO [NoteEvent] -> [ControlEvent] -> IO ()
uX title instr notes controls =
  do
    ns <- notes
    let score = MidiVoicesScore title [Section (Instrument instr) [ns] [controls]]
    LazyByteString.writeFile (title ++ ".mid") (SaveFile.toByteString (scoreToMidiFile score))

u0 :: String -> String -> [NoteEvent] -> [ControlEvent] -> IO ()
u0 title instr notes controls =
  do
    let score = MidiVoicesScore title [Section (Instrument instr) [notes] [controls]]
    LazyByteString.writeFile (title ++ ".mid") (SaveFile.toByteString (scoreToMidiFile score))

u1 :: String -> String -> Pitch -> Rhythm -> IO ()
u1 title instr pitch rhythm =
   u0 title instr [Note pitch rhythm] []

u2 :: String -> Pitch -> Rhythm -> IO ()
u2 = u1 "test"

u3 :: Pitch -> Rhythm -> IO ()
u3 = u2 "Acoustic Grand Piano"

u4 :: Rhythm -> IO ()
u4 = u3 (Pitch C 0)

u5 :: IO ()
u5 = u4 (Rhythm (1%1))

main :: IO ()
main = u5

{--
util :: IO ()
util =
  do
    noteEventss <- return $ [[(Note (Pitch C 0) (Rhyth (1%1)))]]
    voicesSection <- return $ [Section (Instrument "Acoustic Grand Piano") noteEventss [[]]]
    score <- return $ MidiVoicesScore "test" voicesSection
    LazyByteString.writeFile "test.mid" (SaveFile.toByteString (scoreToMidiFile score))
--}


