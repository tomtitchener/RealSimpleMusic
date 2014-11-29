
module Util where

import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Ratio
import           Canon
import           Music
import           MusicToMidi
import qualified Sound.MIDI.File.Save as SaveFile

-- | Assemble defaults into score with single Section, call
--   scoreToMidiFile to create Sound.MIDI.File.T, then 
--   then Sound.MIDI.File.Save.toByteString to convert
--   to byte string, then Data.ByteString.Lazy.writeFile
--   to save.
u0 :: String -> String -> [Note] -> [Control] -> IO ()
u0 title instr notes controls =
  scoreToMidiFiles score
  where
    score = Score title [Voice (Instrument instr) notes [controls]]
        
-- | Assemble default pitch and rhythm into single-array of single Note,
--   empty array of Control.
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

-- main :: IO ()
-- main = u5

-- Frere Jacques
mC = Pitch C 0
mD = Pitch D 0
mE = Pitch E 0
mF = Pitch F 0
mG = Pitch G 0
mA = Pitch A 0
lG = Pitch G (-1)
pt1 = [mC, mD, mE, mC]
pt2 = [mE, mF, mG]
pt3 = [mG, mA, mG, mF, mE, mC]
pt4 = [mC, lG, mC]
fjPitches = pt1 ++ pt1 ++ pt2 ++ pt2 ++ pt3 ++ pt3 ++ pt4 ++ pt4
eig = Rhythm (1%8)
qtr = Rhythm (1%4)
hlf = Rhythm (1%2)
rh1 = [qtr, qtr, qtr, qtr]
rh2 = [qtr, qtr, hlf]
rh3 = [eig, eig, eig, eig, qtr, qtr]
fjRhythms = rh1 ++ rh1 ++ rh2 ++ rh2 ++ rh3 ++ rh3 ++ rh2 ++ rh2
int1 = [0, 1, 2, 0]
int2 = [2, 3, 4]
int3 = [4, 5, 4, 3, 2, 0]
int4 = [0, -3, 0]
fjIntervals = int1 ++ int1 ++ int2 ++ int2 ++ int3 ++ int3 ++ int4 ++ int4
piano = Instrument "Acoustic Grand Piano"
cMaj  = majorScale C
afMaj = majorScale Af
eMaj  = majorScale E
fMaj  = majorScale F
gMaj  = majorScale G
dMaj  = majorScale D
aMaj  = majorScale A

-- writeFJSimpleCanon 4 (2%1)
writeFJSimpleCanon :: Int -> Rational -> IO ()
writeFJSimpleCanon voices dur =
  scoreToMidiFiles score
  where
    title = "Frere Jacques"
    noteMotto = Motto $ zipWith Note fjPitches fjRhythms
    distance = Rhythm dur
    repetitions = 5
    simpleCanon = SimpleCanon title noteMotto distance piano voices repetitions
    score = simpleCanonToScore simpleCanon

-- writeFJTransposingCanon [piano, piano, piano, piano] [5, -3, 7, -6] (1%1)
writeFJTransposingCanon ::[Instrument] -> [Interval] -> Rational -> IO ()
writeFJTransposingCanon instruments intervals dist =
  scoreToMidiFiles score
  where
    title = "Frere Jacques"
    noteMotto = Motto $ zipWith Note fjPitches fjRhythms
    distance = Rhythm dist
    repetitions = 5
    transposingCanon = TransposingCanon title noteMotto distance cMaj intervals instruments repetitions
    score = transposingCanonToScore transposingCanon

-- C grouping major third down, minor third above, with root fifth below.
-- writeFJScalesCanon [piano, piano, piano, piano] [cMaj, afMaj, eMaj, fMaj] [Octave 0, Octave (-1), Octave 0, Octave (-2)] (7%8)

-- Tower of fifths
-- writeFJScalesCanon [piano, piano, piano, piano] [cMaj, gMaj, dMaj, eMaj] [Octave (-1), Octave (-1), Octave 0, Octave 1] (7%8)
writeFJScalesCanon :: [Instrument] -> [Scale] -> [Octave] -> Rational -> IO ()
writeFJScalesCanon instruments scales octaves dist =
  scoreToMidiFiles score
  where
    title = "Frere Jacques"
    distance = Rhythm dist
    repetitions = 5
    scalesCanon = ScalesCanon title fjIntervals fjRhythms distance scales octaves instruments repetitions
    score = scalesCanonToScore scalesCanon


