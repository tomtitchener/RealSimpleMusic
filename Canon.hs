-- | Canons to explore RealSimpleMusic

module Canon where

import           Score
import           ScoreToMidi
import           Data.Ratio
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Sound.MIDI.File.Save as SaveFile

-- | First, simplest of all Canon:
--

data SimpleCanon = SimpleCanon
                   {title       :: Title
                   ,notes       :: NoteMotto
                   ,distance    :: Rhythm
                   ,instrument  :: Instrument
                   ,voices      :: Int
                   ,repetitions :: Int }
                 deriving (Show)

mC = Pitch C 0
mD = Pitch D 0
mE = Pitch E 0
mF = Pitch F 0
mG = Pitch G 0
mA = Pitch A 0
lG = Pitch G (-1)
pt1 = [mC, mD, mE, mC]
pt2 = [mE, mF, mG]
pt3 = [mG, mA, mG, mF, mE, lG]
pt4 = [mC, lG, mC]
pitches = pt1 ++ pt1 ++ pt2 ++ pt2 ++ pt3 ++ pt3 ++ pt4 ++ pt4
eig = Rhythm (1%8)
qtr = Rhythm (1%4)
hlf = Rhythm (1%2)
rh1 = [qtr, qtr, qtr, qtr]
rh2 = [qtr, qtr, hlf]
rh3 = [eig, eig, eig, eig, qtr, qtr]
rhythms = rh1 ++ rh1 ++ rh2 ++ rh2 ++ rh3 ++ rh3 ++ rh2 ++ rh2
ns = Motto $ zipWith Note pitches rhythms
dist = Rhythm $ sum (map getRhythm $ rh1 ++ rh1)
instr = Instrument "Acoustic Grand Piano"
fj = SimpleCanon "Frere Jacques" ns dist instr 4 2

noteEventToRhythm :: NoteEvent -> Rhythm
noteEventToRhythm (Note _ rhythm)           = rhythm
noteEventToRhythm (AccentedNote _ rhythm _) = rhythm
noteEventToRhythm (Rest rhythm)             = rhythm
  
simpleCanonToScore :: SimpleCanon -> Score
simpleCanonToScore (SimpleCanon title notes (Rhythm dist) instrument voices repetitions) =
  midiVoicesScore title sections
  where
    tune = concat $ replicate repetitions $ getMotto notes
    rests = map (Rest . Rhythm) $ take voices [(0%1)*dist, (1%1)*dist..]
    sections = [Section instrument [rest : tune] [] | rest <- rests]

writeFJCanon :: IO ()
writeFJCanon =
  LazyByteString.writeFile (canonTitle ++ ".mid") (SaveFile.toByteString (scoreToMidiFile fjScore))
  where
    sc = SimpleCanon "Frere Jacques" ns dist instr 4 5
    canonTitle = title sc
    fjScore = simpleCanonToScore sc