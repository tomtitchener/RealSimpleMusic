
module Canon.UtilsTest where

import           Canon.Data
import           Canon.Utils
import qualified Data.ByteString.Lazy as LazyByteString
import           Data.Ratio
import qualified Data.Set as Set
import           RealSimpleMusic
import           ScoreToMidi.Utils
import           Test.HUnit

-- Frere Jacques

mIxC, mIxD, mIxE, mIxF, mIxG, mIxA, lIxG :: IndexedPitch
mIxC = IndexedPitch 0 0
mIxD = IndexedPitch 1 0
mIxE = IndexedPitch 2 0
mIxF = IndexedPitch 3 0
mIxG = IndexedPitch 4 0
mIxA = IndexedPitch 5 0
lIxG = IndexedPitch 4 (-1)
ixPt1, ixPt2, ixPt3, ixPt4, fjIndexedPitches :: [IndexedPitch]
ixPt1 = [mIxC, mIxD, mIxE, mIxC]
ixPt2 = [mIxE, mIxF, mIxG]
ixPt3 = [mIxG, mIxA, mIxG, mIxF, mIxE, mIxC]
ixPt4 = [mIxC, lIxG, mIxC]
fjIndexedPitches = ixPt1 ++ ixPt1 ++ ixPt2 ++ ixPt2 ++ ixPt3 ++ ixPt3 ++ ixPt4 ++ ixPt4

accentsPt1, accentsPt2, accentsPt3, accentsPt4, fjAccents :: [VoiceControl]
accentsPt1 = [AccentControl Soft,AccentControl Normal,AccentControl Hard,AccentControl Normal]
accentsPt2 = [AccentControl Soft,AccentControl Normal,AccentControl Hard]
accentsPt3 = [AccentControl Hard,AccentControl Hard,AccentControl Normal,AccentControl Normal,AccentControl Soft,AccentControl VerySoft]
accentsPt4 = [AccentControl Soft,AccentControl Soft,AccentControl VerySoft]
fjAccents = accentsPt1 ++ accentsPt1 ++ accentsPt2 ++ accentsPt2 ++ accentsPt3 ++ accentsPt3 ++ accentsPt4 ++ accentsPt4
fjAccentsSets :: [Set.Set VoiceControl]
fjAccentsSets = map Set.singleton fjAccents

dynamicssPt1, dynamicssPt2, dynamicssPt3, dynamicssPt4, fjDynamicss :: [[Dynamic]]
dynamicssPt1 = [[Piano, Crescendo], [], [Forte], [Piano]]
dynamicssPt2 = [[MezzoPiano, Crescendo], [], [Fortissimo]]
dynamicssPt3 = [[Forte, Decrescendo], [], [], [], [], [Piano]]
dynamicssPt4 = [[MezzoForte, Decrescendo], [], [Pianissimo]]
fjDynamicss  = dynamicssPt1 ++ dynamicssPt1 ++ dynamicssPt2 ++ dynamicssPt2 ++ dynamicssPt3 ++ dynamicssPt3 ++ dynamicssPt4 ++ dynamicssPt4
fjDynamicss' :: [[VoiceControl]]
fjDynamicss' = (map . map) DynamicControl fjDynamicss
fjDynamicsSets :: [Set.Set VoiceControl]
fjDynamicsSets = map Set.fromList fjDynamicss'

articulationsPt1, articulationsPt2, articulationsPt3, articulationsPt4, fjArticulations :: [VoiceControl]
articulationsPt1 = [ArticulationControl Staccato,ArticulationControl Staccato,ArticulationControl Tenuto,ArticulationControl Marcato]
articulationsPt2 = [ArticulationControl Staccato,ArticulationControl Staccato,ArticulationControl Portato]
articulationsPt3 = [ArticulationControl Staccatissimo,ArticulationControl Staccatissimo,ArticulationControl Staccatissimo,ArticulationControl Staccatissimo,ArticulationControl Marcato,ArticulationControl Marcato]
articulationsPt4 = [ArticulationControl Portato,ArticulationControl Portato,ArticulationControl Tenuto]
fjArticulations = articulationsPt1 ++ articulationsPt1 ++ articulationsPt2 ++ articulationsPt2 ++ articulationsPt3 ++ articulationsPt3 ++ articulationsPt4 ++ articulationsPt4
fjArticulationsSets :: [Set.Set VoiceControl]
fjArticulationsSets = map Set.singleton fjArticulations

-- Combine fjAccents and fjArticulations into fjControls which is list of Set.Set composed of two controls each.
fjControls :: [Set.Set VoiceControl]
fjControls = zipWith3 (\accents dynamics articulations -> Set.unions [accents, dynamics, articulations]) fjAccentsSets fjDynamicsSets fjArticulationsSets

-- Want a list of sets, where each set is per note
fjIxNotes :: [IndexedNote]
fjIxNotes = zipWith3 IndexedNote fjIndexedPitches fjRhythms fjControls

eig, qtr, hlf :: Rhythm
eig = Rhythm (1%8)
qtr = Rhythm (1%4)
hlf = Rhythm (1%2)
rh1, rh2, rh3, fjRhythms :: [Rhythm]
rh1 = [qtr, qtr, qtr, qtr]
rh2 = [qtr, qtr, hlf]
rh3 = [eig, eig, eig, eig, qtr, qtr]
fjRhythms = rh1 ++ rh1 ++ rh2 ++ rh2 ++ rh3 ++ rh3 ++ rh2 ++ rh2

piano, marimba, vibes :: Instrument
piano = Instrument "Acoustic Grand Piano"
marimba = Instrument "Marimba"
vibes = Instrument "Vibraphone"

cMaj, afMaj, eMaj, fMaj, gMaj, dMaj, aMaj, cMin, afMin, eMin, dMin :: Scale
cMaj  = majorScale C
afMaj = majorScale Af
eMaj  = majorScale E
fMaj  = majorScale F
gMaj  = majorScale G
dMaj  = majorScale D
aMaj  = majorScale A
dMin  = naturalMinorScale D
eMin  = naturalMinorScale E
cMin  = naturalMinorScale C
afMin = naturalMinorScale Af

keySignature :: KeySignature
keySignature = KeySignature (-1) -- F Major, one flat.

timeSignature :: TimeSignature
timeSignature = TimeSignature 4 4

tempo :: Tempo
tempo = Tempo (Rhythm (1%4)) 60

-- Simple Canon
createFJSimpleCanon :: String -> Int -> Rational -> SimpleCanon
createFJSimpleCanon instrName voices dur =
  SimpleCanon title keySignature timeSignature tempo ixNotes cMaj distance instr voices repetitions  
  where
    title = "Frere Jacques Simple"
    ixNotes = fjIxNotes
    instr = Instrument instrName
    distance = Rhythm dur
    repetitions = 5

writeFJSimpleCanon :: (Score -> a) -> String -> Int -> Rational -> a
writeFJSimpleCanon scoreWriter instrName voices dur =
  scoreWriter score
  where
    simpleCanon = createFJSimpleCanon instrName voices dur
    score       = simpleCanonToScore simpleCanon

-- | Generate test data for simple canon
--   writeFJSimpleCanonToFile "Acoustic Grand Piano" 4 (2%1)
writeFJSimpleCanonToFile :: String -> Int -> Rational -> IO ()
writeFJSimpleCanonToFile = writeFJSimpleCanon scoreToMidiFile
    
writeFJSimpleCanonToByteString :: String -> Int -> Rational -> LazyByteString.ByteString
writeFJSimpleCanonToByteString = writeFJSimpleCanon scoreToByteString

testSimpleCanon :: Assertion
testSimpleCanon =
  do
    referenceFileSimpleCanonByteString <- LazyByteString.readFile "./tests/data/Frere Jacques Simple.mid"
    referenceFileSimpleCanonByteString @=? generatedSimpleCanonByteString
  where
    generatedSimpleCanonByteString = writeFJSimpleCanonToByteString "Acoustic Grand Piano" 4 (2%1)

-- Scales Canon
createFJScalesCanon :: [Instrument] -> [Scale] -> [Octave] -> Rational -> ScalesCanon
createFJScalesCanon instruments scales octaves dur =
  ScalesCanon title keySignature timeSignature tempo ixNotes scales distance octaves instruments repetitions  
  where
    title = "Frere Jacques Scales"
    ixNotes = fjIxNotes
    distance = Rhythm dur
    repetitions = 5

writeFJScalesCanon :: (Score -> a) -> [Instrument] -> [Scale] -> [Int] -> Rational -> a
writeFJScalesCanon scoreWriter instruments scales octaveInts dur =
  scoreWriter score
  where
    octaves     = map Octave octaveInts
    scalesCanon = createFJScalesCanon instruments scales octaves dur
    score       = scalesCanonToScore scalesCanon
    
-- | Generate test data for scales canon
--   writeFJScalesCanonToFile [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] (2%1)
writeFJScalesCanonToFile :: [Instrument] -> [Scale] -> [Int] -> Rational -> IO ()
writeFJScalesCanonToFile = writeFJScalesCanon scoreToMidiFile

writeFJScalesCanonToByteString :: [Instrument] -> [Scale] -> [Int] -> Rational -> LazyByteString.ByteString
writeFJScalesCanonToByteString = writeFJScalesCanon scoreToByteString
  
testScalesCanon :: Assertion
testScalesCanon =
  do
    referenceFileScalesCanonByteString <- LazyByteString.readFile "./tests/data/Frere Jacques Scales.mid"
    referenceFileScalesCanonByteString @=? generatedScalesCanonByteString
  where
    generatedScalesCanonByteString = writeFJScalesCanonToByteString [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] (2%1)

-- Canon
createFJCanon :: [Instrument] -> [Scale] -> [Octave] -> [Rational] -> Canon
createFJCanon instruments scales octaves durs =
  Canon title keySignature timeSignature tempo ixNotess scales distances octaves instruments repetitions  
  where
    title = "Frere Jacques Canon"
    ixNotess = replicate (length instruments) fjIxNotes
    distances = map Rhythm durs
    repetitions = 5

writeFJCanon :: (Score -> a) -> [Instrument] -> [Scale] -> [Int] -> [Rational] -> a
writeFJCanon scoreWriter instruments scales octaveInts durs =
  scoreWriter score
  where
    octaves = map Octave octaveInts
    canon   = createFJCanon instruments scales octaves durs
    score   = canonToScore canon

-- | Generate test data for canon
--   writeFJCanonToFile [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] [2%1, 1%4, 1%8]
writeFJCanonToFile :: [Instrument] -> [Scale] -> [Int] -> [Rational] -> IO ()
writeFJCanonToFile = writeFJCanon scoreToMidiFile
    
writeFJCanonToByteString :: [Instrument] -> [Scale] -> [Int] -> [Rational] -> LazyByteString.ByteString
writeFJCanonToByteString = writeFJCanon scoreToByteString

testCanon :: Assertion
testCanon =
  do
    referenceFileCanonByteString <- LazyByteString.readFile "./tests/data/Frere Jacques Canon.mid"
    referenceFileCanonByteString @=? generatedCanonByteString
  where
    generatedCanonByteString = writeFJCanonToByteString [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] [2%1, 1%4, 1%8]


{--
writeFJSimpleCanonToFile "Acoustic Grand Piano" 4 (2%1)
writeFJScalesCanonToFile [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] (2%1)
writeFJCanonToFile [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] [2%1, 1%4, 1%8]
scoreToLilypondFile $ simpleCanonToScore $ createFJSimpleCanon "Acoustic Grand Piano" 4 (2%1)
scoreToLilypondFile $ scalesCanonToScore $ createFJScalesCanon [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] (2%1)
scoreToLilypondFile $ canonToScore $ createFJCanon [piano, marimba, vibes, piano] [cMaj, afMaj, eMin, dMin] [0, -1, 1, -2] [2%1, 1%4, 1%8]
--}
