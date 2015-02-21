
module ScoreToLilypond.Utils where

import           RealSimpleMusic
--import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder
import           Data.List
import           Data.Monoid

{--
infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
--}

-- data PitchClass = Bs|C|Dff|Bss|Cs|Df|Css|D|Eff|Ds|Ef|Fff|Dss|E|Ff|Es|F|Gff|Ess|Fs|Gf|Fss|G|Aff|Gs|Af|Gss|A|Bff|As|Bf|Cff|Ass|B| Cf deriving (Bounded, Enum, Show, Ord, Eq) 

equivPitchClasses :: [[PitchClass]]
equivPitchClasses = [[Cff, Cf, C, Cs, Css], [Dff, Df, D, Ds, Dss], [Eff, Ef, E, Es, Ess], [Fff, Ff, F, Fs, Fss], [Gff, Gf, G, Gs, Gss], [Aff, Af, A, As, Ass], [Bff, Bf, B, Bs, Bss]]

pitchNames :: [String]
pitchNames = ["C", "D", "E", "F", "G", "A", "B"]

findEquivPitchClassIndex :: PitchClass -> Int
findEquivPitchClassIndex pc =
  case findIndex (elem pc) equivPitchClasses of
   Nothing -> error $ "findEquivPitchClassIndex no match for pitch class " ++ show pc ++ " in " ++ show equivPitchClasses
   Just ix -> ix

renderPitchName :: PitchClass -> Builder
renderPitchName pc = string7 $ pitchNames !! findEquivPitchClassIndex pc

accidentalNames :: [String]
accidentalNames = ["eses", "es", "", "is", "isis"];

findEquivPitchClassAccidentalIndex :: PitchClass -> Int
findEquivPitchClassAccidentalIndex pc =
  case elemIndex pc pcs of
   Nothing -> error $ "findEquivPitchClassAccidentalIndex no match for pitch class " ++ show pc ++ " in " ++ show pcs
   Just ix -> ix
  where
    pcs = equivPitchClasses !! findEquivPitchClassIndex pc

renderAccidental :: PitchClass -> Builder
renderAccidental pc = string7 $ accidentalNames !! (findEquivPitchClassAccidentalIndex pc)
  
renderPitchClass :: PitchClass -> Builder
renderPitchClass pc = renderPitchName pc <> renderAccidental pc

renderOctave :: Octave -> Builder
renderOctave (Octave octave) =
  if (octave >= 0)
  then
    string7 (replicate (octave + 1) '\'')
  else
    if (octave == -1)
  then
    string7 ""
  else
    string7 (replicate (abs (octave + 1)) ',')

renderPitch :: Pitch -> Builder
renderPitch (Pitch pitchClass octave) =
  renderPitchClass pitchClass <> renderOctave octave
