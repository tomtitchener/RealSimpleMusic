
-- | Data definitions for canon types

module Canon.Data where

import RealSimpleMusic

-- | Simplest of all Canons.  Imitation at unison, all voices
--   playing the same instrument.  Parameterized by title,
--   tune, scale, imitative distance, instrument, count of voices,
--   count of repetitions.
data SimpleCanon =
  SimpleCanon
  {sTitle         :: Title
  ,sKeySignature  :: KeySignature
  ,sTimeSignature :: TimeSignature
  ,sTempos        :: [(Tempo,Rhythm)]
  ,sIxNotes       :: [IndexedNote]
  ,sScale         :: Scale
  ,sDistance      :: Rhythm
  ,sInstrument    :: Instrument
  ,sCountVoices   :: Int
  ,sRepetitions   :: Int
  } deriving (Show)

-- | Additionally parameterize by lists of scales and octaves.
data ScalesCanon =
  ScalesCanon
  {scTitle         :: Title
  ,scKeySignature  :: KeySignature
  ,scTimeSignature :: TimeSignature
  ,scTempos        :: [(Tempo,Rhythm)]
  ,scIxNotes       :: [IndexedNote]
  ,scScales        :: [Scale]
  ,scDistance      :: Rhythm
  ,scOctaves       :: [Octave]
  ,scInstruments   :: [Instrument]
  ,scRepetitions   :: Int
  } deriving (Show)

-- | Additionally parameterize by imitative distance.
data Canon =
  Canon
  {cTitle         :: Title
  ,cKeySignature  :: KeySignature
  ,cTimeSignature :: TimeSignature
  ,cTempos        :: [(Tempo,Rhythm)]
  ,cIxNotess      :: [[IndexedNote]]
  ,cScales        :: [Scale]
  ,cDistances     :: [Rhythm]
  ,cOctaves       :: [Octave]
  ,cInstruments   :: [Instrument]
  ,cRepetitions   :: Int
  } deriving (Show)
