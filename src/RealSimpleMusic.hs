
module RealSimpleMusic (
   PitchClass(..)
   ,  Motto(..)
   ,  Scale
   ,  majorScale
   ,  naturalMinorScale
   ,  Octave(..)
   ,  Pitch(..)
   ,  transposePitch
   ,  getPitch
   ,  PitchMotto
   ,  Dynamic(..)
   ,  Balance(..)
   ,  Pan(..)
   ,  Tempo(..)
   ,  KeySignature(..)
   ,  Articulation(..)
   ,  Accent(..)
   ,  Rhythm(..)
   ,  Instrument(..)
   ,  Control(..)
   ,  Note(..)
   ,  transposeNote
   ,  noteToRhythm
   ,  NoteMotto
   ,  transposeNoteMotto
   ,  Interval
   ,  Intervals
   ,  Chord(..)
   ,  Voice(..)
   ,  Title
   ,  Score(..)
   , scoreToMidiFiles
   , scoreToMidiFile
  ) where

import Music.Data
import Music.Utils
import MusicToMidi.Utils
