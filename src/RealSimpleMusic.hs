
module RealSimpleMusic (
   PitchClass(..)
   ,  Motto(..)
   ,  Scale(..)
   ,  majorScale
   ,  naturalMinorScale
   ,  Octave(..)
   ,  Pitch(..)
   ,  IndexedPitch(..)
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
   ,  IndexedNote(..)
   ,  indexedNoteToNote
   ,  indexedNotesToNotes
   ,  transposeNote
   ,  noteToRhythm
   ,  NoteMotto
   ,  IndexedNoteMotto
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
