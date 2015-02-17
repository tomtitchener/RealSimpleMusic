
-- | RealSimpleMusic API.
--   Types and records PitchClass, Scale, Octave, Pitch,
--   IndexedPitch, and etc. implement components aggregated
--   by top-level Score type.
--   Convenience functions to
--   1) generate scales: majorScale,
--   naturalMinorScale, melodicMinorScale, and
--   scaleFromEnhChromaticScale, 2) manipulate Pitch
--   transposePitch, getPitch, 3) manipulate Note:
--   indexedNoteToNote, indexedNoteToNotes, tranposeNote,
--   transposeNoteMotto, noteToRhythm.
--   Workhorse functions to convert Score to Midi file or
--   files:  scoreToMidiFile, scoreToMidiFiles.
module RealSimpleMusic (
   PitchClass(..)
   ,  Motto(..)
   ,  Scale(..)
   ,  majorScale
   ,  naturalMinorScale
   ,  melodicMinorScale
   ,  scaleFromEnhChromaticScale
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
   , scoreToMidiFile
   , scoreToMidiFiles
  ) where

import Music.Data
import Music.Utils
import MusicToMidi.Utils
