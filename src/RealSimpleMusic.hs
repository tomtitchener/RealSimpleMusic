
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
--   transposeNotes, noteToRhythm.
--   Workhorse functions to convert Score to Midi file or
--   files:  scoreToMidiFile, scoreToMidiFiles, and to
--   convert Score to Lilypond file:  scoreToLilypondFile
module RealSimpleMusic (
   PitchClass(..)
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
   ,  Dynamic(..)
   ,  Balance(..)
   ,  Pan(..)
   ,  Tempo(..)
   ,  KeySignature(..)
   ,  TimeSignature(..)
   ,  Articulation(..)
   ,  Accent(..)
   ,  Rhythm(..)
   ,  Instrument(..)
   ,  Control(..)
   ,  Note(..)
   ,  addControlToNote
   ,  IndexedNote(..)
   ,  indexedNoteToNote
   ,  indexedNotesToNotes
   ,  transposeNote
   ,  noteToRhythm
   ,  transposeNotes
   ,  Interval
   ,  Intervals
   ,  Chord(..)
   ,  Voice(..)
   ,  Title
   ,  Score(..)
   , scoreToMidiFile
   , scoreToMidiFiles
   , scoreToLilypondFile
  ) where

import Music.Data
import Music.Utils
import ScoreToMidi.Utils
import ScoreToLilypond.Utils
