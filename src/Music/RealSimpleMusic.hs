
-- | RealSimpleMusic API.
--   Types and records PitchClass, Scale, Octave, Pitch,
--   IndexedPitch, and etc. implement components aggregated
--   by top-level Score type.
--   Convenience functions to
--   1) generate scales: majorScale,
--   naturalMinorScale, melodicMinorScale, and
--   scaleFromEnhChromaticScale, 2) manipulate Pitch
--   getPitch,
--   3) manipulate Note:
--   indexedNoteToNote, indexedNoteToNotes, noteToRhythm.
--   Workhorse functions to convert Score to Midi file or
--   files:  scoreToMidiFile, scoreToMidiFiles, and to
--   convert Score to Lilypond file:  scoreToLilypondFile
module Music.RealSimpleMusic (
   PitchClass(..)
   ,  pitchClassToEnhIdx
   ,  Scale
   ,  majorScale
   ,  chromaticScale
   ,  naturalMinorScale
   ,  melodicMinorScale
   ,  scaleFromEnhChromaticScale
   ,  scaleToAscendingPitchClasses
   ,  scaleToDescendingPitchClasses
   ,  Octave(..)
   ,  Pitch(..)
   ,  IndexedPitch(..)
   ,  Dynamic(..)
   ,  DiscreteDynamicValue(..)
   ,  Balance(..)
   ,  Pan(..)
   ,  PanVal(..)
   ,  Tempo(..)
   ,  TempoVal(..)
   ,  normalizeTempoVals   
   ,  KeySignature(..)
   ,  TimeSignature(..)
   ,  Articulation(..)
   ,  Accent(..)
   ,  Rhythm
   ,  mkRhythm
   ,  getRhythm   
   ,  RhythmDenom(..)   
   ,  Instrument(..)
   ,  ScoreControls(..)
   ,  VoiceControl(..)
   ,  Note(..)
   ,  addControlToNote
   ,  IndexedNote(..)
   ,  ixPitchToPitch
   ,  indexedNoteToNote
   ,  indexedNotesToNotes
   ,  transposeIndexedPitch
   ,  transposeIndexedNote
   ,  Interval
   ,  Intervals
   ,  Chord(..)
   ,  Voice(..)
   ,  Title
   ,  Score(..)
   , scoreToMidiFile
   , scoreToMidiFiles
   , scoreToLilypondFile
   , scoreToByteString
  ) where

import Music.RealSimpleMusic.Music.Data
import Music.RealSimpleMusic.Music.Utils
import Music.RealSimpleMusic.ScoreToMidi.Utils
import Music.RealSimpleMusic.ScoreToLilypond.Utils
