TBD: format in markdown, update description of project struture

Simple Haskell data types for music and a functions to convert them to a Midi file.

Import Score.hs for data types Pitch, Dynamic, Rhythm, Accent, Note, etc.

Import ScoreToMidi for data type Score--with instances MidiScore, MidiVoicesScore, MidiPercussionScore--and functions scoreToMidiFile, midiInstrument and midiPercussionInstrument.

Example:  see util.hs

