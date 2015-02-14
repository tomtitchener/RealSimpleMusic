
### Simple Haskell data types for music ###

Folder hierarchy:

    ./ + -- project root, run: cabal configure --enable-tests,
            cabal build, cabal test, cabal haddock
       ./src + -- source folder hierarchy 
             RealSimpleMusic.hs -- module definition for music types and
                                   conversion functions, implementation
                                   files in Music and MusicToMidi folders
             Canon.hs -- module definition for canon types and conversion
                         functions
             ./Canon +
                     Data.hs -- Canon types
                     Utils.hs -- Functions to convert Canon types to Score
             ./Music +
                     Data.hs -- Music types
                     Utils.hs -- Functions, majorScale, transposeNote, 
                                 transposePitch, etc.
             ./MusicToMidi +
                     Utils.hs -- scoreToMidi and ScoreToMidiFiles functions
                                 and support functions
       ./tests + -- tests folder hierarchy 
               MainTestSuite.hs -- tests for Music and Canon functions
               ./Canon +
			           UtilsTest.hs -- HUnit tests for Canon functions
               ./Music +
			           UtilsTest.hs -- HUnit and QuickCheck tests for Music
                                       functions
               ./data
                       Frere Jacques Canon.mid   -- simple canon reference
                       Frere Jacques Scales.mid  -- scales canon reference
                       Frere Jacques.mid         -- canon reference

The file `./src/Music/Data.hs` declares Music types and builds them
up, element-by-element into a simple `Score` type.  To work backward:

* a `Score`comprises a `Title` (a synonym for `String`) and a list of
  `Voice`s. 
* a `Voice` comprises an `Instrument`, a list of `Note`s, and a list
  of  a list of `Control`s.
* an `Instrument` is a `newtype` that wraps a `String` and is
  converted to a Midi instrument by the Midi library method
  `instrumentNameToProgram`.
* a `Note` is one of `Note`, with `Pitch` and `Rhythm`, or
  `AccentedNote` with `Pitch`, `Rhythm`, and `Accent`, etc.
* a `Control` is one of `DynamicControl` with `Dynamic` and `Rhythm`,
  or `BalanceControl` with `Balance` and `Rhythm`, etc.
* a `Pitch` is a `PitchClass` and an `Octave`
* a `PitchClass` is one of `Bs`, `C`, `Dff`, and etc. spanning pitch
  classes up to two sharps and two flats,
* an `Octave` is a `newtype` wrapper for an `Int` with a `Bounded`
  instance approximating the range of a concert piano
* a `Rhythm` is a `newtype` wrapper for a `Rational`, e.g. `1%2`
  corresonds to a half-note, `1%16` to a sixteenth, and etc, including
  multiples, e.g. `7%16`, `3%2`, or `2%1`.

The file `./src/MusicToMidi/Utils.hs` contains the two main entry
points:

* `scoreToMidiFile`: convert a score to a single Midi file, assigning
  one channel to each voice, with a limit of 16 voices, as that's as
  many channels as are allowed in a standard Midi file.  Note that, as
  each `Voice` requires an `Instrument`, each Midi track starts with a
  `program change` event, which should work around the customar
  association of track 15 with percussion.
* `scoreToByteString`: the same as `scoreToMidiFile` except answer the
  `ByteString` directly instead of writing it to a file.  Used to test
  a generated file against a reference instance created early, see
  e.g. the canon files in the `tests/data` folder.
* `scoreToMidiFiles`: convert a score into many Midi files, one per
  voice.  Allows you to create an arbitrarily large score and import
  the voices one-by-one into editors that handle more than 16 voices,
  e.g. GarageBand and Logic.  

The structure of the `Score` is too simple, lacking tempo, key
signature, or other global settings.  And it may be other aggregates,
like the `Motto` types seem to be of little value and may be
deprecated. 

This is the largest file of the package by far (almost 500 lines),
with many functions to convert the simple music types in
`./src/Music/Data.hs` to the Midi types in the `Sound.MIDI` package.
The conversion maps instances for the `Controls` enum to Midi effects,
e.g. `Dynamic` in `DynamicControl` maps to Midi volume, `Balance` and
`Pan` map to Midi pan, and etc., including `InstrumentControl`, which,
in principal, lets you swap the piccolo part to a tuba if you want.
Absent a tempo control, the Midi file defaults to 120 beats per
minute.  It's a one-way trip.  There's no conversion from MIDI to the
simple music types.  

Here's a minimal `Score` that contains one `Voice` with one note,
middle `C` that's one whole-note long:

    Score
      "Minimal" -- Title
	  [Voice
	    (Instrument "Marimba")            -- Instrument
        [Note (Pitch C 0) (Rhythm (1%1))] -- Notes
		[]                                -- Controls
      ]

Here's a `cabal repl`sesion to create the score and convert it to a
Midi file:

	bash-3.2$ cabal repl
	Preprocessing library RealSimpleMusic-0.1.0.0...
	GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
	...
	λ: :m +Data.Ratio
	λ: :m +RealSimpleMusic
	λ: let score = Score "Minimal" [Voice (Instrument "Marimba") [Note (Pitch C 0) (Rhythm (1%1))] []]
	λ: scoreToMidiFile score
	
See the file `tests/Canon/UtilsTest.hs` for examples showing the use
of simple types, score, and conversion routines.  
