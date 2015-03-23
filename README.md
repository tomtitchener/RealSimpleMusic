
### Simple Haskell data types for music ###

Folder hierarchy:

    ./ + -- project root, run: cabal configure --enable-tests,
            cabal build, cabal test, cabal haddock
       ./src + -- source files
             RealSimpleMusic.hs -- module definition for music types and
                                   conversion functions, implementation
                                   files in Music and ScoreToMidi folders
             Canon.hs -- module definition for canon types and conversion
                         functions
             ./Canon +
                     Data.hs -- Canon types
                     Utils.hs -- Functions to convert Canon types to Score
             ./Music +
                     Data.hs -- Music types
                     Utils.hs -- Functions, majorScale, transposeNote, 
                                 transposePitch, etc.
             ./ScoreToMidi +
                     Utils.hs -- scoreToMidi and ScoreToMidiFiles functions
                                 and support functions
             ./ScoreToLilypond +
                     Utils.hs -- scoreToLilypond function for converting a
                                 score to a Lilypond text file 
       ./tests + -- test files
               MainTestSuite.hs -- tests for Music, ScoreToMidi, and Canon
                                   functions
               ./Canon +
                       UtilsTest.hs -- HUnit tests for Canon functions
               ./Music +
                       UtilsTest.hs -- HUnit and QuickCheck tests for Music
                                       functions
               ./ScoreToMidi +
                       UtilsTest.hs -- HUnit tests for ScoreToMidi functions
               ./ScoreToLilypond +
                       UtilsTest.hs -- HUnit tests for ScoreToLilypond functions
               ./data
                       Frere Jacques Simple.ly   -- simple canon Lilypond 
                       Frere Jacques Simple.mid  -- simple canon reference 
                       Frere Jacques Simple.pdf  -- simple canon Score
                       Frere Jacques Scales.ly   -- scales canon Lilypond
                       Frere Jacques Scales.mid  -- scales canon reference 
                       Frere Jacques Scales.pdf  -- scales canon score
                       Frere Jacques Canon.ly    -- canon Lilypond
                       Frere Jacques Canon.mid   -- canon reference  
                       Frere Jacques Canon.pdf   -- canon score
					   
 The file `./src/Music/Data.hs` declares Music types and builds them  
up, element-by-element into a simple `Score` type:

* a `Score`comprises a `Title` (a synonym for `String`), a composer
   String, a list of `(ScoreControl,Rhythm)` pairs, and a  list of
  `Voice`s. 
* a `Voice` comprises an `Instrument`, a list of `Note`s, and a list
  of  a list of `VoiceControl`s.
* an `Instrument` is a `newtype` that wraps a `String` and is
  converted to a Midi instrument by the Midi library method
  `instrumentNameToProgram`.
* a `Note` is one of `Note`, with `Pitch`, `Rhythm`, and `Set`
  of `VoiceControl`, `Rest` with `Ryhthm` and `Set` of 
  `VoiceControl`, or `PercussionNote` with `Rhythm` and
  `Set` of `VoiceControl`.
* a `VoiceControl` is one of `DynamicControl` with `Dynamic`,
  `BalanceControl` with `Balance`, etc.
* a `Pitch` is a `PitchClass` and an `Octave`
* a `PitchClass` is one of `Bs`, `C`, `Dff`, and etc. spanning pitch
  classes up to two sharps and two flats,
* an `Octave` is a `newtype` wrapper for an `Int` with a `Bounded`
  instance approximating the range of a concert piano
* a `Rhythm` is a `newtype` wrapper for a `Rational`, e.g. `1%2`
  corresonds to a half-note, `1%16` to a sixteenth, and etc, including
  multiples, e.g. `7%16`, `3%2`, or `2%1`.
* a `ScoreControl` is one of `TempoControl` with `Tempo`,
  `KeySignatureControl` with `KeySignature`, or
  `TimeSignatureControl` with `TimeSignature`.

The file `./src/ScoreToMidi/RealSimpleMusic.hs` contains the API
for the package.   In addition to the types above, the main functions
are implemented in `./src/ScoreToMidi/Utils.hs` and
`./src/ScoreToLilypond/Utils.hs`:

* `scoreToMidiFile`: render a score as a single Midi file, assigning
  one channel to each voice, with a limit of 16 voices, as per the 
  Midi spec.  Note that, as each `Voice` requires an `Instrument`,
  each Midi track starts with a `program change` event.  This method
  combines voices by instrument into one track, combining control
  messages for `VoiceControls`.  To preserve per-voice `VoiceControl`s
  e.g. unique per-voice dynamic, or to render a `Score` with more than
  16 unique instruments, use `scoreToMidiFiles` instead.
* `scoreToByteString`: the same as `scoreToMidiFile` except answer the
  `ByteString` directly instead of writing it to a file.  Used to test
  a generated file against a reference instance created early, see
  e.g. the canon files in the `tests/data` folder.
* `scoreToMidiFiles`: render a score into many Midi files, one per
  voice.  Allows you to create an arbitrarily large score and import
  the voices one-by-one into editors that handle more than 16 voices,
  e.g. GarageBand and Logic.  Preserves all `[[VoiceControls]]`
  values.
  * `scoreToLilypondFile`: render a score to a [Lilypond](http://www.lilypond.org) format text
  file.  Lilypond  is a program to engrave scores to PDF files.

The `./src/ScoreToMidi/Utils.hs` file  is the largest file of the
package (over 800 lines), with many functions to convert the
simple music types in `./src/Music/Data.hs` to the Midi types
in the `Sound.MIDI`.  The conversion maps instances for the
`VoiceControls` and `ScoreControls` enum to Midi effects, e.g.
`Dynamic` in `DynamicControl` maps to Midi volume, `Balance`
and `Pan` map to Midi pan, and etc., including `InstrumentControl`,
which, in principal, lets you swap the piccolo part to a tuba if you want.
Absent a tempo control, the Midi file defaults to 120 beats per
minute.  It's a one-way trip.  There's no conversion from MIDI to the
simple music types.  

Here's a minimal `Score` that contains one `Voice` with one note,
middle `C` that's one whole-note long:

    Score
      "Minimal" -- Title
	  "Me"         -- Title
	  []              -- ScoreControls
      [Voice
        (Instrument "Marimba")                          -- Instrument
        [Note (Pitch C 0) (Rhythm (1%1)) empty] -- Notes
      ]              -- Voices

Here's a `cabal repl`sesion to create the score and convert it to a
Midi file:

    bash-3.2$ cabal repl
    Preprocessing library RealSimpleMusic-0.1.0.0...
    GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
    ...
    λ::m +Data.Ratio
    λ: :m +RealSimpleMusic 
    λ: :m +Data.Set
    λ: let score = Score "Minimal" "Me" [Voice (Instrument "Marimba") [Note (Pitch C 0) (Rhythm (1%1)) empty]]
	λ: scoreToMidiFile score
	
See the file `tests/Canon/UtilsTest.hs` for examples showing the use
of simple types, score, and conversion routines.  

For more complete examples, see `./src/Canon/Canon.hs`, the
implemetation files `Data.hs` and `Utils.hs` in `./src/Canon/Canon/`
and the test files in `./src/tests/Canon/UtilsTest.hs`.

For program examples generally, see the files in `./tests`, including

* `MainTestSuite.hs` - test driver
* `Canon/UtilsTest.hs` - test `Canon.hs` types and methods
* `Music/UtilsTest.hs` - test `Music` types and methods
* `ScoreToLilypond/UtilsTest.hs` - test `Lilypond` methods
* `ScoreToMidi/UtilsTest.hs` - test `ScoreToMidi` methods

TBD:

* Continuous controls:
  - Concrete control and start of continuous control
     on same note, e.g. piano + crescendo.
 - Watch out for multiple continous controls on same
    note, e.g. global dynamic plus per-voice dynamic,
    in Lilypond rendering will print multiple values,
	Midi doesn't matter even with multiple controls.

* Bugs:
  - Articulations need to be proportional in a way that doesn't
    sound odd.

* Validate continuous control behavior in both Midi and Lilypond renderings.
  Add behaviors to voices in canons, including accents:
  - Crescendo/Decrescendo (per voice)
  - PanUp/PanDown (per voice)
  - Accelerando/Ritardando (all voices)
  Add behaviors to all voices together:
  - Accelerando/Ritardando.

* Automatic cleff disposition:
  - add 8vb/8va.

* Score controls
  - Lilypond rendering for all tempos with rhythms

* Expand Instrument
  - Need additional attributes:  range, family, staff (Viola, DrumStaff vs. RhythmicStaff
    for relatively pitched, e.g. tam-tam, vs unpitched e.g. snare)
  - With range attribute, add validation pass .. when?  Rendering to Midi at the least,
    maybe to Lilypond as well.
  - With family attribute, group staves by family for Lilypond.

* Multi-note instruments, e.g. piano, marimaba, vibes, etc.
  - Music types need for any Note to also be a Chord of multiple simultaneous
    notes of equal duration.  Ugh, that's not strictly true.  With piano, you
    can hang onto pitches selectively.  So a list of notes each with its own
    duration all of which start at the same time.
  - Midi should be trivially able to render chords.
  - Lilypond rendering requires rhythmic spaces to cover rests for
    multiple staves, disposition of notes by staff 

* Validate Lilypond output.  Just assume lilypond executable exists?
  Is it possible to configure test with command-line arguments?
  Right now, there's just a list of repl commands to create Lilyond files for the
  three canons, which I then check by running through lilypond to see they compile
  without errors.

* Update tests for better coverage.  I tend to forget about adding tests as I get 
  things to just compile and they seem to behave as expected.
