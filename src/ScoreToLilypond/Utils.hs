
module ScoreToLilypond.Utils where

import           Music.Data
import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder
import           Data.List
import           Data.Ratio
import           Data.Maybe
import           Data.Monoid

-- | Data.ByteString.Builder provides alternative encodings: string7, string8, stringUtf8
--   Does Lilypond care?  Seems like string7 (ASCII) should suffice.
--   There don't seem to be non-ASCII characters in the documentation.
--   Redirect encoding primitives for string here to make swapping easier.
stringEncoding :: String -> Builder
stringEncoding = string7

-- | Data.ByteChar.Builder provides alternative encodings: char7, char8, charUtf8
--   Redirect encoding primitives for string here to make swapping easier.
charEncoding :: Char -> Builder
charEncoding = char7

-- | Char constants, quotes are used to indicate upward octave counts,
--   commas for downward octave counts around the base octave which is
--   the C major scale below middle C
quoteChar, commaChar :: Char
quoteChar = '\''
commaChar = ','

-- | Rendered character constants 
renderedSpace, renderedOpen, renderedClose, renderedDot, renderedRest, renderedTie, renderedNewline :: Builder
renderedSpace   = charEncoding ' '
renderedOpen    = charEncoding '{'
renderedClose   = charEncoding '}'
renderedDot     = charEncoding '.'
renderedRest    = charEncoding 'r'
renderedTie     = charEncoding '~'
renderedNewline = charEncoding '\n'

-- | Global reference sets key and time signatures.
renderedGlobalKey :: Builder
renderedGlobalKey = stringEncoding "\\global"

-- | Unique PitchClass by pitch and accidental, aggregated by name for all accidentals
--   to map to separate encoding for Lilypond pitch and accidentals.
equivPitchClasses :: [[PitchClass]]
equivPitchClasses = [[Cff, Cf, C, Cs, Css], [Dff, Df, D, Ds, Dss], [Eff, Ef, E, Es, Ess], [Fff, Ff, F, Fs, Fss], [Gff, Gf, G, Gs, Gss], [Aff, Af, A, As, Ass], [Bff, Bf, B, Bs, Bss]]

-- | Lilypond pitch names are lower-case
pitchNames :: [String]
pitchNames = ["c", "d", "e", "f", "g", "a", "b"]

-- | Find index in equivPitchClasses for PitchClass.
--   Should never fail so long as equivPitchClasses
--   contains all instances of PitchClass (verified
--   by testEquivPitchClasses).
findEquivPitchClassIndex :: PitchClass -> Int
findEquivPitchClassIndex pc =
  fromMaybe
    (error $ "findEquivPitchClassIndex no match for pitch class " ++ show pc ++ " in " ++ show equivPitchClasses)
    (findIndex (elem pc) equivPitchClasses)

-- | Map from e.g. Cff to "c".
renderPitchName :: PitchClass -> Builder
renderPitchName pc = stringEncoding $ pitchNames !! findEquivPitchClassIndex pc

-- | Accidental strings to match order of accidentals in equivPitchClasses elements,
--   e.g. [Eff, Ef, E, Es, Ess] -> ["eses", "es", "", "is", "isis"].  
--   Lilypond accidental names are in Dutch
accidentalNames :: [String]
accidentalNames = ["eses", "es", "", "is", "isis"];

-- | Map from e.g. Cff to "eses".
--   The elemIndex should never fail so long as the count of
--   all lists in equivPitchClasses is the same as the count
--   of elements in accidentalNames (verified by testAccidentalNames).
findEquivPitchClassAccidentalIndex :: PitchClass -> Int
findEquivPitchClassAccidentalIndex pc =
  fromMaybe
    (error $ "findEquivPitchClassAccidentalIndex no match for pitch class " ++ show pc ++ " in " ++ show pcs)
    (elemIndex pc pcs)
  where
    pcs = equivPitchClasses !! findEquivPitchClassIndex pc

-- | Map e.g. Es to "is".
renderAccidental :: PitchClass -> Builder
renderAccidental pc = stringEncoding $ accidentalNames !! findEquivPitchClassAccidentalIndex pc

-- | Map e.g. "Bff" to "b".
renderPitchClass :: PitchClass -> Builder
renderPitchClass pc = renderPitchName pc <> renderAccidental pc

-- | Map e.g. Octave 1 to "''" and Octave -2 to ',,".
renderOctave :: Octave -> Builder
renderOctave (Octave octave)
  | octave >= 0    = stringEncoding $ replicate octaves quoteChar
  | octave == (-1) = stringEncoding ""
  | otherwise      = stringEncoding $ replicate octaves commaChar
  where
    octaves = abs $ octave + 1

-- | Combine pitch class and octave renderings to map e.g.
--   Pitch Cf (Octave 2) to "ces'''".
renderPitch :: Pitch -> Builder
renderPitch (Pitch pitchClass octave) =
  renderPitchClass pitchClass <> renderOctave octave

-- | Map e.g. 1%2 to "2", 3%8 to "4." and etc., where
--   non-integral, non-dotted rhythms are rendered as
--   multiple values meant to be tied notes when matched
--   up with pitch, accidental, and octave renderings.
--   Note zero-length duration may be placeholder e.g.
--   for "no rest" for the first voice of a canon.
--   Answser empty list so higher-level renderer drops
--   value.
renderRhythm :: Rhythm -> [Builder]
renderRhythm (Rhythm rhythm)
  | num == 0              = [] 
  | num > 1 && denom == 1 = replicate num $ intDec denom 
  | num == 1              = [intDec denom]
  | num == 3              = [intDec (denom `div` 2) <> renderedDot]
  | otherwise             = intDec remdenom : renderRhythm (Rhythm (rhythm - remainder))
  where
    num       = fromIntegral $ numerator rhythm
    denom     = fromIntegral $ denominator rhythm
    remainder = fromMaybe
                 (error $ "renderRhythm no remainder for rhythm " ++ show rhythm)
                 (find (rhythm >) [1%1, 1%2, 1%4, 1%8, 1%16, 1%32, 1%64, 1%128, 1%256])
    remdenom  = fromIntegral $ denominator remainder

-- | Music accents range Softest, VerySoft, Soft, Normal, Hard, VeryHard, Hardest
--   and are interepreted by scoreToMidi as modifications to the normalVelocity.
--   There's no span of accent markings to match a range of three on either side
--   of unaccented.  Invent a set here using glyphs and characters.  The "!!" for
--   accentKeys always works so long as fromEnum maxBound::Accent is the length
--   of accentKeys - 1, see testAccentNames.    
--    
--   softest  = "\\markup {\\musicglyph #\"scripts.dmarcato\"}"
--   verysoft = "\\markup {\\musicglyph #\"scripts.upedaltoe\"}"
--   soft     = "<"
--   hard     = ">"
--   veryhard = "\\markup {\\musicglyph #\"scripts.dpedaltoe\"}"
--   hardest  = "\\markup {\\musicglyph #\"scripts.umarcato\"}
accentKeys :: [String]
accentKeys = ["\\softest", "\\verysoft", "\\soft", "", "\\hard", "\\veryhard", "\\hardest"]

softestString, verysoftString, softString, hardString, veryhardString, hardestString :: String
softestString  = "\\markup {\\musicglyph #\"scripts.dmarcato\"}"
verysoftString = "\\markup {\\musicglyph #\"scripts.upedaltoe\"}"
softString     = "\"<\""
hardString     = "\">\""
veryhardString = "\\markup {\\musicglyph #\"scripts.dpedaltoe\"}"
hardestString  = "\\markup {\\musicglyph #\"scripts.umarcato\"}"

accentValues :: [String]
accentValues = [softestString, verysoftString, softString, hardString, veryhardString, hardestString]

renderAccentKeyValue :: String -> String -> Builder
renderAccentKeyValue key val = stringEncoding $ (filter (/= '\\') key) ++ " = " ++ val ++ "\n"

renderedAccentValues :: Builder
renderedAccentValues = mconcat $ zipWith renderAccentKeyValue (filter (/= "") accentKeys) accentValues

-- | Interpreting accents requires the emitting the list accent of symbols above.
renderAccent :: Accent -> Builder
renderAccent accent =  stringEncoding $ accentKeys !! fromEnum accent 

-- | Combine a rendered pitch with a list of rendered rhythms into
--   a rendered note, annotating with ties where there are multiple
--   rhythms.
renderNoteForRhythms :: Builder -> [Builder] -> Builder
renderNoteForRhythms _ [] = mempty
renderNoteForRhythms renderedPitch [renderedRhythm] = renderedPitch <> renderedRhythm
renderNoteForRhythms renderedPitch (renderedRhythm:renderedRhythms) =
  renderedPitch <> renderedRhythm <> mconcat [renderedTie <> renderedSpace <> renderNoteForRhythms renderedPitch renderedRhythms]

-- | Supply accent glyph along with pitch and rhythm.
--   Rendered pitch is first, then accent, then rhythm.
--   To avoid repeated accents, call renderNoteForRhythms
--   for recursive case
renderAccentedNoteForRhythms :: Builder -> Builder -> [Builder] -> Builder
renderAccentedNoteForRhythms _ _ [] = mempty
renderAccentedNoteForRhythms renderedPitch renderedAccent [renderedRhythm] = renderedPitch <> renderedRhythm <> renderedSpace <> renderedAccent
renderAccentedNoteForRhythms renderedPitch renderedAccent (renderedRhythm:renderedRhythms) =
  renderedPitch <> renderedRhythm <> renderedSpace <> renderedAccent <> mconcat [renderedTie <> renderedSpace <> renderNoteForRhythms renderedPitch renderedRhythms]

-- | Render rest or tied rests according to list of rendered rhythms.
renderRestForRhythms :: [Builder] -> Builder
renderRestForRhythms [] = mempty
renderRestForRhythms [renderedRhythm] = renderedRest <> renderedRhythm
renderRestForRhythms (renderedRhythm:renderedRhythms) =
  renderedRest <> renderedRhythm <> mconcat [renderedSpace <> renderRestForRhythms renderedRhythms]

-- | Pitch doesn't matter when written to a percussion staff
dummyPercussionPitch :: Pitch
dummyPercussionPitch = Pitch C $ Octave (-1)

-- | Render Note instances.
renderNote :: Note -> Builder
renderNote (Note pitch rhythm) =
  renderNoteForRhythms (renderPitch pitch) (renderRhythm rhythm)
renderNote (AccentedNote pitch rhythm accent) =
  renderAccentedNoteForRhythms (renderPitch pitch) (renderAccent accent) (renderRhythm rhythm)
renderNote (Rest rhythm) =
  renderRestForRhythms (renderRhythm rhythm)
renderNote (PercussionNote rhythm) =
  renderNoteForRhythms (renderPitch dummyPercussionPitch) (renderRhythm rhythm) 
renderNote (AccentedPercussionNote rhythm accent) =
  renderAccentedNoteForRhythms (renderPitch dummyPercussionPitch) (renderAccent accent) (renderRhythm rhythm) 

-- | Spaces separate notes in a rendered list of notes.
renderNotes :: [Note] -> Builder
renderNotes [] = mempty
renderNotes (note:notes) = renderNote note <> mconcat [ renderedSpace <> renderNote note' | note' <- notes]

-- | An instrument expects to be in a Staff or Voice context.
renderInstrument :: Instrument -> Builder
renderInstrument (Instrument instrumentName) =
  stringEncoding $ "\\set Staff.instrumentName = #\"" ++ instrumentName ++ "\""

-- | Start each voice with a block that enables automatic ties across bar lines.
renderedVoicePrefix :: Builder
renderedVoicePrefix = stringEncoding "\\new Voice \\with \n{\\remove \"Note_heads_engraver\" \\consists \"Completion_heads_engraver\" \\remove \"Rest_engraver\" \\consists \"Completion_rest_engraver\"}\n"

-- | A voice depends on global, score, and accent contexts.
--   TBD:  [[Controls]]  Each of these should be processed alongside notes!
--   Therre are two problems: 1) map the various controls to Lilypond text:
--   DynamicControl, BalanceControl, PanControl, TempoControl, KeySignatureControl,
--   TimeSignatureControl, ArticulationControl, TextControl, InstrumentControl
--   2) add the Lilypond text at the right rhythmic point in the list of notes
--   for the voice.  To solve the second problem, each of the lists in the
--   controlss and the notes must be iterated across simultaneously so that
--   the control text comes at the right point in the score.
--   For articulations, see Section A.13 in the reference.
--   Dynamics and Articulations are attached to individual notes, raising a
--   question about the way I keep them strictly separate in my Music type
--   definitions.  It'd certainly render more simply if my notes each had
--   attached an optionally empty list of controls.  For some controls, it
--   just doesn't make much sense to have the control abstracted from a note!
--   And maybe for simplicity I just convert and have all controls associate
--   with a note.  For continuously varying behavior like a crescendo across
--   a sustained note there's already an annotation. At most I might be losing
--   some degree of control from the Midi rendering, e.g. continuously varying
--   the pan setting across a sustained note.  It all sort of falls out nicely
--   the way things are, thanks to the Midi merge behavior.  And I'm going to
--   have to rework my rendering code with integrated controls e.g. turning
--   into zero delay Midi events that come before a note on event.  

renderVoice :: Voice -> Builder
renderVoice (Voice instrument notes _) =
  renderedVoicePrefix
  <> renderedOpen
  <> renderInstrument instrument
  <> renderedSpace
  <> renderedGlobalKey
  <> renderedSpace
  <> renderNotes notes
  <> renderedClose
  <> renderedNewline

renderVoices :: [Voice] -> Builder
renderVoices [] = mempty
renderVoices (voice:voices) = renderVoice voice <> mconcat [renderVoices voices]

-- | Every Lilypond file should start with a version number.
renderedVersion :: Builder
renderedVersion = stringEncoding "\\version \"2.18.2\"\n"

-- | TBD: add time and key signatures to Score, then renderers for each.
renderedGlobalValues :: Builder
renderedGlobalValues = stringEncoding "global = {\\time 4/4 \\key c \\major}\n"

renderedHeader :: String -> String -> Builder
renderedHeader title composer =
  stringEncoding "\\header {title = "
  <> stringEncoding title
  <> stringEncoding " composer = \""
  <> stringEncoding composer
  <> charEncoding '\"'
  <> renderedClose
  <> renderedNewline
  
renderScore :: Score -> Builder
renderScore (Score title voices) =
  renderedVersion
  <> renderedHeader title ""
  <> renderedGlobalValues
  <> renderedAccentValues
  <> stringEncoding "\\score {\n\\new StaffGroup << \n"
  <> renderVoices voices
  <> stringEncoding ">>\n\\layout { }\n\\midi { }\n"
  <> renderedClose
  <> renderedNewline

scoreToLilypondByteString :: Score -> L.ByteString
scoreToLilypondByteString = toLazyByteString . renderScore

scoreToLilypondFile :: Score -> IO ()
scoreToLilypondFile score@(Score title _) = L.writeFile (title ++ ".ly") $ scoreToLilypondByteString score
  
