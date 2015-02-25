

module ScoreToLilypond.Utils where

import           RealSimpleMusic
--import qualified Data.ByteString.Lazy               as L
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
renderedSpace, renderedOpen, renderedClose, renderedDot, renderedRest, renderedTie :: Builder
renderedSpace = charEncoding ' '
renderedOpen  = charEncoding '{'
renderedClose = charEncoding '}'
renderedDot   = charEncoding '.'
renderedRest  = charEncoding 'r'
renderedTie   = charEncoding '~'

-- | Global reference sets key and time signatures.
renderedGlobalPrefix :: Builder
renderedGlobalPrefix = stringEncoding "\\global"

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
renderRhythm :: Rhythm -> [Builder]
renderRhythm (Rhythm rhythm)
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
--   accentStrings always works so long as fromEnum maxBound::Accent is the length
--   of accentStrings - 1, see testAccentNames.    
--    
--   softest  = "\\markup {\\musicglyph #\"scripts.dmarcato\"}"
--   verysoft = "\\markup {\\musicglyph #\"scripts.upedaltoe\"}"
--   soft     = "<"
--   hard     = ">"
--   veryhard = "\\markup {\\musicglyph #\"scripts.dpedaltoe\"}"
--   hardest  = "\\markup {\\musicglyph #\"scripts.umarcato\"}
accentStrings :: [String]
accentStrings = ["\\softest", "\\verysoft", "\\soft", "", "\\hard", "\\veryhard", "\\hardest"]

renderedAccentNames :: Builder
renderedAccentNames = stringEncoding "softest = \"\\markup {\\musicglyph #\"scripts.dmarcato\"} verysoft = \"\\markup {\\musicglyph #\"scripts.upedaltoe\"} soft = \"<\" hard = \">\"veryhard = \"\\markup {\\musicglyph #\"scripts.dpedaltoe\"}hardest  = \"\\markup {\\musicglyph #\"scripts.umarcato\"}"

-- | Interpreting accents requires the emitting the list accent of symbols above.
renderAccent :: Accent -> Builder
renderAccent accent =  stringEncoding $ accentStrings !! fromEnum accent 

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

-- Pitch doesn't matter when written to a percussion staff
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
renderedVoicePrefix = stringEncoding "\\new Voice \\with {\\remove \"Note_heads_engraver\" \\consists \"Completion_heads_engraver\" \\remove \"Rest_engraver\" \\consists \"Completion_rest_engraver\"}"

-- | A voice depends on global, score, and accent contexts.
--   TBD:  [[Controls]]  Each of these should be processed alongside notes!
renderVoice :: Voice -> Builder
renderVoice (Voice instrument notes _) =
  renderedVoicePrefix <> renderedOpen <> renderInstrument instrument <> renderedSpace <> renderedGlobalPrefix <> renderedSpace <> renderNotes notes <> renderedClose

renderVoices :: [Voice] -> Builder
renderVoices [] = mempty
renderVoices (voice:voices) = renderVoice voice <> mconcat [renderedSpace <> renderVoices voices]

-- | Every Lilypond file should start with a version number.
renderedVersion :: Builder
renderedVersion = stringEncoding "\\version \"2.18.2\""

-- | TBD: add time and key signatures to Score, then renderers for each.
renderedGlobalValues :: Builder
renderedGlobalValues = stringEncoding "global = {\\time 4/4 \\key c \\major}"

renderedHeader :: String -> String -> Builder
renderedHeader title composer = stringEncoding "\\header {title = " <> stringEncoding title <> stringEncoding " composer = " <> stringEncoding composer <> renderedClose
  
renderScore :: Score -> Builder
renderScore (Score title voices) = renderedVersion <> renderedSpace <> renderedHeader title "" <> renderedGlobalValues <> renderedAccentNames <> stringEncoding "\\score { \\new StaffGroup << " <> renderVoices voices <> stringEncoding ">> \\layout { } \\midi { } " <> renderedClose
  
{-- Snippets
version = "2.18.2"
global = { \time 4/4 \key c \major }
\header {
  title = "SUITE I."
  composer = "J. S. Bach."
}
softest  = "\\markup {\\musicglyph #\"scripts.dmarcato\"}"
verysoft = "\\markup {\\musicglyph #\"scripts.upedaltoe\"}"
soft     = "<"
hard     = ">"
veryhard = "\\markup {\\musicglyph #\"scripts.dpedaltoe\"}"
hardest  = "\\markup {\\musicglyph #\"scripts.umarcato\"}
\score {
  \new Staff \relative g, {
    \clef bass
    \key g \major
    \repeat unfold 2 { g16( d' b') a b d, b' d, } |
    \repeat unfold 2 { g,16( e' c') b c e, c' e, } |
}
\header {
piece = "Pr ́elude." }
}
\score {
  \new Staff \relative b {
    \clef bass
    \key g \major
    \partial 16 b16 |
    <g, d' b'~>4 b'16 a( g fis) g( d e fis) g( a b c) |
    d16( b g fis) g( e d c) b(c d e) fis( g a b) |
}
\header {
    piece = "Allemande."
  }
}
--}
