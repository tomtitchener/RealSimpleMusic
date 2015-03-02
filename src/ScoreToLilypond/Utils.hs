
module ScoreToLilypond.Utils where

import           Music.Data
import           Music.Utils
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
softString     = "\\markup {<}"
hardString     = "\\markup {>}"
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

-- | Match strings to enum: NoDynamic | Pianissimo | Piano | MezzoPiano | MezzoForte | Forte | Fortissimo 
dynamicValues :: [String]
dynamicValues = ["", "\\pp", "\\p", "\\mp", "\\mf", "\\f", "\\ff"]

-- | Match of enum values to list above.
renderDynamic :: Dyanmic -> Builder
renderDynamic =  stringEncoding $ dynamicValues !! fromEnum

-- | NoBalance | LeftBalance | MidLeftBalance | CenterBalance | MidRightBalance | RightBalance deriving (Bounded, Enum, Show, Ord, Eq)
balanceValues :: [String]
balanceValues = ["", "\\markup {left}", "\\markup {center-left}", "\\markup {center}", "\\markup {center-right}", "\\markup {right}"] 

-- | Match of enum values to list above.
renderBalance :: Balance -> Builder
renderBalance = stringEncoding $ balanceValues !! fromEnum

renderPan :: Pan -> Builder
renderPan (Pan pan) = stringEncoding "\\markup {pan " <> intDec pan <> stringEncoding " }"

renderTempo :: Tempo -> Builder
renderTempo (Tempo (Rhythm rhythm) bpm) = stringEncoding "\\tempo " <> intDec numer <> stringEncoding " = " ++ intDec bpm 
  where
    numer = numerator rhythm

-- | Lilypond wants e.g. \key g \minor, but all I have from KeySignature is the count of sharps and flats.
--   From which I could deduce major or minor tonic pitches equally.  Curiously enough, though, from the
--   perspective of the key signature, the issue of tonic has no bearing!  So actually, the bare count of
--   sharps or flats suffices.  Assuming a major key then if there are sharps, the tonic can be a half step
--   above the final sharp, or if there are flats, then the fifth above the final flat.  And if there are
--   no flats or sharps then it's just C.  Computing with the list of fifths is actually easier, so for
--   sharps, go instead for the natural minor tonic, where one sharp is E, or two fifths below.  Note this
--   will look a little screwy in the Lilypond output, but so long as the count of flats and sharps is right
--   it shouldn't matter.  So what I need is a way to say, e.g. for sharps, what's the pitch represented
--   by the count of sharps less two?  And for flats, what's the pitch for the count of flats plus one?
renderKeySignature :: KeySignature -> Builder
renderKeySignature (KeySignature accidentals) =
  stringEncoding "\\key " <> renderedPitchName tonicPitch <> renderedSpace <> stringEncoding mode <> renderedSpace
  where
    cIndex        = cycleOfFifthsIdx C
    cIndexOffset  = if accidentals == 0 then 0 else if accidentals > 0 then accidentals - 2 else accidentals + 1
    tonicPitch    = cycleOfFifths !! (cIndex + cIndexOffset)
    mode          = if accidentals >= 0 then "minor" else "major"

-- | Lilypond time signature is just e.g. "\time 2/4"
renderTimeSignature :: TimeSignature -> Builder
renderTimeSignature (TimeSignature num den) =
  stringEncoding "\\time" <> renderedSpace <> intDec num <> charEncoding "/" <> intDec den <> renderedSpace

-- | Lilypond has "." for staccato, "^" for marcato, and slurs for legato, so I suppose "-" is really tenuto
--   e.g. with shorthand notation "-^" for marcato, "--" for tenuto, and "-." for staccato.  There's also
--   "-'" for staccatissimo, so why not?  And similarly for portato, "-_".
articulationStrings :: [String]
articulationStrings = ["", "--", "-_", "-^", "-.", "-'"]

-- | Map Articulation enums NoArticulation | Tenuto | Portato | Marcato | Staccato | Staccatissimo
--   to Lilypond strings.
renderArticulation :: Articulation -> Builder
renderArticulation = stringEncoding $ articulationValues !! fromEnum

-- | TBD:  spit out simple quoted text for now
renderText :: Text -> Builder
renderText = charEncoding "-" <> stringEncoding

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


renderControl :: Control' -> Builder
renderControl (DynamicControl' dynamic)             = renderDynamic       dynamic
renderControl (BalanceControl' balance)             = renderBalance       balance
renderControl (PanControl' pan)                     = renderPan           pan
renderControl (TempoControl' tempo)                 = renderTempo         tempo
renderControl (KeySignatureControl' keySignature)   = renderKeySignature  keySignature
renderControl (TimeSignatureControl' timeSignature) = renderTimeSignature timeSignature
renderControl (ArticulationControl' articulation)   = renderArticulation  articulation
renderControl (TextControl' text)                   = renderText          text
renderControl (AccentControl' accent)               = renderAccent        accent

renderControls :: [Control'] -> Builder
renderControls controls = mconcat $ map renderControl controls

renderNote' :: Note' -> Builder
renderNote' (Note' pitch rhythm controls) =
  renderNoteForRhythms (renderPitch pitch) (renderControls controls) (renderRhythm rhythm)
renderNote' (Rest' rhythm controls) =
  renderRestForRhythms (renderControls controls) (renderRhythm rhythm)
renderNote' (PercussionNote' rhythm controls) =
  renderNoteForRhythms (renderPitch dummyPercussionPitch) (renderControls controls) (renderRhythm rhythm) 

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
  
