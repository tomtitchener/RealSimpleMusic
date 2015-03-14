
module ScoreToLilypond.Utils where

import           Control.Monad()
import           Control.Monad.State
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import qualified Data.Set as Set
import           Data.Traversable
import           Music.Data
import           Music.Utils

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

-- | Rendered character constants 
renderedQuote, renderedComma, renderedSpace, renderedOpen, renderedClose, renderedDot, renderedRest, renderedTie, renderedNewline, renderedDash, renderedSlash, renderedDoubleQuote, renderedAsterisk, renderedNothing, renderedDoubleBar, renderedStopTextSpan :: Builder
renderedQuote         = charEncoding '\''
renderedComma         = charEncoding ','
renderedSpace         = charEncoding ' '
renderedOpen          = charEncoding '{'
renderedClose         = charEncoding '}'
renderedDot           = charEncoding '.'
renderedRest          = charEncoding 'r'
renderedTie           = charEncoding '~'
renderedNewline       = charEncoding '\n'
renderedDash          = charEncoding '-'
renderedSlash         = charEncoding '/'
renderedDoubleQuote   = charEncoding '\"'
renderedAsterisk      = charEncoding '*'
renderedNothing       = stringEncoding ""
renderedDoubleBar     = stringEncoding "\\bar \"|.\""
renderedStopTextSpan  = stringEncoding "\\stopTextSpan"

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
  | octave >= 0    = mconcat $ replicate octaves renderedQuote
  | octave == (-1) = stringEncoding ""
  | otherwise      = mconcat $ replicate octaves renderedComma
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
  | num > 1 && denom == 1 = [intDec denom <> renderedAsterisk <> intDec num]  -- replicate num $ intDec denom 
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
--   softest  = "^\\markup {\\musicglyph #\"scripts.dmarcato\"}"
--   verysoft = "^\\markup {\\musicglyph #\"scripts.upedaltoe\"}"
--   soft     = "^\\markup {<}"
--   hard     = "^\\markup {>}"
--   veryhard = "^\\markup {\\musicglyph #\"scripts.dpedaltoe\"}"
--   hardest  = "^\\markup {\\musicglyph #\"scripts.umarcato\"}
accentKeys :: [String]
accentKeys = ["\\softest", "\\verysoft", "\\soft", "", "\\hard", "\\veryhard", "\\hardest"]

softestString, verysoftString, softString, hardString, veryhardString, hardestString :: String
softestString  = "^\\markup {\\musicglyph #\"scripts.dmarcato\"}"
verysoftString = "^\\markup {\\musicglyph #\"scripts.upedaltoe\"}"
softString     = "^\\markup {<}"
hardString     = "^\\markup {>}"
veryhardString = "^\\markup {\\musicglyph #\"scripts.dpedaltoe\"}"
hardestString  = "^\\markup {\\musicglyph #\"scripts.umarcato\"}"

accentValues :: [String]
accentValues = [softestString, verysoftString, softString, hardString, veryhardString, hardestString]

renderAccentKeyValue :: String -> String -> Builder
renderAccentKeyValue key val = stringEncoding $ filter (/= '\\') key ++ " = " ++ val ++ "\n"

renderedAccentValues :: Builder
renderedAccentValues = mconcat $ zipWith renderAccentKeyValue (filter (/= "") accentKeys) accentValues

-- | Interpreting accents requires the emitting the list accent of symbols above.
renderAccent :: Accent -> Builder
renderAccent accent =  stringEncoding $ accentKeys !! fromEnum accent

-- | Match strings to enum: Pianissimo | Piano | MezzoPiano | MezzoForte | Forte | Fortissimo | Crescendo | EndCrescendo | Decrescendo | EndDecrescendo
renderedDynamicValues :: [Builder]
renderedDynamicValues = map stringEncoding ["\\pp", "\\p", "\\mp", "\\mf", "\\f", "\\ff", "\\<", "\\!", "\\>", "\\!"]

-- | Match of enum values to list above.  First Bool is flag to tell if
--   continuous dynamic control (cresc, decresc) is engaged.  Second is
--   for continuous pan control and gets passed through unchanged.
renderDynamic :: (Bool, Bool, Dynamic) -> (Bool, Bool, Builder)
renderDynamic (c, p, dynamic) =
  (c', p, if c then renderedStopTextSpan <> renderedDynamic else renderedDynamic)
  where
    renderedDynamic = renderedDynamicValues !! fromEnum dynamic
    c' = dynamic == Crescendo || dynamic == Decrescendo
    
-- | LeftBalance | MidLeftBalance | CenterBalance | MidRightBalance | RightBalance deriving (Bounded, Enum, Show, Ord, Eq)
balanceValues :: [String]
balanceValues = ["_\\markup {left}", "_\\markup {center-left}", "_\\markup {center}", "_\\markup {center-right}", "_\\markup {right}"] 

-- | Match of enum values to list above.
renderBalance :: Balance -> Builder
renderBalance balance = stringEncoding $ balanceValues !! fromEnum balance

-- | Pan is markup below staff.
renderPanValue :: Int -> Builder
renderPanValue val = stringEncoding "_\\markup" <> renderedOpen <> stringEncoding "pan " <> intDec val <> renderedClose

-- | First Bool is flag to tell if continuous dynamic control (cresc, decresc) is engaged,
--   gets passed through unchanged.  Second is for continuous pan control.
renderPan :: (Bool, Bool, Pan) -> (Bool, Bool, Builder)
renderPan (c, p, Pan pan) = if c then (False, p, renderedStopTextSpan <> renderPanValue pan) else (c, p, renderPanValue pan)
renderPan (_, p, PanUp)   = (True, p, stringEncoding "\\override TextSpanner.bound-details.left.text = \"pan up\"\\startTextSpan")
renderPan (_, p, PanDown) = (True, p, stringEncoding "\\override TextSpanner.bound-details.left.text = \"pan down\"\\startTextSpan")

renderTempo' :: Rhythm -> Integer -> Builder
renderTempo' (Rhythm rhythm) bpm =
  stringEncoding "\\tempo " <> integerDec denom <> stringEncoding " = " <> integerDec bpm
  where
    denom = denominator rhythm

-- | Bool is flag saing if contnous tempo control is engaged.
renderTempo :: (Bool, Tempo) -> (Bool, Builder)
renderTempo (_, Accelerando)       = (True, stringEncoding "\\override TextSpanner.bound-details.left.text = \"accel.\"\\startTextSpan")
renderTempo (_, Ritardando)        = (True, stringEncoding "\\override TextSpanner.bound-details.left.text = \"rit.\"\\startTextSpan")
renderTempo (s, Tempo rhythm bpm) = (False, if s then renderedStopTextSpan <> renderedTempo else renderedTempo) where renderedTempo = renderTempo' rhythm bpm

-- | Lilypond wants e.g. \key g \minor, but all I have from KeySignature is the count of sharps and flats.
--   From which I could deduce major or minor tonic pitches equally.  Curiously enough, though, from the
--   perspective of the key signature, the issue of tonic has no bearing!  So actually, the bare count of
--   sharps or flats suffices.  What I need is a way to say, e.g. for sharps, what's the pitch represented
--   by the count of sharps less two?  And for flats, what's the pitch for the count of flats plus one?
renderKeySignature :: KeySignature -> Builder
renderKeySignature (KeySignature accidentals') =
  stringEncoding "\\key " <> renderPitchName tonicPitch <> renderedSpace <> stringEncoding "\\major" <> renderedSpace
  where
    cIndex        = cycleOfFifthsIdx C
    tonicPitch    = cycleOfFifths !! (cIndex + accidentals')

-- | Lilypond time signature is just e.g. "\time 2/4"
renderTimeSignature :: TimeSignature -> Builder
renderTimeSignature (TimeSignature num den) =
  stringEncoding "\\time" <> renderedSpace <> integerDec num <> renderedSlash <> integerDec den <> renderedSpace

-- | Lilypond has "." for staccato, "^" for marcato, and slurs for legato, so I suppose "-" is really tenuto
--   e.g. with shorthand notation "-^" for marcato, "--" for tenuto, and "-." for staccato.  There's also
--   "-'" for staccatissimo, so why not?  And similarly for portato, "-_".
articulationValues :: [String]
articulationValues = ["", "--", "-_", "-^", "-.", "-'"]

-- | Map Articulation enums NoArticulation | Tenuto | Portato | Marcato | Staccato | Staccatissimo
--   to Lilypond strings.
renderArticulation :: Articulation -> Builder
renderArticulation articulation = stringEncoding $ articulationValues !! fromEnum articulation

-- | Render text below notes e.g. -"foo".  To go above, ^"foo"
renderText :: String -> Builder
renderText text = renderedDash <> renderedQuote <> stringEncoding text <> renderedQuote

-- | Combine a rendered pitch with a list of rendered rhythms into
--   a rendered note, annotating with ties where there are multiple
--   rhythms.
renderNoteForRhythms :: Builder -> Builder -> [Builder] -> Builder
renderNoteForRhythms _ _ [] = mempty
renderNoteForRhythms renderedPitch renderedControls [renderedRhythm] = renderedPitch <> renderedRhythm <> renderedControls 
renderNoteForRhythms renderedPitch renderedControls (renderedRhythm:renderedRhythms) =
  renderedPitch <> renderedRhythm <> renderedControls <> mconcat [renderedTie <> renderedSpace <> renderNoteForRhythms renderedPitch renderedNothing renderedRhythms]

-- | Emit Lilypond text corresponding to VoiceControl enum.
renderVoiceControl :: (Bool, Bool, VoiceControl) -> (Bool, Bool, Builder)
renderVoiceControl (c, p, DynamicControl dynamic)             = renderDynamic (c, p, dynamic)
renderVoiceControl (c, p, BalanceControl balance)             = (c, p, renderBalance balance)
renderVoiceControl (c, p, PanControl pan)                     = renderPan (c, p, pan)
renderVoiceControl (c, p, ArticulationControl articulation)   = (c, p, renderArticulation articulation)
renderVoiceControl (c, p, TextControl text)                   = (c, p, renderText text)
renderVoiceControl (c, p, AccentControl accent)               = (c, p, renderAccent accent)
renderVoiceControl (c, p, InstrumentControl instrument)       = (c, p, renderInstrument instrument)

-- | Emit a list list of space-separated controls.
renderVoiceControls' :: VoiceControl -> State (Bool, Bool) Builder
renderVoiceControls' control =
  do (c, p) <- get
     let (c', p', b) = renderVoiceControl (c, p, control)
     put (c', p')
     return b
    
renderVoiceControls :: (Bool, Bool) -> [VoiceControl] -> (Bool, Bool, Builder)
renderVoiceControls (c, p) controls =
  (c', p', mconcat bs)
  where
    (bs, (c', p')) = runState (traverse renderVoiceControls' controls) (c, p)

renderScoreControl :: (Bool, ScoreControl) -> (Bool, Builder)
renderScoreControl (s, TempoControl tempo)                 = renderTempo (s, tempo)
renderScoreControl (s, KeySignatureControl keySignature)   = (s, renderKeySignature  keySignature)
renderScoreControl (s, TimeSignatureControl timeSignature) = (s, renderTimeSignature timeSignature)

-- Bool in first Pair element is flag to say if accel, ritard are active so we
-- know to end text span when they stop.
renderScoreControls' :: ScoreControl -> State Bool Builder
renderScoreControls' control =
  do s <- get
     let (s', b) = renderScoreControl (s, control)
     put s'
     return b

renderScoreControls :: Bool -> [ScoreControl] -> Builder
renderScoreControls s controls = mconcat $ evalState (traverse renderScoreControls' controls) s

-- | Pitch doesn't matter when written to a percussion staff
dummyPercussionPitch :: Pitch
dummyPercussionPitch = Pitch C $ Octave (-1)

-- | Render Note instances.
renderNote :: (Bool, Bool) -> Note -> (Bool, Bool, Builder)
renderNote (c, p) (Note pitch rhythm controls) =
  (c', p', renderedNote)
  where
    (c', p', renderedVoiceControls) = renderVoiceControls (c, p) (Set.toAscList controls)
    renderedPitch  = renderPitch pitch
    renderedRhythms = renderRhythm rhythm
    renderedNote = renderNoteForRhythms renderedPitch renderedVoiceControls renderedRhythms
renderNote (c, p) (Rest rhythm controls) =
  (c', p', renderedNote)
  where
    (c', p', renderedVoiceControls) = renderVoiceControls (c, p) (Set.toAscList controls)
    renderedRhythms = renderRhythm rhythm
    renderedNote = renderNoteForRhythms renderedRest renderedVoiceControls renderedRhythms
renderNote (c, p) (PercussionNote rhythm controls) =
  (c', p', renderedNote)
  where
    (c', p', renderedVoiceControls) = renderVoiceControls (c, p) (Set.toAscList controls)
    renderedPitch = renderPitch dummyPercussionPitch
    renderedRhythms = renderRhythm rhythm
    renderedNote = renderNoteForRhythms renderedPitch renderedVoiceControls renderedRhythms

-- | Spaces separate notes in a rendered list of notes.
renderNotes' :: Note -> State (Bool, Bool) Builder 
renderNotes' note =
  do (c, p) <- get
     let (c', p', b) = renderNote (c, p) note
     put (c', p')
     return b
    
renderNotes :: [Note] -> Builder
renderNotes notes = mconcat $ intersperse renderedSpace $ evalState (traverse renderNotes' notes) (False, False)

-- | An instrument expects to be in a Staff or Voice context.
renderInstrument :: Instrument -> Builder
renderInstrument (Instrument instrumentName) =
  stringEncoding $ "\\set Staff.instrumentName = #\"" ++ instrumentName ++ "\""

-- | Start each voice with a block that enables automatic ties across bar lines.
renderedVoicePrefix :: Builder
renderedVoicePrefix = stringEncoding "\\new Voice \\with \n{\\remove \"Note_heads_engraver\" \\consists \"Completion_heads_engraver\" \\remove \"Rest_engraver\" \\consists \"Completion_rest_engraver\"}\n"

-- | Render the Voice type in Lilypond syntax.
renderVoice :: Voice -> Builder
renderVoice (Voice instrument notes) =
  renderedVoicePrefix
  <> renderedOpen
  <> renderInstrument instrument
  <> renderedSpace
  <> renderedGlobalKey
  <> renderedSpace
  <> renderNotes notes
  <> renderedSpace
  <> renderedDoubleBar
  <> renderedClose
  <> renderedNewline

-- | Render a list of Voice types in Lilypond syntax, no separators.
renderVoices :: [Voice] -> Builder
renderVoices [] = mempty
renderVoices (voice:voices) = renderVoice voice <> mconcat [renderVoices voices]

-- | Every Lilypond file should start with a version number.
renderedVersion :: Builder
renderedVersion = stringEncoding "\\version \"2.18.2\"\n"

-- | Global section contains tempo, time, and key signatures.
renderedGlobalValues :: [(ScoreControl,Rhythm)] -> Builder
renderedGlobalValues controls =
  stringEncoding "global = {"
  <> renderScoreControls False (map fst controls) -- tbd: rhythms
  <> renderedClose
  <> renderedNewline

renderedHeader :: String -> String -> Builder
renderedHeader title composer =
  stringEncoding "\\header {title = \""
  <> stringEncoding title
  <> stringEncoding "\" composer = \""
  <> stringEncoding composer
  <> renderedDoubleQuote
  <> renderedClose
  <> renderedNewline
  
renderScore :: Score -> Builder
renderScore (Score title composer controls voices) =
  renderedVersion
  <> renderedHeader title composer
  <> renderedGlobalValues controls
  <> renderedAccentValues
  <> stringEncoding "\\score {\n\\new StaffGroup << \n"
  <> renderVoices voices
  <> stringEncoding ">>\n\\layout { }\n\\midi { }\n"
  <> renderedClose
  <> renderedNewline

scoreToLilypondByteString :: Score -> L.ByteString
scoreToLilypondByteString = toLazyByteString . renderScore

scoreToLilypondFile :: Score -> IO ()
scoreToLilypondFile score = L.writeFile (scoreTitle score ++ ".ly") $ scoreToLilypondByteString score

