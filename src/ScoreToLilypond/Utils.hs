--bash-3.2$ grep " error " ./RealSimpleMusic/src/ScoreToLilypond/Utils.hs | wc -l
--       7

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
import           Data.Word
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
renderedQuote, renderedComma, renderedSpace, renderedOpen, renderedClose, renderedDot, renderedRest, renderedTie, renderedNewline, renderedDash, renderedSlash, renderedDoubleQuote, renderedAsterisk, renderedEmpty, renderedNothing, renderedDoubleBar, renderedStopTextSpan, renderedStartTextSpan, renderedClef, renderedBass, renderedTreble, renderedCloseSection, renderedEndDynamic, renderedOpenArrow, renderedCloseArrow :: Builder
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
renderedEmpty         = charEncoding 's'
renderedNothing       = stringEncoding ""
renderedDoubleBar     = stringEncoding "\\bar \"|.\""
renderedStopTextSpan  = stringEncoding "\\stopTextSpan"
renderedStartTextSpan = stringEncoding "\\startTextSpan"
renderedClef          = stringEncoding "\\clef"
renderedTreble        = stringEncoding "treble"
renderedBass          = stringEncoding "bass"
renderedCloseSection  = stringEncoding ">>"
renderedEndDynamic    = stringEncoding "\\!"
renderedOpenArrow     = stringEncoding "<<"
renderedCloseArrow    = stringEncoding ">>"

-- | Global reference sets key and time signatures.
renderedGlobalKey :: Builder
renderedGlobalKey = stringEncoding "\\global"

pitchClassToLilypondName :: PitchClass -> String
pitchClassToLilypondName Cff = "c"
pitchClassToLilypondName Cf  = "c"
pitchClassToLilypondName C   = "c"
pitchClassToLilypondName Cs  = "c"
pitchClassToLilypondName Css = "c"
pitchClassToLilypondName Dff = "d"
pitchClassToLilypondName Df  = "d"
pitchClassToLilypondName D   = "d"
pitchClassToLilypondName Ds  = "d"
pitchClassToLilypondName Dss = "d"
pitchClassToLilypondName Eff = "e"
pitchClassToLilypondName Ef  = "e"
pitchClassToLilypondName E   = "e"
pitchClassToLilypondName Es  = "e"
pitchClassToLilypondName Ess = "e"
pitchClassToLilypondName Fff = "f"
pitchClassToLilypondName Ff  = "f"
pitchClassToLilypondName F   = "f"
pitchClassToLilypondName Fs  = "f"
pitchClassToLilypondName Fss = "f"
pitchClassToLilypondName Gff = "g"
pitchClassToLilypondName Gf  = "g"
pitchClassToLilypondName G   = "g"
pitchClassToLilypondName Gs  = "g"
pitchClassToLilypondName Gss = "g"
pitchClassToLilypondName Aff = "a"
pitchClassToLilypondName Af  = "a"
pitchClassToLilypondName A   = "a"
pitchClassToLilypondName As  = "a"
pitchClassToLilypondName Ass = "a"
pitchClassToLilypondName Bff = "b"
pitchClassToLilypondName Bf  = "b"
pitchClassToLilypondName B   = "b"
pitchClassToLilypondName Bs  = "b"
pitchClassToLilypondName Bss = "b"

pitchClassToLilypondAccidental :: PitchClass -> String
pitchClassToLilypondAccidental Cff = "eses"
pitchClassToLilypondAccidental Cf  = "es"
pitchClassToLilypondAccidental C   = ""
pitchClassToLilypondAccidental Cs  = "is"
pitchClassToLilypondAccidental Css = "isis"
pitchClassToLilypondAccidental Dff = "eses"
pitchClassToLilypondAccidental Df  = "es"
pitchClassToLilypondAccidental D   = ""
pitchClassToLilypondAccidental Ds  = "is"
pitchClassToLilypondAccidental Dss = "isis"
pitchClassToLilypondAccidental Eff = "eses"
pitchClassToLilypondAccidental Ef  = "es"
pitchClassToLilypondAccidental E   = ""
pitchClassToLilypondAccidental Es  = "is"
pitchClassToLilypondAccidental Ess = "isis"
pitchClassToLilypondAccidental Fff = "eses"
pitchClassToLilypondAccidental Ff  = "es"
pitchClassToLilypondAccidental F   = ""
pitchClassToLilypondAccidental Fs  = "is"
pitchClassToLilypondAccidental Fss = "isis"
pitchClassToLilypondAccidental Gff = "eses"
pitchClassToLilypondAccidental Gf  = "es"
pitchClassToLilypondAccidental G   = ""
pitchClassToLilypondAccidental Gs  = "is"
pitchClassToLilypondAccidental Gss = "isis"
pitchClassToLilypondAccidental Aff = "eses"
pitchClassToLilypondAccidental Af  = "es"
pitchClassToLilypondAccidental A   = ""
pitchClassToLilypondAccidental As  = "is"
pitchClassToLilypondAccidental Ass = "isis"
pitchClassToLilypondAccidental Bff = "eses"
pitchClassToLilypondAccidental Bf  = "es"
pitchClassToLilypondAccidental B   = ""
pitchClassToLilypondAccidental Bs  = "b"
pitchClassToLilypondAccidental Bss = "isis"

-- | Map from e.g. Cff to "c".
renderPitchName :: PitchClass -> Builder
renderPitchName = stringEncoding . pitchClassToLilypondName

-- | Map e.g. Es to "is".
renderAccidental :: PitchClass -> Builder
renderAccidental = stringEncoding . pitchClassToLilypondAccidental

-- | Map e.g. "Bff" to "beses".
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
--   TBD:  constrain rhythm constructor so you cannot
--   create an instance that doesn't map to a Lilypond
--   value?  Similar sort of problem in ScoreToMidi but
--   divisor is smaller, ratio has to divide evenly to
--   512.  Doesn't seem unreasonable to constrain here
--   to something that can be printed, although could
--   have customizable constructor by resolution of
--   rendering target.  Real trick would be to have
--   type system allow e.g. only limited range of 
--   powers of two for denominator, e.g.
--   [1,2,4,8,16,32,64,128].  But then you'd always
--   be interpreting the type for this kind of
--   behavior, e.g. rendering Peano cardinals as 
--   integers.
renderRhythm :: Rhythm -> [Builder]
renderRhythm (Rhythm start)
  | startNum == 0                   = [] 
  | startNum > 1 && startDenom == 1 = [(intDec . fromIntegral) startDenom <> renderedAsterisk <> (intDec . fromIntegral) startNum]  -- replicate num $ intDec denom 
  | startNum == 1                   = [(intDec . fromIntegral) startDenom]
  | startNum == 3                   = [intDec (fromIntegral startDenom `div` 2) <> renderedDot]
  | otherwise                       = intDec (fromIntegral (denominator unit)) : renderRhythm (Rhythm remain)
  where
    startNum   = numerator start
    startDenom = denominator start
    unit       = fromMaybe
                 (error $ "renderRhythm no remainder for rhythm " ++ show start)
                 (find (start >) [1%1, 1%2, 1%4, 1%8, 1%16, 1%32, 1%64, 1%128])
    remain = start - unit

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

-- | Match strings to enum: Pianissimo | Piano | MezzoPiano | MezzoForte | Forte | Fortissimo | Crescendo | Decrescendo 
renderedDynamicValues :: [Builder]
renderedDynamicValues = map stringEncoding ["\\pp", "\\p", "\\mp", "\\mf", "\\f", "\\ff", "\\<", "\\>"]

-- Snippet:  "<<g'1*2-_\hard{s2\ff\> s1\p\< s2\ff}>>", generated by:
-- (Rhythm (2%1)) [(Fortissimo,0),(Decrescendo,1),(Piano,0),(Crescendo,2),(Fortissimo,1)]
-- result for this method one or maybe two dynamic annotations from bits within "{" and "}",
-- e.g. from {s2\ff\> s1\p\< s2\ff}, just "s2\ff\> ", s1\p\< ", or "s2\ff ".
-- Buffer 0 duration events to follow space keyword for first non-zero duration event.
-- Not allowed to have two 0 duration events in a row.
-- Note that FractionalDynamic cannot end with zero-duration event, e.g.:
-- [(Crescendo,1),(Fortissimo,0),(Decrescendo,2),(Piano,0)].  Just like it'll
-- have to be ok to terminate a discrete, continuous dynamic with a fractional
-- dynamic that begins with a non-continuous, discrete value, it'll also be ok
-- to terminate a continuous, discrete dynamic that concludes a fractional dynamic
-- with a new non-continuous discrete dynamic, or, I suppose, with a new fractional
-- dynamic that begins with a non-continuous discrete dynamic.  Ugh.
renderFractionalDynamic' :: Rational -> Builder -> (DiscreteDynamicValue, Word) -> (Builder,Builder)
renderFractionalDynamic' unit oldRenderedDynamic (dynamic, fraction) 
  | fraction == 0 && notNothing oldRenderedDynamic = error errorText
  | fraction == 0                                  = (newRenderedDynamic, renderedNothing)
  | otherwise                                      = (renderedNothing,    renderedFraction)
  where
    errorText           = "renderFractionalDynamic sequential zero duration dynamics " ++ show (toLazyByteString oldRenderedDynamic) ++ " , " ++ show dynamic
    notNothing rendered = toLazyByteString rendered /= toLazyByteString renderedNothing
    newRenderedDynamic  = renderedDynamicValues !! fromEnum dynamic 
    renderedFraction    = renderSpace (Rhythm (unit * fromIntegral fraction)) <> oldRenderedDynamic <> newRenderedDynamic <> renderedSpace

renderFractionalDynamic :: Rational -> (DiscreteDynamicValue, Word) -> State Builder Builder
renderFractionalDynamic unit fraction =
  get >>= \old -> let (old', new) = renderFractionalDynamic' unit old fraction in put old' >> return new

-- | Ignore trailing zero-duration event, e.g. termination of crescendo or decrescendo.
--   Lilypond doesn't allow e.g. {s1*2\< s2\ff\>\p}, which seems like a bug.
--   NB:  unit has to have a divisor that's a power of 2!  But here we divide rhythm,
--   with a denominator we know to be a power of 2 by some random value.  For example,
--   (1%1)/3 == 1%3, which isn't allowed.  Or (1%4)/3 == 1%12.  To turn e.g. 1%12 into
--   X%(some power of 2) requires finding a power of 2 that can be exactly divided by 12!
--   And hoping that Lilypond won't barf when you tell it something totally wacky like
--   s2048*4.  For Midi rendering, we've already moved into the land of durations, where
--   it's much easier to handle this.  But when it comes to Lilypond, that total value had
--   better be a power of 2 or there's just no hope.  So when we see a unit that's not a
--   power of 2, don't even try to render a fractional dynamic.
renderFractionalDynamics :: Rhythm -> [(DiscreteDynamicValue, Word)] -> Builder
renderFractionalDynamics (Rhythm rhythm) fractions
  | unit `elem` [2,4,8,16,32] = renderedNothing
  | otherwise                 = renderedOpen <> mconcat renderedFractions <> renderedClose
  where
    total             = sum $ map snd fractions
    unit              = rhythm / fromIntegral total
    renderedFractions = evalState (traverse (renderFractionalDynamic unit) fractions) renderedNothing
    
-- | Match of enum values to list above.  First Bool is flag to tell if
--   continuous dynamic control (cresc, decresc) is engaged.  Second is
--   for continuous pan control and gets passed through unchanged.
renderDynamic :: Bool -> Bool -> Rhythm -> Dynamic -> (Bool, Bool, Builder)
renderDynamic True  _ _ (FractionalDynamic _)              = error "renderDynamic FractionalDynamic inside crescendo or decrescendo"
renderDynamic False p rhythm (FractionalDynamic fractions) = (False, p, renderFractionalDynamics rhythm fractions)
renderDynamic True  _ _ (DiscreteDynamic Crescendo)        = error "renderDynamic Cresecendo inside crescendo or decrescendo"
renderDynamic False p _ (DiscreteDynamic Crescendo)        = (True,  p, renderedDynamicValues !! fromEnum Crescendo)
renderDynamic True  _ _ (DiscreteDynamic Decrescendo)      = error "renderDynamic Decrescendo inside crescendo or decrescendo"
renderDynamic False p _ (DiscreteDynamic Decrescendo)      = (True,  p, renderedDynamicValues !! fromEnum Decrescendo)
renderDynamic False p _ (DiscreteDynamic dynamic)          = (False, p, renderedDynamicValues !! fromEnum dynamic)
renderDynamic True  p _ (DiscreteDynamic dynamic)          = (False, p, renderedEndDynamic <> renderedDynamicValues !! fromEnum dynamic)

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
renderPan (d, p, Pan (PanVal pan)) = if p then (d, p, renderedStopTextSpan <> renderPanValue pan) else (d, p, renderPanValue pan)
renderPan (d, _, PanUp)            = (d, True, stringEncoding "\\override TextSpanner.bound-details.left.text = \"pan up\"\\startTextSpan")
renderPan (d, _, PanDown)          = (d, True, stringEncoding "\\override TextSpanner.bound-details.left.text = \"pan down\"\\startTextSpan")

renderTempo' :: Rhythm -> Integer -> Builder
renderTempo' (Rhythm rhythm) bpm =
  stringEncoding "\\tempo " <> integerDec (denominator rhythm) <> stringEncoding " = " <> integerDec bpm

-- | Bool is flag saing if continous tempo control is engaged.
renderTempo :: (Bool, Tempo) -> (Bool, Builder)
renderTempo (False, Accelerando)                = (True, stringEncoding "\\override TextSpanner.bound-details.left.text = \"accel.\"")
renderTempo (True,  Accelerando)                = error "renderTempo two continuous controls in a row without intervening discrete control"
renderTempo (False, Ritardando)                 = (True, stringEncoding "\\override TextSpanner.bound-details.left.text = \"rit.\"")
renderTempo (True,  Ritardando)                 = error "renderTempo two continuous controls in a row without intervening discrete control"
renderTempo (_,     Tempo (TempoVal denom bpm)) = (False, renderTempo' (Rhythm (1 % rhythmDenomToInteger denom)) bpm)

-- | Lilypond wants e.g. \key g \minor, but all I have from KeySignature is the count of sharps and flats.
--   From which I could deduce major or minor tonic pitches equally.  Curiously enough, though, from the
--   perspective of the key signature, the issue of tonic has no bearing!  So actually, the bare count of
--   sharps or flats suffices.  What I need is a way to say, e.g. for sharps, what's the pitch represented
--   by the count of sharps less two?  And for flats, what's the pitch for the count of flats plus one?
renderKeySignature :: KeySignature -> Builder
renderKeySignature (KeySignature accidentals') =
  stringEncoding "\\key " <> renderPitchName tonicPitch <> renderedSpace <> stringEncoding "\\major" <> renderedSpace
  where
    cIndex        = fromEnum C
    tonicPitch    = cycleOfFifths !! (cIndex + accidentals')

-- | Lilypond time signature is just e.g. "\time 2/4"
renderTimeSignature :: TimeSignature -> Builder
renderTimeSignature (TimeSignature num den) =
  stringEncoding "\\time" <> renderedSpace <> integerDec num <> renderedSlash <> integerDec den <> renderedSpace

-- | Lilypond has "." for staccato, "^" for marcato, and slurs for legato, so I suppose "-" is really tenuto
--   e.g. with shorthand notation "-^" for marcato, "--" for tenuto, and "-." for staccato.  There's also
--   "-'" for staccatissimo, so why not?  And similarly for portato, "-_".
articulationValues :: [String]
articulationValues = ["", "--", "-_", "-^", "-.", "-!"]

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
renderNoteForRhythms :: Builder -> Builder -> Builder -> [Builder] -> Builder
renderNoteForRhythms _ _ _ [] = mempty
renderNoteForRhythms _ renderedPitch renderedControls [renderedRhythm] = renderedPitch <> renderedRhythm <> renderedControls 
renderNoteForRhythms renderedTie' renderedPitch renderedControls (renderedRhythm:renderedRhythms) =
  renderedPitch <> renderedRhythm <> renderedControls <> mconcat [renderedTie' <> renderedSpace <> renderNoteForRhythms renderedTie' renderedPitch renderedNothing renderedRhythms]

-- | Emit Lilypond text corresponding to VoiceControl enum.
renderVoiceControl ::  (Bool, Bool, Rhythm, VoiceControl) -> (Bool, Bool, Builder)
renderVoiceControl (c, p, r, DynamicControl dynamic)             = renderDynamic c p r dynamic
renderVoiceControl (c, p, _, BalanceControl balance)             = (c, p, renderBalance balance)
renderVoiceControl (c, p, _, PanControl pan)                     = renderPan (c, p, pan)
renderVoiceControl (c, p, _, ArticulationControl articulation)   = (c, p, renderArticulation articulation)
renderVoiceControl (c, p, _, TextControl text)                   = (c, p, renderText text)
renderVoiceControl (c, p, _, AccentControl accent)               = (c, p, renderAccent accent)
renderVoiceControl (c, p, _, InstrumentControl instrument)       = (c, p, renderInstrument instrument)
renderVoiceControl (c, p, _, KeySignatureControl keySignature)   = (c, p, renderKeySignature keySignature)
renderVoiceControl (c, p, _, TimeSignatureControl timeSignature) = (c, p, renderTimeSignature timeSignature)

-- | Emit a list list of space-separated controls.
renderVoiceControls' :: Rhythm -> VoiceControl -> State (Bool, Bool) Builder
renderVoiceControls' rhythm control =
  get >>= \(c, p) -> let (c', p', b) = renderVoiceControl (c, p, rhythm, control) in put (c', p') >> return b
     
renderVoiceControls :: (Bool, Bool, Rhythm) -> [VoiceControl] -> (Bool, Bool, Builder)
renderVoiceControls (c, p, rhythm) controls =
  (c', p', mconcat bs)
  where
    (bs, (c', p')) = runState (traverse (renderVoiceControls' rhythm) controls) (c, p)

renderSpacesForRhythms :: [Builder] -> Builder
renderSpacesForRhythms []                               = mempty
renderSpacesForRhythms [renderedRhythm]                 = renderedEmpty <> renderedRhythm
renderSpacesForRhythms (renderedRhythm:renderedRhythms) = renderedEmpty <> renderedRhythm <> mconcat [renderedSpace <> renderSpacesForRhythms renderedRhythms]

renderSpace :: Rhythm -> Builder
renderSpace rhythm = renderSpacesForRhythms (renderRhythm rhythm)

-- | Bool in first Pair element is flag to say if accel or ritard is active so we
--   know to end text span when they stop.
renderTempoAndRhythm :: Bool -> Tempo -> Rhythm -> (Bool, Builder)
renderTempoAndRhythm s tempo rhythm =
  (s', b')
  where
    (s', b) = renderTempo (s, tempo)                           -- b = \tempo 4 = 200 <or> \override TextSpanner.bound-details.left.text = "accel."
    spaces  = renderSpacesForRhythms (renderRhythm rhythm)
    b'
      | not s && s'     = b <> spaces <> renderedStartTextSpan -- b''= \override TextSpanner.bound-details.left.text = "accel." s1*10 \startTextSpan
      | s     && not s' = b <> spaces <> renderedStopTextSpan  -- b'' = \b'= \tempo 4 = 200 s1*10 \stopTextSpan
      | otherwise       = b <> spaces
    
renderTempoControl :: (Tempo, Rhythm) -> State Bool Builder
renderTempoControl (tempo, rhythm) =
  get >>= \s -> let (s', b) = renderTempoAndRhythm s tempo rhythm in put s' >> return b
     
renderTempoControls :: [(Tempo,Rhythm)] -> Builder
renderTempoControls controls =
  renderedOpen
  <> mconcat (evalState (traverse renderTempoControl controls) False)
  <> renderedClose
  <> renderedNewline

-- | Pitch doesn't matter when written to a percussion staff
dummyPercussionPitch :: Pitch
dummyPercussionPitch = Pitch C $ Octave (-1)

-- | Render Note instances.
renderNote :: (Bool, Bool) -> Note -> (Bool, Bool, Builder)
renderNote (c, p) (Note pitch rhythm controls) =
  (c', p', renderedNote)
  where
    (c', p', renderedVoiceControls) = renderVoiceControls (c, p, rhythm) (Set.toAscList controls)
    renderedPitch  = renderPitch pitch
    renderedRhythms = renderRhythm rhythm
    renderedNote = renderNoteForRhythms renderedTie renderedPitch renderedVoiceControls renderedRhythms
renderNote (c, p) (Rest rhythm controls) =
  (c', p', renderedNote)
  where
    (c', p', renderedVoiceControls) = renderVoiceControls (c, p, rhythm) (Set.toAscList controls)
    renderedRhythms = renderRhythm rhythm
    renderedNote = renderNoteForRhythms renderedNothing renderedRest renderedVoiceControls renderedRhythms
renderNote (c, p) (PercussionNote rhythm controls) =
  (c', p', renderedNote)
  where
    (c', p', renderedVoiceControls) = renderVoiceControls (c, p, rhythm) (Set.toAscList controls)
    renderedPitch = renderPitch dummyPercussionPitch
    renderedRhythms = renderRhythm rhythm
    renderedNote = renderNoteForRhythms renderedTie renderedPitch renderedVoiceControls renderedRhythms

isAnyFractionalDynamicControl :: VoiceControl -> Bool
isAnyFractionalDynamicControl (DynamicControl (FractionalDynamic _)) = True
isAnyFractionalDynamicControl _ = False

containsFractionalDynamicControl :: Set.Set VoiceControl -> Bool
containsFractionalDynamicControl controls = not (Set.null (Set.filter isAnyFractionalDynamicControl controls))

noteToControls :: Note -> Set.Set VoiceControl
noteToControls (Note _ _ controls)           = controls
noteToControls (Rest _ controls)             = controls
noteToControls (PercussionNote _ controls)   = controls

renderNoteFrame :: (Bool, Bool) -> Note -> (Bool, Bool, Builder)
renderNoteFrame pr note =
  if containsFractionalDynamicControl (noteToControls note)
  then
    (c, p, renderedOpenArrow <> renderedNote <> renderedCloseArrow)
  else
    (c, p, renderedNote)
  where
    (c, p, renderedNote) = renderNote pr note

-- | Spaces separate notes in a rendered list of notes.
renderNotes' :: Note -> State (Bool, Bool) Builder 
renderNotes' note =
  get >>= \(c, p) -> let (c', p', b) = renderNoteFrame (c, p) note in put (c', p') >> return b
    
renderNotes :: [Note] -> Builder
renderNotes notes = mconcat $ intersperse renderedSpace $ evalState (traverse renderNotes' notes) (False, False)

noteToPitch :: Note -> Maybe Pitch
noteToPitch (Note pitch _ _)     = Just pitch
noteToPitch (Rest _ _)           = Nothing
noteToPitch (PercussionNote _ _) = Nothing

mapPair :: (a -> b) -> (a, a) -> (b, b)
mapPair f (x, y) = (f x, f y)

-- Scan range of [Note] to determine "\clef treble", "\clef bass", "\clef treble_8", "\clef bass^8", "\clef treble_15", "\clef bass^15",  and etc.
renderClef :: [Note] -> Builder
renderClef notes
  | treblesUp  > treblesDown = renderedClef <> renderedSpace <> renderedTreble
  | bassesDown > bassesUp    = renderedClef <> renderedSpace <> renderedBass
  | otherwise                = error $ "renderClef split range, trebles up " ++ show treblesUp ++ " down " ++ show treblesDown ++ " basses down " ++ show bassesDown ++ " up " ++ show bassesUp
  where
    pitches = mapMaybe noteToPitch notes
    (treblesDown, treblesUp) = mapPair length $ partition (< Pitch F (Octave (-1))) pitches
    (bassesDown,  bassesUp)  = mapPair length $ partition (< Pitch G (Octave 1))    pitches

-- | An instrument expects to be in a Staff or Voice context.
renderInstrument :: Instrument -> Builder
renderInstrument (Instrument instrumentName) =
  stringEncoding $ "\\set Staff.instrumentName = #\"" ++ instrumentName ++ "\""

-- | Start each voice with a block that enables automatic ties across bar lines.
renderedVoicePrefix :: Builder
renderedVoicePrefix = stringEncoding "\\new Voice \\with \n{\\remove \"Note_heads_engraver\" \\consists \"Completion_heads_engraver\" \\remove \"Rest_engraver\" \\consists \"Completion_rest_engraver\"}\n<<\n"

-- | Render the Voice type in Lilypond syntax.
renderVoice :: [(Tempo, Rhythm)] -> Voice -> Builder
renderVoice tempoControls (Voice instrument notes) =
  renderedVoicePrefix
  <> renderTempoControls tempoControls
  <> renderedOpen
  <> renderInstrument instrument
  <> renderedSpace
  <> renderedGlobalKey
  <> renderedSpace
  <> renderClef notes
  <> renderedSpace
  <> renderNotes notes
  <> renderedSpace
  <> renderedDoubleBar
  <> renderedClose
  <> renderedNewline
  <> renderedCloseSection
  <> renderedNewline

-- | Render a list of Voice types in Lilypond syntax, no separators.
renderVoices :: [(Tempo, Rhythm)] -> [Voice] -> Builder
renderVoices _ []                         = mempty
renderVoices tempoControls (voice:voices) = renderVoice tempoControls voice <> mconcat [renderVoices tempoControls voices]

-- | "Every Lilypond file should start with a version number."
renderedVersion :: Builder
renderedVersion = stringEncoding "\\version \"2.18.2\"\n"

-- | Global section contains tempo, time, and key signatures.
renderedGlobalValues :: KeySignature -> TimeSignature -> Builder
renderedGlobalValues keySignature timeSignature =
  stringEncoding "global = {"
  <> renderKeySignature keySignature
  <> renderedSpace
  <> renderTimeSignature timeSignature
  <> renderedSpace
  <> renderedSpace
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
renderScore (Score title composer (ScoreControls keySignature timeSignature tempoControls) voices) =
  renderedVersion
  <> renderedHeader title composer
  <> renderedGlobalValues keySignature timeSignature
  <> renderedAccentValues
  <> stringEncoding "\\score {\n\\new StaffGroup << \n"
  <> renderVoices tempoControls voices
  <> stringEncoding ">>\n\\layout { }\n\\midi { }\n"
  <> renderedClose
  <> renderedNewline

scoreToLilypondByteString :: Score -> L.ByteString
scoreToLilypondByteString = toLazyByteString . renderScore

scoreToLilypondFile :: Score -> IO ()
scoreToLilypondFile score = L.writeFile (scoreTitle score ++ ".ly") $ scoreToLilypondByteString score

