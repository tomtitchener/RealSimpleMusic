

module ScoreToLilypond.Utils where

import           RealSimpleMusic
--import qualified Data.ByteString.Lazy               as L
import           Data.ByteString.Builder
import           Data.List
import           Data.Ratio
import           Data.Maybe
import           Data.Monoid

equivPitchClasses :: [[PitchClass]]
equivPitchClasses = [[Cff, Cf, C, Cs, Css], [Dff, Df, D, Ds, Dss], [Eff, Ef, E, Es, Ess], [Fff, Ff, F, Fs, Fss], [Gff, Gf, G, Gs, Gss], [Aff, Af, A, As, Ass], [Bff, Bf, B, Bs, Bss]]

-- | Lilypond pitch names are lower-case
pitchNames :: [String]
pitchNames = ["c", "d", "e", "f", "g", "a", "b"]

findEquivPitchClassIndex :: PitchClass -> Int
findEquivPitchClassIndex pc =
  fromMaybe
    (error $ "findEquivPitchClassIndex no match for pitch class " ++ show pc ++ " in " ++ show equivPitchClasses)
    (findIndex (elem pc) equivPitchClasses)

-- | Data.ByteString.Builder provides alternative encodings: string7, string8, stringUtf8
--   Does Lilypond care?  Seems like string7 (ASCII) should suffice.
--   There don't seem to be non-ASCII characters in the documentation.
stringEncoding :: String -> Builder
stringEncoding = string7

-- | Data.ByteChar.Builder provides alternative encodings: char7, char8, charUtf8
charEncoding :: Char -> Builder
charEncoding = char7

renderPitchName :: PitchClass -> Builder
renderPitchName pc = stringEncoding $ pitchNames !! findEquivPitchClassIndex pc

-- | Lilypond accidental names are in Dutch
accidentalNames :: [String]
accidentalNames = ["eses", "es", "", "is", "isis"];

findEquivPitchClassAccidentalIndex :: PitchClass -> Int
findEquivPitchClassAccidentalIndex pc =
  fromMaybe
    (error $ "findEquivPitchClassAccidentalIndex no match for pitch class " ++ show pc ++ " in " ++ show pcs)
    (elemIndex pc pcs)
  where
    pcs = equivPitchClasses !! findEquivPitchClassIndex pc
    
renderAccidental :: PitchClass -> Builder
renderAccidental pc = stringEncoding $ accidentalNames !! findEquivPitchClassAccidentalIndex pc
  
renderPitchClass :: PitchClass -> Builder
renderPitchClass pc = renderPitchName pc <> renderAccidental pc

renderOctave :: Octave -> Builder
renderOctave (Octave octave) 
  | octave >= 0    = string7 $ replicate (octave + 1) '\''
  | octave == (-1) = string7 ""
  | otherwise      = string7 $ replicate (abs (octave + 1)) ','

renderPitch :: Pitch -> Builder
renderPitch (Pitch pitchClass octave) =
  renderPitchClass pitchClass <> renderOctave octave

renderRhythm :: Rhythm -> [Builder]
renderRhythm (Rhythm rhythm)
  | num > 1 && denom == 1 = replicate num $ intDec denom  -- TBD: longa, breve (if it matters)
  | num == 1              = [intDec denom]
  | num == 3              = [intDec (denom `div` 2) <> dot]
  | otherwise             = intDec remdenom : renderRhythm (Rhythm (rhythm - remain))
  where
    dot      = charEncoding '.'
    num      = fromIntegral $ numerator rhythm
    denom    = fromIntegral $ denominator rhythm
    remain   = fromMaybe
                 (error $ "renderRhythm no remainder for rhythm " ++ show rhythm)
                 (find (rhythm >) [1%1, 1%2, 1%4, 1%8, 1%16, 1%32, 1%64, 1%128, 1%256])
    remdenom = fromIntegral $ denominator remain

accentStrings :: [String]
accentStrings = ["\\pp", "\\p", "\\mp", "\\mf", "\\f", "\\ff", "\\fff"]

renderAccent :: Accent -> Builder
renderAccent accent =  stringEncoding $ accentStrings !! fromEnum accent 

renderNoteForRhythms :: Builder -> [Builder] -> Builder
renderNoteForRhythms _ [] = mempty
renderNoteForRhythms renderedPitch [renderedRhythm] = renderedPitch <> renderedRhythm
renderNoteForRhythms renderedPitch (renderedRhythm:renderedRhythms) =
  renderedPitch <> renderedRhythm <> mconcat [stringEncoding "~ " <> renderNoteForRhythms renderedPitch renderedRhythms]

renderAccentedNoteForRhythms :: Builder -> Builder -> [Builder] -> Builder
renderAccentedNoteForRhythms _ _ [] = mempty
renderAccentedNoteForRhythms renderedPitch renderedAccent [renderedRhythm] = renderedPitch <> renderedRhythm <> charEncoding ' ' <> renderedAccent
renderAccentedNoteForRhythms renderedPitch renderedAccent (renderedRhythm:renderedRhythms) =
  renderedPitch <> renderedRhythm <> charEncoding ' ' <> renderedAccent <> mconcat [stringEncoding "~ " <> renderAccentedNoteForRhythms renderedPitch renderedAccent renderedRhythms]

renderRestForRhythms :: [Builder] -> Builder
renderRestForRhythms [] = mempty
renderRestForRhythms [renderedRhythm] = charEncoding 'r' <> renderedRhythm
renderRestForRhythms (renderedRhythm:renderedRhythms) =
  charEncoding 'r' <> renderedRhythm <> mconcat [charEncoding ' ' <> renderRestForRhythms renderedRhythms]

-- Pitch doesn't matter when written to a percussion staff
dummyPercussionPitch :: Pitch
dummyPercussionPitch = Pitch C $ Octave (-1)

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

renderNotes :: [Note] -> Builder
renderNotes [] = mempty
renderNotes (note:notes) = renderNote note <> mconcat [ charEncoding ' ' <> renderNote note' | note' <- notes]

{-- WIP

{--
Use references and frames to avoid in-line nesting, allow for re-use of voices in score and per-voice parts.

  Voice assembly needs to know: key & time signatures, name,

  global = { \time 4/4 \key c \major }

  violaMusic = { \global \clef alto  g'1 b }
  violaVoice = \new Voice { \set Staff.instrumentName = #"Viola" \violaMusic } 

  Score assembly needs to know:  instrument families, sub-groupings (Vn I/Vn II + Va, Cello, Bass and etc. for brass and woodwinds).

  \score { 
  }

  \new Voice {
    \set Staff.instrumentName = #"Viola"
    \violaMusic
  }

--}

{--
  \new Staff = "Staff_viola" {
    \set Staff.instrumentName = #"Viola"
    \violaMusic
  }
--}

staffFrame :: String -> Builder
staffFrame instrument =
  stringEncoding "\\new Staff = \"Staff_" <> encodedInstrument <> stringEncoding " { \\set Staff.instrumentName = #\"" <> encodedInstrument <> stringEncoding "\\" <> encodedInstrument <> stringEncoding "Music }"
  where
    encodedInstrument = stringEncoding instrument

renderInstrument :: Instrument -> Builder
renderInstrument (Instrument name) =
  undefined
  
{--
Long notes which overrun bar lines can be converted automatically to tied notes. This is done by replacing the Note_heads_engraver with the Completion_heads_engraver.
Similarly, long rests which overrun bar lines are split automatically by replacing the Rest_engraver with the Completion_rest_engraver.
In the following example, notes and rests crossing the bar lines are split, notes are also tied.
     \new Voice \with {
       \remove "Note_heads_engraver"
       \consists "Completion_heads_engraver"
       \remove "Rest_engraver"
       \consists "Completion_rest_engraver"
      }
--}

voicePrefix :: Builder
voicePrefix = stringEncoding "\\new Voice \\with {\\remove \"Note_heads_engraver\" \\consists \"Completion_heads_engraver\" \\remove \"Rest_engraver\" \\consists \"Completion_rest_engraver\""

renderVoice :: Voice -> Builder
renderVoice (Voice instrument notes _) =
  voicePrefix <> renderInstrument instrument <> renderNotes notes <> charEncoding '}'

--}
