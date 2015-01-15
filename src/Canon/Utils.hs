-- | Canons to explore RealSimpleMusic

module Canon.Utils where

import           Control.Applicative
import           Data.Ratio
import           Data.List
import           RealSimpleMusic

-- | Simplest of all Canons.  Imitation at unison, all voices
--   playing the same instrument.  Parameterized by title,
--   tune, imitative distance, instrument, count of voices,
--   count of repetitions.
data SimpleCanon = SimpleCanon
                   {sTitle       :: Title
                   ,sNotes       :: NoteMotto
                   ,sDistance    :: Rhythm
                   ,sInstrument  :: Instrument
                   ,sVoices      :: Int
                   ,sRepetitions :: Int
                   } deriving (Show)

-- | Render a simple canon as a Midi voices score
--  (no percussion).  Count of voices less than
--  Midi max (16).  Skip standard statement of
--  theme in unison voices before imitation.
simpleCanonToScore :: SimpleCanon -> Score
simpleCanonToScore (SimpleCanon title (Motto notes) (Rhythm dist) instrument countVoices repetitions) =
  Score title voices
  where
    tune = (concat . replicate repetitions) notes
    rests = take countVoices $ map (Rest . Rhythm) [(0%1)*dist, (1%1)*dist..]
    voices = zipWith (\rest pan -> Voice instrument (rest : tune) [[pan]]) rests pans
    incr = getPan (maxBound::Pan) `div` countVoices
    pans = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]

-- | Additionally parameterize by imitative interval, list of instruments
data TransposingCanon = TransposingCanon
                        {xpTitle       :: Title
                        ,xpNotes       :: NoteMotto
                        ,xpDistance    :: Rhythm
                        ,xpScale       :: Scale
                        ,xpIntervals   :: [Interval]
                        ,xpInstruments :: [Instrument]
                        ,xpRepetitions :: Int
                        } deriving (Show)

-- | Refactored for common behavior between transposingCanonToScore and commonCanonToScore.
assembleVoices :: [Rhythm] -> Int -> [[Note]] -> [Instrument] -> [Voice]
assembleVoices rhythms repetitions tuness instruments =
  zipWith3 makeVoice instruments restTuness pans
  where
    makeVoice instrument tunes pan = Voice instrument tunes [[pan]]
    dists = map getRhythm rhythms
    rests = map (Rest . Rhythm) [sum (take x dists) | x <- [0,1..]]
    repTuness =  map (concat . replicate repetitions) tuness
    restTuness = zipWith (:) rests repTuness
    incr = getPan (maxBound::Pan) `div` length instruments
    pans = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]

-- | Render a transposing canon as a Midi voices score (no percussion).
transposingCanonToScore :: TransposingCanon -> Score
transposingCanonToScore (TransposingCanon title noteMotto dist scale intervals instruments repetitions)
  | lenIntervals /= length instruments =
      error $ "transposingCanonToScore mismatched intervals " ++ show intervals ++ " vs. instruments " ++ show instruments
  | otherwise =
      Score title voices
      where
        lenIntervals = length intervals
        xposeTune interval = getMotto (transposeNoteMotto scale interval noteMotto)
        tuness = map xposeTune intervals
        dists = replicate lenIntervals dist
        voices = assembleVoices dists repetitions tuness instruments
  
-- Refactored for common behavior between scalesCanonToScore and rhythmCanonToScore
commonCanonToScore :: Title -> [Interval] -> [Rhythm] -> [Rhythm] -> [Scale] -> [Octave] -> [Instrument] -> Int -> Score
commonCanonToScore title intervals rhythms distances scales octaves instruments repetitions 
  | lenScales /= length instruments || lenScales /= length octaves || lenScales /= length distances =
      error $ "commonCanonToScore mismatched length of scales " ++ show scales ++ " vs. length of instruments " ++ show instruments ++ " vs. length of octaves " ++ show octaves ++ " vs. length of distances " ++ show distances
  | length intervals /= length rhythms =
      error $ "commonCanonToScore mismatched length of intervals " ++ show intervals ++ "vs. lengths of rhythms " ++ show rhythms
  | not (all (== lenHeadScale) lensTailScales) = 
      error $ "commonCanonToScore scales not all equal length" ++ show scales
  | otherwise =
      Score title voices
      where
        lenScales = length scales
        lenHeadScale = length (head scales)
        lensTailScales = map length (tail scales)
        genPitches scale octave = map (getPitch scale octave) intervals
        genTune scale octave = zipWith Note (genPitches scale octave) rhythms
        tuness = zipWith genTune scales octaves
        voices = assembleVoices distances repetitions tuness instruments

-- | Additionally parameterize by scale.  Tune becomes lists of rhythms 
--   and of intervals for mapping over scale.  Parameterize by lists 
--   of scale, octave  tranposition for scale root, and instruments
--   per voice.
data ScalesCanon = ScalesCanon
                   {scTitle       :: Title
                   ,scIntervals   :: [Interval]
                   ,scRhythms     :: [Rhythm]
                   ,scDistance    :: Rhythm
                   ,scScales      :: [Scale]
                   ,scOctaves     :: [Octave]
                   ,scInstruments :: [Instrument]
                   ,scRepetitions :: Int
                   } deriving (Show)

-- | Render a scales canon as a Midi voices score (no percussion).
scalesCanonToScore :: ScalesCanon -> Score
scalesCanonToScore (ScalesCanon title intervals rhythms dist scales octaves instruments repetitions) =
  commonCanonToScore title intervals rhythms dists scales octaves instruments repetitions
  where
    lenScales = length scales
    dists = replicate lenScales dist

-- | Additionally parameterize by imitative distance.
data Canon = Canon
             {cTitle       :: Title
             ,cRepetitions :: Int
             -- these lists must have the same lengths,
             -- can that be part of the type definition?
             ,cIntervals   :: [Interval]
             ,cRhythms     :: [Rhythm]
             -- these lists must have the same lengths,
             -- can that be part of the type definition?
             ,cDistances   :: [Rhythm]
             ,cScales      :: [Scale]
             ,cOctaves     :: [Octave]
             ,cInstruments :: [Instrument]
             } deriving (Show)

canonToScore :: Canon -> Score
canonToScore (Canon title repetitions intervals rhythms dists scales octaves instruments) =
  commonCanonToScore title intervals rhythms dists scales octaves instruments repetitions

-- Titles of first canon is title of score.
-- Titles of inner canons can be meta-data text events.
newtype SequentialCompoundCanon = SequentialCompoundCanon { getCanons :: [Canon] } deriving (Show)

sequentialCompoundCanonToScore :: SequentialCompoundCanon -> Score
sequentialCompoundCanonToScore  =
  undefined
  
-- | Instead of repeating same parameters for each repetition,
--   map each repetition by new parameters.  Simplify name, as
--   all previous canons can be rewritten using this.
data CompoundCanon = CompoundCanon
                     {canTitle        :: Title
                     ,canIntervals    :: [[Interval]]
                     ,canRhythms      :: [[Rhythm]]
                     ,canDistancess   :: [[Rhythm]]
                     ,canScaless      :: [[Scale]] 
                     ,canOctavess     :: [[Octave]]
                     ,canInstrumentss :: [[Instrument]]
                     } deriving (Show)

notesToRhythm :: [Note] -> Rhythm
notesToRhythm notes = Rhythm total
  where
    rhythms = map noteToRhythm notes
    ratios = map getRhythm rhythms
    total = sum ratios

addInstrumentControlToVoice :: Voice -> Voice
addInstrumentControlToVoice (Voice instrument notes controlss) =
  Voice instrument notes (controlss ++ [[instrumentControl]])
  where
    instrumentControl = InstrumentControl instrument (notesToRhythm notes)
    
appendVoice :: Voice -> Voice -> Voice
appendVoice (Voice i1 ns1 ctrlss1) (Voice i2 ns2 ctrlss2) =
  Voice i1 (ns1 ++ ns2) ctrlss
  where
    instrumentControl = InstrumentControl i2 (notesToRhythm ns2)
    ctrlss = zipWith (++) (sort ctrlss1) (sort (ctrlss2 ++ [[instrumentControl]]))

-- | Collapse a list of scores into one score.
--   Make title of first score title of returned score.
--   Then append voices in each of tail of list of scores
--   to voices in head of list of score.

-- Doesn't work!  Imitative distance for first canon fixes
-- overlap of voices for all subsequent canons.  If first
-- canon has a one measure overlap, then voices of second
-- canon begin at one measure distance as well.  Say second
-- canon has quarter note distance.  First voice of second
-- canon starts right away with no rest.  Second voice of
-- second canon starts one measure plus one quarter note
-- rest later.  And etc. for subsequent voices, e.g. at
-- offsets of one, two, three, etc. measures plus one, two
-- three etc. quarter notes.  If I want successive canons,
-- then I really need for last voice of previous canon to
-- spin down before starting new canon in all voices from
-- the same rhythmic starting point.
    
-- But that's not really what I wanted, which was a 
-- continuous, full-voice texture where the parameters
-- of the canon vary the texture without a sense of
-- starting and stopping.  That gets back to the sense
-- of a steady state, a point in which all voices are
-- engaged in the canon in some rhythmic unit.  Extract
-- that and then it can be juxtaposed, canon to canon
-- without a pause.

-- Note it may be the canon body saturates the original
-- melody, as happens with Frere Jacques at an imitative
-- distance of two measures.  You get a distillation of 
-- the texture with all four voices jumping between two
-- measure segments and it's the same thing over and over,
-- in units of two measures each:    
    
--            |   |   |   |   |   |
-- a a b b c c d d a a b b c c d d   
--     a a b b c c d d a a b b c c d d
--         a a b b c c d d a a b b c c d d
--             a a b b c c d d a a b b c c d d
--            |   |   |   |   |   |
    
-- But change the imitative distance from two measures
-- to one and you get a much longer unit of steady state
-- texture where the units last 8 measures instead.

--      |               |             |
-- a a b b c c d d a a b b c c d d a b b c c d d   
--   a a b b c c d d a a b b c c d d a a b b c c d d
--     a a b b c c d d a a b b c c d d a a b b c c d d
--       a a b b c c d d a a b b c c d d a a b b c c d d
--      |               |             |

-- In the second case, the unit of repetition equals the
-- duration of the tune.  In the first it's equal to just
-- a quarter of the tune.  The first reduces in musical
-- effect to the same thing repeated four times.  It may
-- just be too troublesome to try to capture and manipulate
-- that, programmatically.

-- Either way, I have to solve the scheduling of parameters
-- such that the new canon exhibits the proper relationship
-- between voices.  Note: is it just the imitative distance
-- that causes trouble?  What about the lead-in/lead-out
-- portions of the canon?  Note that with equal imitative
-- distances, successive canons would interlace voice-by-voice
-- with no drop in the count of voices at all, which seems
-- desireable.

-- One exception would be the count of voices. Do I want 
-- to allow for adding and dropping of voices?  That might
-- be a nice behavior.  It would manifest itself as a new
-- element in e.g. the list of list of scales where one
-- inner list might have 5 scales and the next 6.  Note that
-- for this to work, each of the lists that drive the count
-- of voices--distance, scale, octave, instrument--would have
-- to vary in sync with each other.

-- And, finally, what I've done in the past is to vary the
-- canon pitches themselves, or maybe just the accents or
-- rhythms.  So I'd want a way to schedule changes there as
-- well.    

-- Note regarding imitative distances that the trick is going
-- to be to shorten the run of the second, third, and etc.
-- voices at the point of the overlap.  Say the first distance
-- is four quarters and the second is one quarter.  If the
-- canon has two voices and goes on for two measures, then this
-- is what things look like at the end of the first statement.
-- The second voice has just finished its first full run of
-- the two measure phrase, starting four quarters after the
-- first:

-- |       |       |       |       |
-- 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4
-- a b c d e f g h a b c d e f g h a b c d
--         a b c d e f g h a b c d e f g h

-- Now let's say the next canon starts, with the same
-- two measure tune, but at an imitative distance of
-- one quarter.

-- |       |       |       |       |       |       |
-- 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4
-- a b c d e f g h a b c d e f g h a b c d a b c d e f g h
--         a b c d e f g h a b c d e f g h a a b c d e f g h

-- The first voice just launches, like it always does, with
-- no rest.  The second voice starts its ordinary overlap
-- e.g. of four quarters.  But as the new imitative distance
-- is one quarter, it stops after one quarter and restarts
-- from the beginning, going "a a b c" in that first measure.
-- If the new imitative distance had been three quarters, then
-- the first measure of the new voice would have been "a b a b".
-- And etc. if the new imitative distance was longer than the
-- previous four quarters.

-- So the trick is to imagine the algorithm to fix up that
-- overlay.  Note first of all the previous canon, if created
-- in full, ends with the bit where the voices drop out, one
-- by one.  So my starting point wasn't quite correct, and
-- should show a gap at the end of the first voice.    

-- |       |       |       |       |
-- 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4
-- a b c d e f g h a b c d e f g h
--         a b c d e f g h a b c d e f g h

-- One way to stitch things together would be as follows:

-- |       |       |       |       |       |       |
-- 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1
-- a b c d e f g h a b c d e f g h a b c d e f g h
--         a b c d e f g h a b c d e a b c d e f g h

-- In this case, I segue the lead voice directly into
-- the new canon and rewrite the end of the other
-- voices.  In the example above, I replace "f g h"
-- with "a b c" and keep going from there.  The issue
-- with that is undoing the endings of all those other
-- voices, which would be awkward.  It seems easier to
-- first fill in the voices that drop out, brining a
-- full texture to the point where the new imitative    
-- distance starts, using the old parameters.

-- Note another strategy would be to leave those voices
-- empty with rests, e.g.    
    
-- |       |       |       |       |       |       |       |
-- 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1 2 3 4 1
-- a b c d e f g h a b c d e f g h - - - - a b c d e f g h
--         a b c d e f g h a b c d e f g h - a b c d e f g h

-- It seems like this sort of fill-in-the-gap is going to be
-- easier to implement and leave me with the option to put in
-- rests or continue the old texture.

-- Using the current canon and the imitative distance for the
-- new canon, the trick will be to compute the additional notes
-- needed to fill in the gap.  For the first voice, that's going
-- to be the imitative distance times the number of voices less
-- one, plus the imitative distance in the new canon.

-- So if you have four voices imitating at a distance of four
-- quarters, that's a total of 12 quarters for the first voice,
-- 8 quarters for the second voice, and 4 for the third voice.
-- Whops, plus you need to fill the wait time at the start of
-- the new canon.  If the new canon starts after one quarter,
-- that's 12 quarters plus nothing for the first voice, 8 quarters
-- plus 1 for the second, 4 plus 2 for the third, and 3 quarters
-- for the fourth voice.

-- The formula voice-by-voice for voice v[j] [0..n) for count 
-- of voices n with original imitative distance i1 and new
-- imitative distance i2 would be:
--   ((n - (j + 1)) * i0) + (j * i1)
-- So, for i1 = 4, n = 4, i2 = 1, voice indexes j [0..3]
-- the fillers: would be
-- ((n - (j + 1)) * i1) + (j * i2)
-- ((4 - (0 + 1)) * 4)  + (0 * 1) = 12 + 0 = 12
-- ((4 - (1 + 1)) * 4)  + (1 * 1) = 8  + 1 =  9
-- ((4 - (2 + 1)) * 4)  + (2 * 1) = 4  + 2 =  6
-- ((4 - (3 + 1)) * 4)  + (3 * 1) = 0  + 3 =  3

-- So the first voice needs to continue the canon for
-- a total of 12 quarters, the second for 9, the third
-- for 6, and the fourth for 3.  Then it's safe to
-- just append the new canon onto the end of each voice
-- and keep on going.

-- The trick is going to be to build a routine to take
-- a given rhythmic duration from a statement of the
-- tune for the previous canon, noting that the duration
-- for the final note may be truncated.  Or actually,
-- it's a question of taking from the old voice note by
-- note, decrementing the total rhythmic allocation and
-- maybe truncating the duration of the last note when
-- the duration remaining is shorter than the duration
-- of the last note.  So the process of stitching
-- canons together comprises first emitting the canon
-- and then emitting the continuation for the overlap
-- to the new canon.

-- And what if the imitative distance is the same?
-- What should be able to happen at that point is you
-- skip all this filler stuff and tack the new canon
-- directly onto the old one.  But if you follow the
-- algorithm and imagine i2 above is 4, just like i1,
-- then you get output durations 12, 12, 12, and 12,
-- effectively dragging all the voices through one 
-- full iteration at the imitative distance, but with
-- no effect on the texture beyond the extension in
-- duration for that canon.  So it seems like a wash
-- and you might as well special-case it and just 
-- append with no filler, unless it makes a surprise
-- in terms of the overal rhythm, canon-to-canon.
    
-- What if i2 is longer longer than i1?  Say in the
-- example above i1 was 1 and i2 was 4?  Then the
-- filler distances would be exactly reversed,
-- which is what you'd want:
-- ((n - (j + 1)) * i1) + (j * i2)
-- ((4 - (0 + 1)) * 1)  + (0 * 4) = 3 + 0  =  3
-- ((4 - (1 + 1)) * 1)  + (1 * 4) = 2 + 4  =  6
-- ((4 - (2 + 1)) * 1)  + (2 * 4) = 1 + 8  =  9
-- ((4 - (3 + 1)) * 1)  + (3 * 4) = 0 + 12 = 12

-- | Given a new rhythm and a note, replace the rhythm
--   in the note with the new rhythm
replaceNoteRhythm :: Rhythm -> Note -> Note
replaceNoteRhythm rhythm (Note pitch _)                    = Note pitch rhythm
replaceNoteRhythm rhythm (AccentedNote pitch _ accent)     = AccentedNote pitch rhythm accent
replaceNoteRhythm rhythm (Rest _)                          = Rest rhythm
replaceNoteRhythm rhythm (PercussionNote _)                = PercussionNote rhythm
replaceNoteRhythm rhythm (AccentedPercussionNote _ accent) = AccentedPercussionNote rhythm accent

-- | Given a duration as a rhythm and a list of notes,
--   answer a new list of notes spanning exactly equal
--   to or shorter than the duration.  Fold across all
--   the notes decrementing the duration by the rhythm
--   for each note and accumulating a new list of notes
--   the cumulative duration for which is no larger than
--   the input duration.
takeNotesForDuration :: Rhythm -> [Note] -> [Note]
takeNotesForDuration dur ns =
  reverse $ snd $ foldl takeNote (dur, []) ns
  where
    takeNote (dur', ns') n
      | dur' == Rhythm (0%1) = (dur',           ns')
      | dur' >= noteDur      = (dur' - noteDur, n:ns')
      | otherwise            = (Rhythm (0%1),   n':ns')
      where
        noteDur = noteToRhythm n
        n' = replaceNoteRhythm dur' n

-- | Given old imitative distance i1, new imitative
--   distance i2, the count for a voice, the total
--   number of voices, and the notes for a canon,
--   compute the duration to wind down the voice in
--   preparation for the new canon and answer the
--   list of notes to append to the voice from the
--   old canon.
wrapCanon :: (Rhythm, Rhythm) -> Int -> Int -> [Note] -> [Note]
wrapCanon (Rhythm i1, Rhythm i2) vNum vTot =
  takeNotesForDuration (Rhythm dur) 
  where
    c1Wrap = i1 * toRational (vTot - (vNum + 1))
    c2Wrap = i2 * toRational vNum
    dur = c1Wrap + c2Wrap

-- Consume 1) list of list of imitative distances, per
-- voice/per canon, 2) list of list of notes per canon

-- Problem is this just appends voices, canon-by-canon
-- (here each in its own score).  Need to first wrap
-- voices from old canon given imitative distances for
-- old and new canons (voice-by-voice) and tune for old
-- canon.

-- This would work fine, assuming canon in each score
-- was already wrapped.

-- But maybe this is the logical place, given I'm already    
-- folding from the voices in the first canon over a list
-- of all the voices from all the other canons.

-- One trick would be to fetch the bits from the old     
-- canon, which is just tacked onto the end of the
-- voice already at some indeterminate count of notes
-- earlier on.

-- Also I need to pass along the pair of imitative distances,
-- old and new.  It'd be easy to just pass along the distss 
-- this method, then organize them by pairs, [(0,1),(1,2)..]
-- for zipping over.  Or preprocessing them to be that way and
-- passing them as another input argument.

-- concat $ zipWith (\x y -> x:[y]) [0..5] [10..15]

interject :: [a] -> [a] -> [a]
interject xs ys =
  foldr (\(x,y) xs' -> x:y:xs') [] pairs
  where
    pairs = zip xs ys
    
{--
  foldr ((++) . pr2list) [] pairs
  where
    pairs = zip xs ys
    pr2lst p = [fst p, snd p]

  concat $ map pr2list $ zip xs ys
--}
    
scoresToScore :: [Score] -> [Score] -> Score
scoresToScore scores glues =
  Score title voices
  where
    lastScore = scores !! (length scores - 1)
    gluedScores = interject scores glues
    allScores = gluedScores ++ [lastScore] 
    (Score title vs) = head allScores
    vs' = map addInstrumentControlToVoice vs
    vss = map scoreVoices $ tail allScores
    voices = foldl (zipWith appendVoice) vs' vss

-- Nope, this won't work.  There are heterogeneous lists here.
-- The intervalss and rhythmss group together, separately from
-- distss, scaless, octavess, instrumentss, and reptition counts.
-- What I really want is a commonCanonToScore that takes the tune
-- and then I can zip over the trailing five lists.  Furthermore,
-- note that assembling the tune means I can shove it into the
-- scoresToScore method.

-- But that's wrong, because the Canon type includes lists of
-- lists of intervals and rhythms, which should match the lengths
-- of the other list of lists, allowing for each canon to have a
-- unique tune, effectively making the canon really a string of
-- canons, a composite canon and not really an integral canon at
-- all!

-- Meanwhile, I'm stuck with the problem that I really need to
-- be able to stitch the scores together.  What do I need to do
-- to emit just the intermediate Scores, the ones that have the 
-- wrappers to tag onto the ends of each score?  If I can get
-- that, then I can go back to just concatenating all the Scores
-- together.

-- The range would be over one less than the total number of scores
-- as the last would just run down.  Input to each iteration would be
-- all the bits that go into making the canon plus the imitative
-- distance for the canon that follows.
    
-- (\xs -> zip xs (tail xs)) [1..10]

canonToGlueScore :: Title -> [Interval] -> [Rhythm] -> [(Rhythm, Rhythm)] -> [Scale] -> [Octave] -> [Instrument] -> Score
canonToGlueScore title intervals rhythms distancePairs scales octaves instruments =
  Score title voices
  where
    voiceCount = length scales
    genPitches scale octave = map (getPitch scale octave) intervals
    genTune scale octave = zipWith Note (genPitches scale octave) rhythms
    tuness = zipWith genTune scales octaves
    incr = getPan (maxBound::Pan) `div` length instruments
    pans = map (\i -> PanControl (Pan (incr * i)) (Rhythm (0%4))) [0,1..]
    gluess = getZipList $
             wrapCanon 
             <$> ZipList distancePairs
             <*> ZipList [0,1..]
             <*> ZipList [voiceCount,voiceCount..]
             <*> ZipList tuness
    makeVoice instrument tunes pan = Voice instrument tunes [[pan]]
    voices = zipWith3 makeVoice instruments gluess pans

-- Problem: this is really ugly to assemble, creating parallel lists of lists of 
-- distances, scales, octaves, and instruments.  What I have here is a compound
-- canon where the compounding takes place linearly with each new canon grafted    
-- onto the end of the previous canon.  Rather than jamming all bits together
-- a priori into e.g. the CompoundCanon record, it would be easier to program 
-- if there was a way to tack a canon onto another one, piece-by-piece.  Then
-- you could have any of the base canon types (although they're all really just
-- specializations of RhythmCanon, which should get renamed as just "Canon").
-- What's the resulting type?  There should be some sense of the maximum voice
-- count to allow for variability and for generating the rests for the silent
-- voices when necessary.  But really, I suppose the method that accumulates
-- canons could either just consume a "Canon" (aka RhythmCanon) and build up
-- a list of them, then the conversion to score would operate over that list.
-- And for convenience there could be a way to convert the simpler canons to
-- the base "Canon" type.
-- What about a change in the count of voices though?  Should be pretty easy.
-- When growing, the splice takes place overlapping as though the voice counts
-- were the same.  Then the new canon just continues to grow with the entry of
-- each new voice.  When shrinking, the splice works as it does now, and the
-- extra voices gather rests for the duration of the new canon (which, NB, may
-- have repetition count > 1).
-- The important thing is the API changes and the SequentialCompoundCanon
-- type is just a list of Canon with APIs for growing the list.  Then the 
-- conversion method to a score has to search the list for the maximum
-- voice count and the conversion to score has to manage "phantom" voices
-- each of which gets a single rest for duration of the canon.
-- Then after that I can consider SimultaneousCompoundCanon--and maybe
-- rename as Linear vs. Vertical?  There could be a trivial rendition
-- that just juxtaposes in parallel voices, canon by canon, with some sort
-- of way to indicate rests.  And there could be a non-trivial version that
-- somehow interprets the canon parameters cumulatively, for a canon within
-- a canon, for a fractal canon.

compoundCanonToScore :: CompoundCanon -> Score
compoundCanonToScore (CompoundCanon title intervalss rhythmss distss scaless octavess instrumentss) =
  scoresToScore scores' glues
  where
    scores = getZipList $
             commonCanonToScore title
             <$> ZipList intervalss
             <*> ZipList rhythmss
             <*> ZipList distss
             <*> ZipList scaless
             <*> ZipList octavess
             <*> ZipList instrumentss
             <*> ZipList [1,1..]
    -- Problem: each inner score has leading rests for
    -- new canon, need to be removed.  As simple as taking
    -- first note from each of scores !! 1 and following.
    -- no -- take first note from all voices other than
    -- the first
    scores' = head scores : map trimScoreRests (tail scores)
    trimScoreRests (Score _ voices) = Score title (head voices : map trimVoiceRests (tail voices))
    trimVoiceRests (Voice instr notes controlss) = Voice instr (tail notes) controlss
    -- Problem: distss is list of list of distances where each outer list is per canon,
    -- each inner list is per voice for that canon.  Result that I want is list of list
    -- of dist pairs, voice-by-voice, for each canon.  So to start by pairing list of
    -- dists is correct.  But then resulting list of pairs where each element is itself
    -- a list of dists has to be further processed
    -- distPairss = map (\(xs, ys) -> zip xs ys) $ zip distss (tail distss)
    distPairss = zipWith zip distss (tail distss)
    glues = getZipList $
             canonToGlueScore title
             <$> ZipList intervalss
             <*> ZipList rhythmss
             <*> ZipList distPairss
             <*> ZipList scaless
             <*> ZipList octavess
             <*> ZipList instrumentss
