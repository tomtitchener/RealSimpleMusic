\version "2.18.2"
\header {title = "Frere Jacques Simple" composer = "folk"}
global = {\key f \major  \time 4/4   }
softest = ^\markup {\musicglyph #"scripts.dmarcato"}
verysoft = ^\markup {\musicglyph #"scripts.upedaltoe"}
soft = ^\markup {<}
hard = ^\markup {>}
veryhard = ^\markup {\musicglyph #"scripts.dpedaltoe"}
hardest = ^\markup {\musicglyph #"scripts.umarcato"}
\score {
\new StaffGroup << 
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble c'4_\markup{pan 0}-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p r1*6 \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble r1*2_\markup{pan 31} c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p r1*4 \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble r1*4_\markup{pan 62} c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p r1*2 \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble r1*6_\markup{pan 93} c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p c'4-.\soft\p\< d'4-. e'4--\hard\!\f c'4-^\p e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff e'4-.\soft\mp\< f'4-. g'2-_\hard\!\ff g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p g'8-!\hard\f\> a'8-!\hard g'8-! f'8-! e'4-^\soft c'4-^\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p c'4-_\soft\mf\> g4-_\soft c'2--\verysoft\!\p \bar "|."}
>>
>>
\layout { }
\midi { }
}
