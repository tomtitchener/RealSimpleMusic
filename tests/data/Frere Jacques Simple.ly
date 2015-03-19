\version "2.18.2"
\header {title = "Frere Jacques Simple" composer = "folk"}
global = {\key f \major  \time 4/4  \tempo 4 = 60 }
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
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble c'4_\markup{pan 0}-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- r1*6 \bar "|."}
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble r1*2_\markup{pan 31} c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- r1*4 \bar "|."}
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble r1*4_\markup{pan 62} c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- r1*2 \bar "|."}
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble r1*6_\markup{pan 93} c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- \bar "|."}
>>
\layout { }
\midi { }
}
