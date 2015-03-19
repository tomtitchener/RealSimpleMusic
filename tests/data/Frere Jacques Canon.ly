\version "2.18.2"
\header {title = "Frere Jacques Canon" composer = "folk"}
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
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble c'4_\markup{pan 0}-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- c'4-. d'4-. e'4-- c'4-^ c'4-. d'4-. e'4-- c'4-^ e'4-. f'4-. g'2-_ e'4-. f'4-. g'2-_ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ g'8-! a'8-! g'8-! f'8-! e'4-^ c'4-^ c'4-_ g4-_ c'2-- c'4-_ g4-_ c'2-- r1 r1 r4. \bar "|."}
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
{\set Staff.instrumentName = #"Marimba" \global \clef treble r1*2_\markup{pan 31} aes4-. bes4-. c'4-- aes4-^ aes4-. bes4-. c'4-- aes4-^ c'4-. des'4-. ees'2-_ c'4-. des'4-. ees'2-_ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ aes4-_ ees4-_ aes2-- aes4-_ ees4-_ aes2-- aes4-. bes4-. c'4-- aes4-^ aes4-. bes4-. c'4-- aes4-^ c'4-. des'4-. ees'2-_ c'4-. des'4-. ees'2-_ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ aes4-_ ees4-_ aes2-- aes4-_ ees4-_ aes2-- aes4-. bes4-. c'4-- aes4-^ aes4-. bes4-. c'4-- aes4-^ c'4-. des'4-. ees'2-_ c'4-. des'4-. ees'2-_ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ aes4-_ ees4-_ aes2-- aes4-_ ees4-_ aes2-- aes4-. bes4-. c'4-- aes4-^ aes4-. bes4-. c'4-- aes4-^ c'4-. des'4-. ees'2-_ c'4-. des'4-. ees'2-_ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ aes4-_ ees4-_ aes2-- aes4-_ ees4-_ aes2-- aes4-. bes4-. c'4-- aes4-^ aes4-. bes4-. c'4-- aes4-^ c'4-. des'4-. ees'2-_ c'4-. des'4-. ees'2-_ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ ees'8-! f'8-! ees'8-! des'8-! c'4-^ aes4-^ aes4-_ ees4-_ aes2-- aes4-_ ees4-_ aes2-- r4. \bar "|."}
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
{\set Staff.instrumentName = #"Vibraphone" \global \clef treble r1_\markup{pan 62} r1 r4 e''4-. fis''4-. g''4-- e''4-^ e''4-. fis''4-. g''4-- e''4-^ g''4-. a''4-. b''2-_ g''4-. a''4-. b''2-_ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ e''4-_ b'4-_ e''2-- e''4-_ b'4-_ e''2-- e''4-. fis''4-. g''4-- e''4-^ e''4-. fis''4-. g''4-- e''4-^ g''4-. a''4-. b''2-_ g''4-. a''4-. b''2-_ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ e''4-_ b'4-_ e''2-- e''4-_ b'4-_ e''2-- e''4-. fis''4-. g''4-- e''4-^ e''4-. fis''4-. g''4-- e''4-^ g''4-. a''4-. b''2-_ g''4-. a''4-. b''2-_ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ e''4-_ b'4-_ e''2-- e''4-_ b'4-_ e''2-- e''4-. fis''4-. g''4-- e''4-^ e''4-. fis''4-. g''4-- e''4-^ g''4-. a''4-. b''2-_ g''4-. a''4-. b''2-_ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ e''4-_ b'4-_ e''2-- e''4-_ b'4-_ e''2-- e''4-. fis''4-. g''4-- e''4-^ e''4-. fis''4-. g''4-- e''4-^ g''4-. a''4-. b''2-_ g''4-. a''4-. b''2-_ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ b''8-! c'''8-! b''8-! a''8-! g''4-^ e''4-^ e''4-_ b'4-_ e''2-- e''4-_ b'4-_ e''2-- r8 \bar "|."}
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef bass r1_\markup{pan 93} r1 r4. d,4-. e,4-. f,4-- d,4-^ d,4-. e,4-. f,4-- d,4-^ f,4-. g,4-. a,2-_ f,4-. g,4-. a,2-_ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ d,4-_ a,,4-_ d,2-- d,4-_ a,,4-_ d,2-- d,4-. e,4-. f,4-- d,4-^ d,4-. e,4-. f,4-- d,4-^ f,4-. g,4-. a,2-_ f,4-. g,4-. a,2-_ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ d,4-_ a,,4-_ d,2-- d,4-_ a,,4-_ d,2-- d,4-. e,4-. f,4-- d,4-^ d,4-. e,4-. f,4-- d,4-^ f,4-. g,4-. a,2-_ f,4-. g,4-. a,2-_ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ d,4-_ a,,4-_ d,2-- d,4-_ a,,4-_ d,2-- d,4-. e,4-. f,4-- d,4-^ d,4-. e,4-. f,4-- d,4-^ f,4-. g,4-. a,2-_ f,4-. g,4-. a,2-_ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ d,4-_ a,,4-_ d,2-- d,4-_ a,,4-_ d,2-- d,4-. e,4-. f,4-- d,4-^ d,4-. e,4-. f,4-- d,4-^ f,4-. g,4-. a,2-_ f,4-. g,4-. a,2-_ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ a,8-! bes,8-! a,8-! g,8-! f,4-^ d,4-^ d,4-_ a,,4-_ d,2-- d,4-_ a,,4-_ d,2-- \bar "|."}
>>
\layout { }
\midi { }
}
