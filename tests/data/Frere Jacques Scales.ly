\version "2.18.2"
\header {title = "Frere Jacques Scales" composer = "folk"}
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
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef treble c'4\p\<_\markup{pan 0}-.\soft d'4-. e'4\!\f--\hard c'4\p-^ c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ c'4\p\<-.\soft d'4-. e'4\!\f--\hard c'4\p-^ e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard e'4\mp\<-.\soft f'4-. g'2\!\ff-_\hard g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft g'8\f\>-!\hard a'8-!\hard g'8-! f'8-! e'4-^\soft c'4\!\p-^\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft c'4\mf\>-_\soft g4-_\soft c'2\!\p--\verysoft r1*6 \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Marimba" \global \clef treble r1*2_\markup{pan 31} aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ aes4\p\<-.\soft bes4-. c'4\!\f--\hard aes4\p-^ c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard c'4\mp\<-.\soft des'4-. ees'2\!\ff-_\hard ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft ees'8\f\>-!\hard f'8-!\hard ees'8-! des'8-! c'4-^\soft aes4\!\p-^\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft aes4\mf\>-_\soft ees4-_\soft aes2\!\p--\verysoft r1*4 \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Vibraphone" \global \clef treble r1*4_\markup{pan 62} e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ e''4\p\<-.\soft fis''4-. g''4\!\f--\hard e''4\p-^ g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard g''4\mp\<-.\soft a''4-. b''2\!\ff-_\hard b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft b''8\f\>-!\hard c'''8-!\hard b''8-! a''8-! g''4-^\soft e''4\!\p-^\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft e''4\mf\>-_\soft b'4-_\soft e''2\!\p--\verysoft r1*2 \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 80s1\override TextSpanner.bound-details.left.text = "accel."s1*7\startTextSpan\tempo 4 = 120s1\stopTextSpan\override TextSpanner.bound-details.left.text = "rit."s1*7\startTextSpan\tempo 4 = 80s1\stopTextSpan}
{\set Staff.instrumentName = #"Acoustic Grand Piano" \global \clef bass r1*6_\markup{pan 93} d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ d,4\p\<-.\soft e,4-. f,4\!\f--\hard d,4\p-^ f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard f,4\mp\<-.\soft g,4-. a,2\!\ff-_\hard a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft a,8\f\>-!\hard bes,8-!\hard a,8-! g,8-! f,4-^\soft d,4\!\p-^\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft d,4\mf\>-_\soft a,,4-_\soft d,2\!\p--\verysoft \bar "|."}
>>
>>
\layout { }
\midi { }
}
