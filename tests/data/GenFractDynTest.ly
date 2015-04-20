\version "2.18.2"
\header {title = "GenFractDynTest" composer = "Test"}
global = {\key c \major  \time 4/4   }
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
{\tempo 4 = 120}
{\set Staff.instrumentName = #"Trombone" \global \clef treble c'1*4\mp <<c'1*4{s1*2\< s1*2\ff\> }>> <<c'1*4{s1\ff s1\p s1\mf s1\mp }>> \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 120}
{\set Staff.instrumentName = #"Trombone" \global \clef treble a'1*4\mp <<a'1*4{s1\ff s1\p s1\mf s1\mp }>> d1*4\mf \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 120}
{\set Staff.instrumentName = #"Trombone" \global \clef bass d1*4\mp <<d1*4{s1\ff s1\p s1\mf s1\mp }>> <<g,1*4{s1\ff s1\p s1\mf s1\mp }>> \bar "|."}
>>
\new Voice \with 
{\remove "Note_heads_engraver" \consists "Completion_heads_engraver" \remove "Rest_engraver" \consists "Completion_rest_engraver"}
<<
{\tempo 4 = 120}
{\set Staff.instrumentName = #"Trombone" \global \clef bass g,1*4\mf <<g,1*4{s1*2\< s1*2\ff\> }>> a'1*4\mf \bar "|."}
>>
>>
\layout { }
\midi { }
}
