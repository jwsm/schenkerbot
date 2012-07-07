% Schenkerian Graph Template | 1 Grand Staff
% created by Kris Shaffer


\version "2.6.0"
#(set-default-paper-size "letter")

\header {
	tagline = ""
}


staffPiano = \new PianoStaff {
			\set Score.timing = ##f
			\set PianoStaff.followVoice = ##t

	<<
		\context Staff = "RH" {  % Right hand 
			\clef treble
% 			\key c \major
			{
				\override Staff.NoteCollision
				#'merge-differently-headed = ##t
	<<

% use first two voices for urlinie (fundamental structure, upper staff)
		{
			\override Beam  #'positions = #'(8 . 8)
			\override NoteHead #'transparent = ##t

			% insert eighth notes here

			\revert Beam #'positions
			\revert NoteHead #'transparent
			} 
	\\
		{
			\override Stem #'transparent = ##t

			% insert half notes here
			
		  	\revert Stem #'transparent
			}
	\\

% use other voices for non-urlinie notes and analysis marks
\override Stem #'transparent = ##t
< a'   e''   > 

		{

			}
	\\
		{

			}
	\\
		{

			}
	\\
		{

			}
	>>
	\bar "|."
			}
		}
		\context Staff = "LH" {  % Left hand 			
			\clef bass
% 			\key c \major
			 {
				\override Staff.NoteCollision
				#'merge-differently-headed = ##t
	<<

% use first two voices for bassbrechung (fundamental structure, lower staff)
		{
			\override Beam  #'positions = #'(-8 . -8)
			\override NoteHead #'transparent = ##t

			% insert eighth notes here
			
			\revert Beam #'positions
			\revert NoteHead #'transparent
			}
	\\
		{
			\override Stem #'transparent = ##t

			% insert half notes here
			
			\revert Stem #'transparent
			}
	\\
	
% use other voices for non-bassbrechung notes and analysis marks
\override Stem #'transparent = ##t
< a   c' > 

		{
		
			}
	\\
		{

			}
	\\
		{

			}
	\\
		{

			}
	>>
	\bar "|."
			}
		}
	>>
}



\score {
	<<
		\staffPiano
	>>
	
%	\midi {
%	}

	\layout  {
		raggedright = ##t
	}
}

\paper {
}

