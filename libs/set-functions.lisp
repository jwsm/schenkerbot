

; requires fuzzy.lisp, fuzzy-music.lisp

(defun pitch-set-for-midi-pitch (midi-pitch)
  "return the pitch set i.e., (1 0 0 0 0 0 0 0 0 0 0 0) for a given midi pitch."
  (ror-n *FZ-NOTE* (midi-pitch-to-pitch-class midi-pitch)))
  
(defun pitch-set-for-cope-event (cope-event)
	"return the pitch set for a given cope event."
	(pitch-set-for-midi-pitch (second cope-event)))