(defun note-is-scale-degree-n (cope-event n)
  "Check to see if note is of scale degree n"
  (equal (midi-pitch-to-pitch-class (second cope-event)) n))

(defun note-is-harmonized-by-root-n (cope-event root)
  "Returns 1 if note is harmonized by root n"
  )

(defun note-is-near-beginning (cope-event)
  "Fuzzy function, returns 1 if note is near beginning.")

(defun note-is-near-end (cope-event)
  "Fuzzy function, returns 1 if note is near the end.")

(defun note-is-near-top-of-range (cope-event)
  "Fuzzy function, returns 1 if note is near top of range.")

(defun note-is-top-voice (cope-event)
  "Fuzzy function, returns 1 if note is in (or near) top voice.")

(defun note-is-harmonized (cope-event)
  "Fuzzy function, returns 1 if note is properly harmonized within the key.")




; Function to set Schenker Level

