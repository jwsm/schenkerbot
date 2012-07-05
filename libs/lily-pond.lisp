
#|
midi-pitch-to-pitch-class
|#
(defun midi-pitch-to-pitch-class (midi-note)
  "return the pitch class for a given midi note number"
  (mod midi-note 12))

#|
pitch-class-to-lily-pond
|#
(defun pitch-class-to-lily-pond (pitch-class)
  "return the lily pond name for a given pitch class"
  (nth pitch-class '("c" "cis" "d" "dis" "e" "f" "fis" "g" "gis" "a" "ais" "b")))

#|
pitch-class-to-note-name
|#
(defun pitch-class-to-note-name (pitch-class)
  "return the note name for a given pitch class"
  (nth pitch-class '("c" "c#" "d" "d#" "e" "f" "f#" "g" "g#" "a" "a#" "b")))

#|
midi-note-to-octave
|#
(defun midi-pitch-to-octave (midi-pitch)
  "Return the octave number of the note given a midi pitch."
  (1- (floor (/ midi-pitch 12))))

#|
midi-octave-to-lilypond-octave
|#
(defun midi-octave-to-lilypond-octave (octave)
  "Take the MIDI octave number and return the lilypond octave number (centered around middle c)."
  (- octave 3))


(defun midi-pitch-to-lilypond-octave (midi-pitch)
  (midi-octave-to-lilypond-octave (midi-pitch-to-octave midi-pitch)))

#|
repeat-string

use to generate repeated tick marks or commas
|#
(defun repeat-string (count string)
  "A non-recursive function that concatenates a set of tick marks, with the number specified in count."
      (with-output-to-string (s)
         (dotimes (n count)
           (format s "~a" string ))))


#|
midi-note-to-lily-pond

convert note from MIDI pitch number to lowercase letter plus appropriate tick marks or commas to
designate octave.
|#
(defun midi-pitch-to-lily-pond (midi-pitch)
  "Given a midi pitch value to return an absolute lilypond pitch string."
  (let* ((lp-octave (midi-pitch-to-lilypond-octave midi-pitch))
        (pitch-class (midi-pitch-to-pitch-class midi-pitch))
        (lp-pitch-class (pitch-class-to-lily-pond pitch-class)))
    (cond ((zerop lp-octave) lp-pitch-class)
          ((> lp-octave 0) (concatenate 'string lp-pitch-class (repeat-string lp-octave "'")))
          (T (concatenate 'string lp-pitch-class (repeat-string (abs lp-octave) ","))))))



(defun cope-events-to-lily-pond (events)
  "Convert a list of cope events to lily pond pitch symbols."
  (if (null events) ()
    (cons (midi-pitch-to-lily-pond
            (second (first events)))
          (cope-events-to-lily-pond (rest events)))))


(defun beat-group-to-pitches (beat-group-note-events)
  "Extract pitches from a list of beat-group events."
  (if (null beat-group-note-events) ()
    (cons (second (first beat-group-note-events))
          (beat-group-to-pitches (rest beat-group-note-events)))))


(defun beat-tree-to-pitches (tree)
  "Create a list of lists of pitches out of a tree of beat-group objects"
  (if (null tree) ()
    (cons (beat-group-to-pitches (note-events (first tree)))
          (beat-tree-to-pitches (rest tree)))))


;(print-lily-pond-section-with-chords (beat-tree-to-pitches *tree*))2

#|
print-each-note

|#
(defun print-lily-pond-chords (chords)
  (cond ((null chords) T)
        ((null (first chords)) (print-lily-pond-chords (rest chords)))
        (T (progn
             (print-lily-pond-chord (first chords))
             (print-lily-pond-chords (rest chords))))))

(defun print-lily-pond-chord (chord-notes)
  (if (null chord-notes) T
    (progn
      (format t "<")
      (print-each-lily-pond-note chord-notes)
      (format t " > ~%"))))



(defun print-each-lily-pond-note (notes)
  (if (null notes) T
    (progn
      (format t " ~a " (midi-pitch-to-lily-pond (first notes)))
      (format t " ")
      (print-each-lily-pond-note (rest notes)))))

(defun print-lily-pond-section (notes)
  (format t "{")
  (print-each-lily-pond-note notes)
  (format t "}"))

(defun print-lily-pond-section-with-chords (chords)
  (format t "{")
  (print-lily-pond-chords chords)
  (format t "}"))



(defun split-pitches-within-chord (pitches)
  "Takes pitches in a list and returns two sublists, containing pitches > and <= note 60 (two hands)."
  (if (null pitches) '(() ())
    (let ((recursive-call (split-pitches-within-chord (rest pitches))))
      (cond ((> 60 (first pitches))
             (list (cons (first pitches) (first recursive-call)) (second recursive-call)))
            (T
             (list (first recursive-call) (cons (first pitches) (second recursive-call))))))))

#|
  (cond ((null pitches) '(() ()))

        ((> 60 (first pitches))
         (cons (first pitches) (first (split-pitches-within-chord (rest pitches)))))
        (T
         (cons (first pitches) (second (split-pitches-within-chord (rest pitches)))))))
|#



; split-chord-pitch-list-into-two-hands

;; THIS IS THE RIGHT FUNCTION TO CALL RIGHT HERE:::: --\
;(split-chord-pitch-list-into-two-hands (beat-tree-to-pitches *tree*))

(defun split-chord-pitch-list-into-two-hands (chord-pitch-list)
  "Split a list of lists of pitches (each sublist a chord) into two sublists (for right and left hand)."
  (if (null chord-pitch-list) '(() ())
    (let ((split-chord (split-pitches-within-chord (first chord-pitch-list)))
          (recursive-call (split-chord-pitch-list-into-two-hands (rest chord-pitch-list))))
      (list (cons (first split-chord) (first recursive-call))
            (cons (second split-chord) (second recursive-call))))))





;(defun split-cope-events-by-pitch (cope-events)


;(print-lily-pond-section (cope-events-to-lily-pond *events*))



(defun write-test-file ()
  (with-open-file (outfile "/Users/jwsm/Sites/wacm/tmp/my-test-output.ly" :direction :output :if-exists :supersede)
    (let ((*standard-output* outfile))
      
      (print-lily-pond-section (cope-events-to-lily-pond *events*))
      ;(wrap-lilypond-section (cope-events-to-lilypond *events*) outfile)


)))

(write-test-file)