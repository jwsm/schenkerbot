
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
  "Take the MIDI octave number and return the lilypond octave number."
  (- octave 3))

(defun midi-pitch-to-lilypond-octave (midi-pitch)
  "Take the midi pitch and return the lilypond octave number."
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

(defun event-groups-to-lily-pond (event-groups left-hand)
  "PRINT a set of event groups in lily pond format; calls event-group-to-lily-pond for each group."
  (if (null event-groups) ()
    (progn
      (event-group-to-lily-pond (first event-groups) left-hand)
      (event-groups-to-lily-pond (rest event-groups) left-hand))))


(defun event-group-to-lily-pond (event-group left-hand)
"PRINT an event group in lily-pond format, either for left hand or right hand"
  (cond
        ;; print the left hand (and chord symbols if they exist)
        (left-hand
          ;(cond ((equal (schenker-level event-group) 2)
          ;        (format t "\\override Stem #'transparent = ##f~%"))
          ;      (T (format t "\\override Stem #'transparent = ##t~%")))

          (print-lily-pond-chord (left-hand-events event-group))
          (let ((best-function-choice (first (possible-functions event-group)))
                (inversion-symbol (inversion-index-to-name (inversion event-group))))
            (if (null best-function-choice) T
              (format t "[_\\markup { \\bold ~a ~a }]~%"
                          (second best-function-choice)
                          inversion-symbol)))
          (format t "s ~%~%")
          (format t "s ~%~%")
          )

        ;; print the right hand
        (T
          (print-lily-pond-chord (right-hand-events event-group)))))


(defun print-lily-pond-chord (chord-notes)
  "PRINT: print a single chord to OUTFILE"
  (if (null chord-notes) T
    (progn
      (format t "<")
      (print-each-lily-pond-note (cope-events-to-lily-pond chord-notes))
      (format t " > ~%")
      ;(format t "[_\\markup { \\bold I }]~%")
      )))

(defun print-each-lily-pond-note (notes)
  "PRINT: print a single note to OUTFILE"
  (if (null notes) T
    (progn
      ;;(format t " ~a " (midi-pitch-to-lily-pond (first notes)))
      (format t " ~a " (first notes))
      (format t " ")
      (print-each-lily-pond-note (rest notes)))))


(defun lily-pond-start-rh-section ()
  (format t "{")
  (format t "\\override Staff.NoteCollision ~% #'merge-differently-headed = ##t")
  (format t "\\clef treble")
  (format t "\\override Stem #'transparent = ##t")
)

(defun lily-pond-start-lh-section ()
  (format t "{")
  (format t "\\override Staff.NoteCollision ~% #'merge-differently-headed = ##t")
  (format t "\\clef bass")
  (format t "\\override Stem #'transparent = ##t")
)

(defun lily-pond-end-section ()
  (format t "}")
)

(defun lily-pond-file-from-event-groups (event-groups left-hand)
  "Write out event-group-output.ly file based on calling event-groups-to-lily-pond."
  (with-open-file (outfile "/Users/jwsm/Sites/wacm/tmp/event-group-output.ly" :direction :output :if-exists :supersede)
    (let ((*standard-output* outfile))

      ;(lily-pond-start-rh-section)
      ;(event-groups-to-lily-pond event-groups nil)
      ;(lily-pond-end-section)

      (lily-pond-start-lh-section)
      (event-groups-to-lily-pond event-groups T)
      (lily-pond-end-section))))

