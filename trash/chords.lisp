(defvar *chord-templates*)
(defvar *chord-functions*)

;; Define chord templates as pitch class sets
(setq *chord-templates* '(

('major (1 0 0 0 1 0 0 1 0 0 0 0) )
('minor (1 0 0 1 0 0 0 1 0 0 0 0) )
('diminished (1 0 0 1 0 0 1 0 0 0 0 0) )
('augmented (1 0 0 0 1 0 0 0 1 0 0 0) )
('dominant-seventh (1 0 0 0 1 0 0 1 0 0 1 0) )

))

(setq *chord-functions* '(I NA ii NA iii IV NA V NA vi NA vii))


#|
(defun all-chord-rotations (current-chord &optional (current-rotation 0))
  (if (equal current-rotation *NUM-PITCH-CLASSES*) ()
    (cons (ror-n current-chord current-rotation)
          (all-chord-rotations current-chord (1+ current-rotation)))))
|#


#|
compare-pitch-set-to-chord-template-rotations

Determine the most likely chord & quality. Rotate the provided chord templates and
match them against the sounding pitches to determine which chord template and which
transposition fits best.

called by compare-all-chords-to-pitch-set

improvements:
-take into account multiple notes of the same pitch?
|#
(defun compare-pitch-set-to-chord-template-rotations (pitch-set chord-template &optional (current-rotation 0))
  (if (equal current-rotation *NUM-PITCH-CLASSES*) ()
    (let ((chord-probability (/ (apply #'+ (mapcar #'* pitch-set (ror-n (second chord-template) current-rotation)))
                                       (apply #'+ (second chord-template))))
          (recursive-call (compare-pitch-set-to-chord-template-rotations pitch-set chord-template (1+ current-rotation))))
      (if (> chord-probability 0)
        (cons (list (first chord-template)
                    current-rotation
                    chord-probability)
              recursive-call)
        recursive-call))))

#|
compare-all-chords-to-pitch-set

Call compare-pitch-set-to-chord-template-rotations on each chord in the passed chord templates list.
Return a list of chord possibilities: (quality transposition likelihood)
|#
(defun compare-all-chords-to-pitch-set (pitch-set all-chords)
  (if (null all-chords) ()
    (append (compare-pitch-set-to-chord-template-rotations pitch-set (first all-chords))
            (compare-all-chords-to-pitch-set pitch-set (rest all-chords)))))


(defun lookup-function-given-chord-and-key (chord key)
  (nth (mod (- chord key) *NUM-PITCH-CLASSES*) *chord-functions*))