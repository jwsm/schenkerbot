; ----------------------------------------------------------
; Fuzzy Chord Root Detector
; ----------------------------------------------------------
; requres PQE's fz_catch_chord.lisp


(defvar *chord-quality-symbols* ())
(setq *chord-quality-symbols*
  '(:dom7 :m7b5 :dim7 :maj7 :min7 :aug :maj :min :min2 :min3 :maj3 :p4 :tt :p5 :min6 :maj6 :min7 :maj7 :pitch))


(defun calculate-roots-helper (pitches-by-chord-wt)
  "Return lists of (onset (root chord-type)) for each group of pitches."
  (if (null pitches-by-chord-wt) ()
    (cons (list (first (first pitches-by-chord-wt)) ; onset time
                (chord-root-and-type (second (first pitches-by-chord-wt)))) ; (root chord-type)
          (calculate-roots-helper (rest pitches-by-chord-wt)))))

(defun calculate-roots (events)
  "Return lists of (onset-time (root chord-type)) for each set of simultaenous cope-events."
  "Calls calculate-roots-helper to recurse through list."
  (let* ((events-by-chord (group-cope-events-by-simultaneous-start events))
         (pitches-by-chord-with-times (cope-event-groups-to-pitch-groups-with-times events-by-chord)))
    (calculate-roots-helper pitches-by-chord-with-times)))

; (calculate-roots *input-events*)

(defun chord-roots-to-cope-events (chord-roots)
  "Convert a list of chord (onset-time (root chord-type)) lists to cope-events."
  (if (null chord-roots) ()
    (let* ((root-data (first chord-roots))
           (duration
            (if (null (first (rest chord-roots))) *ms-per-beat*
              (- (first (first (rest chord-roots))) (first root-data)))))
    (cons
     (list (first root-data)
           (+ 36 (first (second root-data)))
           duration
           1
           64)
     (chord-roots-to-cope-events (rest chord-roots))))))

; (setq *events* (chord-roots-to-cope-events (calculate-roots *input-events*)))
; (saveit)


(defun chord-roots-to-cope-event-groups (roots-list)
  (cope-events-to-event-groups (chord-roots-to-cope-events roots-list)))

(defun filter-chord-roots-to-beat-level (chord-roots)
  (filter-chord-roots-to-x-level chord-roots *ms-per-beat*))

(defun filter-chord-roots-to-bar-level (chord-roots)
  (filter-chord-roots-to-x-level chord-roots *bar-length*))

(defun filter-chord-roots-to-2-beat-level (chord-roots)
  (filter-chord-roots-to-x-level chord-roots (* *ms-per-beat* 2)))

(defun filter-chord-roots-to-x-level (chord-roots window-length)
  "Only allow roots to pass through if they start at (*first-bar* + (window-length * n)) ms"
  (if (null chord-roots) ()
    (if (zerop (mod (- (first (first chord-roots)) *first-bar*)
                    window-length))
      (cons (first chord-roots)
            (filter-chord-roots-to-x-level (rest chord-roots) window-length))
      (filter-chord-roots-to-x-level (rest chord-roots) window-length))))

; (setq *events* (chord-roots-to-cope-events (filter-chord-roots-to-beat-level (calculate-roots *input-events*))))
; (setq *events* (chord-roots-to-cope-events (filter-chord-roots-to-2-beat-level (calculate-roots *input-events*))))
; (setq *events* (chord-roots-to-cope-events (filter-chord-roots-to-bar-level (calculate-roots *input-events*))))
; (saveit)

; (setq *chord-root-groups* (chord-roots-to-cope-event-groups (calculate-roots *input-events*)))





;; Find the inversion of the chord

(defun find-lowest-pitch-class-in-cope-events (cope-events)
  (midi-pitch-to-pitch-class (find-lowest-pitch-in-cope-events cope-events)))

(defun find-lowest-pitch-in-cope-events (cope-events)
  (apply #'min (mapcar #'second cope-events)))

(defun find-highest-pitch-in-cope-events (cope-events)
  (apply #'max (mapcar #'second cope-events)))

(defun test-intervals-above-lowest-pitch (lowest-pitch root)
  (let ((lowest-note (ror-n *FZ-note* lowest-pitch))
        (fuzzy-3rd (ror-n *FZ-3* root))
        (fuzzy-5th (ror-n *FZ-5* root))
        (fuzzy-7th (ror-n *FZ-7* root)))
    (+ 1 (ltop
          (list (apply #'max (fz-intersect lowest-note fuzzy-3rd))
                (apply #'max (fz-intersect lowest-note fuzzy-5th))
                (apply #'max (fz-intersect lowest-note fuzzy-7th)))))))

; if distance from root to lowest note is a fuzzy third, first inversion
; if distance from root to lowest note is a fuzzy fifth, 2nd inversion
; if distance from root to lowest note is a fuzzy seventh, 3rd inversion

; 0 = root position
; 1 = 1st inversion
; 2 = 2nd inversion
; 3 = 3rd inversion
(defun find-inversion-using-cope-events-and-root (cope-events root-pc)
  (if (or (null cope-events) (null root-pc)) ()
  (let ((lowest-pitch (find-lowest-pitch-class-in-cope-events cope-events)))
    (if (equal lowest-pitch root-pc) 0
      (test-intervals-above-lowest-pitch lowest-pitch root-pc)))))



(defvar *chord-functions*)
(defvar *inversion-names*)

; Given a scale degree within the key and a quality, return the possible chord functions
(defun functions-given-scale-degree-and-quality (scale-degree quality)
  "Given a scale degree and chord quality, return the functions that chord might have."
 ;; (let ((chord-quality-symbol (chord-quality-index-to-symbol quality)))
    ;(format t "~%Checking for functions ")
    ;(format t "~10t scale-degree: ~a" scale-degree)
    ;(format t "~10t symbol: ~a" chord-quality-symbol)
  (getf (nth scale-degree *chord-functions*) (chord-quality-index-to-symbol quality)))

(defun pitch-to-scale-degree-given-key (pitch key)
  "Given a pitch, return its scale degree in the given key. Assuming major keys for the time being."
  (mod (- pitch key) *NUM-PITCH-CLASSES*))

(defun chord-quality-index-to-symbol (index)
  (nth index *chord-quality-symbols*))

(defun inversion-index-to-name (index)
  (nth index *inversion-names*))

;; TODO: defun diatonic-mapping -- returns expected diatonic chord quality based on scale-degree

;; TODO: add an "allowed in" that says whether these functions are allowed in major or minor keys, or both.
;; right now data only for major keys or both.
(setq *chord-functions*  '(

;;0 - Tonic
  (
    :maj ((:numeral "I")
            (:numeral "V/iv" :moves-to 5))
    :dom7 ((:numeral "V7/IV" :moves-to 5)
           (:numeral "V7/iv" :moves-to 5))
  )

;; 1 - Raised Tonic
  (
    :dim ((:numeral "vii°/ii" :moves-to 2))
    :maj ((:numeral "N" :moves-to 7))
  )

;; 2 - Supertonic
  (
    :maj ((:numeral "V/V"))
    :min ((:numeral "ii"))
    :dom7 ((:numeral "V7/V"))
  )

;; 3 - Raised Supertonic
  (
    :diminished ((:numeral "vii°/iii"))
  )

;; 4 - Mediant
  (
    :maj ((:numeral "V/vi" :moves-to 9))
    :dim ((:numeral "vii°/IV" :moves-to 5))
  )

;; 5 - Subdominant
  (
    :maj ((:numeral "IV"))
    :min ((:numeral "iv" :chromatic T ))
  )

;; 6 - Raised Subdominant
  ()

;; 7 - Dominant
  (
    :maj ((:numeral "V"))
  )

;; 8 - Raised Dominant / Lowered Minor Submediant
  (
    :maj ((:numeral "bVI" :chromatic T))
  )

;; 9 - Submediant
  (
    :min ((:numeral "vi"))
  )

;; 10 - Subtonic
  (
    :maj ((:numeral "bVII"))
  )

;; 11 - Leading Tone
  (
    :dim ((:numeral "vii°"))
  )

))

; TODO: need to modify the 2nd inversion if it is a 7th chord.

(setq *inversion-names*  '(
  ;0
  ""
  ;1
  "6"
  ;2
  "6/4"
  ;3
  "4/2"
))