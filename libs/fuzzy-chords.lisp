(defun calculate-roots-helper (pitches-by-chord-wt)
  "Return lists of (onset (root chord-type)) for each group of pitches."
  (if (null pitches-by-chord-wt) ()
    (cons (list (first (first pitches-by-chord-wt)) ; onset time
                (chord-root-and-type (second (first pitches-by-chord-wt)))) ; (root chord-type)
          (calculate-roots-helper (rest pitches-by-chord-wt)))))

#|
    (progn
      (format t "onset: ~a " (first (first pitches-by-chord-wt)))
      (post-chord-root-and-type (second (first pitches-by-chord-wt)))
      (format t "~%")
      (calculate-roots-helper (rest pitches-by-chord-wt)))))
|#

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
