; COPE-EVENT FUNCTIONS
; Functions performing common tasks on cope events.
; ---------------------------------------------------------

#|
midi-pitch-to-pitch-class
|#
(defun midi-pitch-to-pitch-class (midi-pitch)
  "return the pitch class for a given midi note number"
  (mod midi-pitch 12))

#|
pitch-class-to-note-name
|#
(defun pitch-class-to-note-name (pitch-class)
  "return the note name for a given pitch class"
  (nth pitch-class '(C C# D D# E F F# G G# A A# B)))

(defun cope-event-to-note-name (cope-event)
  "return the note name for a given cope event"
  (pitch-class-to-note-name (midi-pitch-to-pitch-class (second cope-event))))

(defun highest-pitch-in-cope-events (cope-events)
  "return the highest MIDI pitch in a list of cope events."
  (apply #'max (mapcar #'second cope-events)))

(defun lowest-pitch-in-cope-events (cope-events)
  "return the lowest MIDI pitch in a list of cope events."
  (apply #'min (mapcar #'second cope-events)))


(defun lowest-cope-event-in-cope-events-helper (cope-events min lowest-so-far)
  "Helper method for lowest-cope-event-in-cope-events."
  (if (null cope-events) lowest-so-far
    (if (< (second (first cope-events)) min)
      (lowest-cope-event-in-cope-events-helper (rest cope-events) (second (first cope-events)) (first cope-events))
      (lowest-cope-event-in-cope-events-helper (rest cope-events) min lowest-so-far))))

(defun lowest-cope-event-in-cope-events (cope-events)
  "Find the lowest-pitched cope event among a list of cope events."
  (if (null cope-events) ()
    (lowest-cope-event-in-cope-events-helper cope-events (second (first cope-events)) (first cope-events))))


(defun split-cope-event-groups-by-register-helper (cope-event-group split down)
  (cond ((null cope-event-group) ())
        ((equal down T)
          (if (>= split (second (first cope-event-group)))
                (cons (first cope-event-group)
                      (split-cope-event-groups-by-register-helper (rest cope-event-group) split down))
                (split-cope-event-groups-by-register-helper (rest cope-event-group) split down)))
        (T
          (if (< split (second (first cope-event-group)))
                (cons (first cope-event-group)
                      (split-cope-event-groups-by-register-helper (rest cope-event-group) split down))
                (split-cope-event-groups-by-register-helper (rest cope-event-group) split down)))))

(defun split-cope-event-groups-by-register (cope-event-groups &optional (split 60) (down nil))
  (if (null cope-event-groups) ()
    (cond ((null (first cope-event-groups))
             (cons nil (split-cope-event-groups-by-register (rest cope-event-groups) split down)))
          (T (cons (split-cope-event-groups-by-register-helper (first cope-event-groups) split down)
                   (split-cope-event-groups-by-register (rest cope-event-groups) split down))))))


#|
group-by-simultaneous-start

group note events that start at the same time into lists. Note events that occur
on their own are put into their own list. The notes must be provided in order for
this function to work.

(group-by-simultaneous-start *events*)
|#
(defun group-cope-events-by-simultaneous-start (notes)
  "group note events that start at the same time into lists"
  (if (null notes) ()
    ;; recursive-result: (list) the recursive result for the rest of the list
    ;; last-result: (list) the most recent tuple on the result list
    ;; last-value: (atom) the next value to be processed from input list
    (let* ((recursive-result (group-cope-events-by-simultaneous-start (rest notes)))
           (last-result (first recursive-result))
           (last-note (first notes)))
      ;; check 3 possible conditions:
      (cond
            ;; last-result is nil (at end of list and need to add first tuple)
            ((null last-result)
             (cons (list (first notes)) recursive-result))
            ;; last-result matches last-value (add last value to adjacent tuple)
            ((= (first (first last-result)) (first last-note))
             (cons (cons last-note last-result) (rest recursive-result)))
            ;; otherwise, add a new tuple containing our non-matching number
            (T (cons (list last-note) recursive-result))))))


(defun cope-event-groups-to-pitch-groups (cope-event-groups)
  "Given a set of cope events, create lists of (pitch pitch ...) for every discrete onset time."
  (if (null cope-event-groups) ()
    (cons (mapcar #'second (first cope-event-groups))
          (cope-event-groups-to-pitch-groups (rest cope-event-groups)))))


(defun cope-event-groups-to-pitch-groups-with-times (cope-event-groups)
  "Given a set of cope events, create lists of (onset-time (pitch pitch ...)) for every discrete onset time."
  (if (null cope-event-groups) ()
    (cons (list (first (first (first cope-event-groups))) (mapcar #'second (first cope-event-groups)))
          (cope-event-groups-to-pitch-groups-with-times (rest cope-event-groups)))))

