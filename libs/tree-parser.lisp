; ---------------------------------------------------------
; Global Vars
; ----------------------------------------------------------
(defvar *tree*)
(setq *tree* ())

; ---------------------------------------------------------
; Functions
; ----------------------------------------------------------

#|
add-event-to-beat-group
Called by parse-tree find-events-up-until-time; adds a cope event to a beat group object.
|#
(defun add-event-to-beat-group (event beat-group)
  "Add a cope event tuple into a beat group object."
  ;(format t "Adding event ~a to beat group ~a ~%" (first event) beat-group)
  (add-event event beat-group))


#|
add-beat-group-to-tree
Called by parse-tree; adds new beat groups to global tree variable after they are created
|#
(defun add-beat-group-to-tree (beat-group)
  "Add a beat group object onto the global *tree* variable."
  (setf *tree* (append *tree* (list beat-group))))


(defun cope-events-to-beat-groups-helper (events current-beat-start current-beat-group)
  "Helper function that adds cope events to current beat group;
  creates a new beat group when notes from the current beat are exhausted."
  (if (null events) T
    (let ((next-beat-start (+ current-beat-start *ms-per-beat*)))
    (cond ((< (first (first events)) next-beat-start)
           ;(format t "Time: ~a~10t Pitch: ~a ~%" (first (first events)) (cope-event-to-note-name (first events)))
           (add-event-to-beat-group (first events) current-beat-group)
           (cope-events-to-beat-groups-helper (rest events) current-beat-start current-beat-group))
          (T 
           ;(format t "~%Current Beat ~a ~%" (+ current-beat-start *ms-per-beat*))
           (let ((next-beat-group (make-instance 'beat-group
                                    :start-time next-beat-start
                                    :duration *ms-per-beat*)))
             (add-beat-group-to-tree next-beat-group)
             (cope-events-to-beat-groups-helper events next-beat-start next-beat-group)))))))


#|
Create a set of beat-group objects out of a set of cope events.
(setf *events* (get-midi "invent8.mid"))
(setf *tree* ())
(cope-events-to-beat-groups *events*)
|#
(defun cope-events-to-beat-groups (events)
  "Given a set of cope events, parse them into beat group objects, stored in *tree*"
  (let ((next-beat-group (make-instance 'beat-group
                           :start-time (first (first events))
                           :duration *ms-per-beat*)))
    (add-beat-group-to-tree next-beat-group)
    (cope-events-to-beat-groups-helper events *first-bar* next-beat-group)))


; Sort the events by time, ascending
;(setf *events* (sort *events* #'(lambda (x y) (< (first x) (first y)))))

#|
(print-tree *tree*)
|#
(defun print-tree (tree)
  (if (null tree) T
    (progn
      (print-beat (first tree))
      (print-tree (rest tree)))))

