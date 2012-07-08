; EVENT-GROUP OBJECT
; ---------------------------------------------------------
; This object defines an event-group, a group of cope events
; with some additional information attached.
;
; ---------------------------------------------------------
; Class Definition
; --------------------------------------------------------
(defclass event-group ()
  ((start-time
    :initarg :start-time
    :initform 0
    :accessor start-time)
   (duration
    :initarg :duration
    :initform 0
    :accessor duration)
   (cope-events
    :initform ()
    :accessor cope-events)
   (root
    :initform ()
    :accessor root)
   (quality
    :initform ()
    :accessor quality)
   (inversion
    :initform ()
    :accessor inversion)
   (schenker-levels
    :initform ()
    :accessor schenker-levels)))

; ---------------------------------------------------------
; Generic Method Declarations
; ---------------------------------------------------------

; Custom Accessors
(defgeneric add-cope-event-to-group (cope-event eg))

; Value Added
(defgeneric pitches (eg))
(defgeneric pitch-set (eg))
(defgeneric left-hand-events (eg &optional left-hand))
(defgeneric right-hand-events (eg))
(defgeneric possible-functions (eg))
(defgeneric schenker-level (n eg)) ; return the cope-events in schenker level n
(defgeneric add-cope-event-to-schenker-level (event level eg))

; Print Methods
(defgeneric print-group (eg))

; Analysis Methods
(defgeneric self-analysis (eg))

; ---------------------------------------------------------
; Method Implementations
; ----------------------------------------------------------

; Value-added Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod add-cope-event-to-group (cope-event (eg event-group))
  "Add a cope event to an event group."
  (setf (cope-events eg) (append (cope-events eg) (list cope-event))))

(defun left-right-hand-events-helper (events left-hand &optional (split 61))
  (cond ((null events) ())
        ((and (>= (second (first events)) split) (not left-hand))
          (cons (first events) (left-right-hand-events-helper (rest events) left-hand split)))
        ((and (< (second (first events)) split) left-hand)
          (cons (first events) (left-right-hand-events-helper (rest events) left-hand split)))
        (T (left-right-hand-events-helper (rest events) left-hand split))))

(defmethod left-hand-events ((eg event-group) &optional (left-hand t))
  (left-right-hand-events-helper (cope-events eg) left-hand))

(defmethod right-hand-events ((eg event-group))
  (left-right-hand-events-helper (cope-events eg) nil))

(defmethod pitches ((eg event-group))
  (mapcar #'second (slot-value eg 'cope-events)))

(defmethod inversion ((eg event-group))
  (find-inversion-using-cope-events-and-root (cope-events eg) (root eg)))

(defmethod possible-functions ((eg event-group))
  (let* ((root-scale-degree (pitch-to-scale-degree-given-key (root eg) *key*)))
    (functions-given-scale-degree-and-quality root-scale-degree (quality eg))))

(defmethod scale-degree-of-root ((eg event-group))
  (pitch-to-scale-degree-given-key (root eg) *key*))


; Schenker Level Functions
; add and remove cope events to different levels
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod schenker-level (n (eg event-group))
  (cond ((equal n 1)
          (cope-events eg))
        (T
          (second (assoc n (schenker-levels eg) :test #'equal)))))

(defmethod add-cope-event-to-schenker-level (event level (eg event-group))
  (let* ((current-levels (slot-value eg 'schenker-levels))
         (existing-list (assoc level current-levels :test #'equal)))
    (cond ((null existing-list)
            (setf (schenker-levels eg)
              (cons (list level (list event)) current-levels)))
          (T
            (setf (schenker-levels eg)
              (substitute (list level (cons event (second existing-list)))
                          existing-list
                          current-levels))))))

(defmethod add-cope-event-to-schenker-level-groups (event level event-groups)
  (let ((target-event-group (event-group-starting-at event-groups (first event))))
    (add-cope-event-to-schenker-level
      event
      level
      target-event-group)
    ; also add the corresponding bass note
    (add-cope-event-to-schenker-level
      (lowest-cope-event-in-cope-events (cope-events target-event-group))
      level
      target-event-group)))


; Printing Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod print-group ((eg event-group))
  (format t "Event-Group")
  (format t "~10t Count: ~a" (length (cope-events eg)))
  (format t "~10t Start: ~a" (start-time eg))
  (format t "~10t Dur: ~a " (duration eg))
  (format t "~10t Schenk: ~a " (schenker-levels eg))
  (format t "~10t Root: ~a " (root eg))
  (format t "~10t inv: ~a " (inversion eg))
  (format t "~10t Root in Key: ~a " (scale-degree-of-root eg))
  (format t "~10t Funcs: ~a " (functions-given-scale-degree-and-quality (scale-degree-of-root eg) (quality eg)))
  (format t "~10t Qual: ~a" (chord-quality-index-to-symbol (quality eg)))
;  (format t "~10t Function: ~a" (possible-functions eg))
  (format t "~%"))

(defun print-groups (event-groups)
  (mapcar #'print-group event-groups))

(defun make-cope-event-groups (cope-events-lists)
  (if (null cope-events-lists) ()
    (let* ((new-event-group (make-instance 'event-group
                            :start-time (first (first (first cope-events-lists))))))
      (mapcar #'(lambda (x) (add-cope-event-to-group x new-event-group)) (first cope-events-lists))
      (cons new-event-group (make-cope-event-groups (rest cope-events-lists))))))

(defun cope-events-to-event-groups (cope-events)
  (make-cope-event-groups (group-cope-events-by-simultaneous-start cope-events)))

; Analysis Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(defmethod self-analysis ((eg event-group))
  (let* ((root-and-type (chord-root-and-type (pitches eg)))
         (duration (apply #'max (mapcar #'first (cope-events eg))))
         (inversion (find-inversion-using-cope-events-and-root (cope-events eg) (root eg))))
    (setf (root eg) (first root-and-type))
    (setf (quality eg) (second root-and-type))
    (setf (duration eg) duration)
    (setf (inversion eg) inversion)))



(defun event-group-starting-at (event-group-list ontime)
  "Return the first event group found in event-group-list that starts at ontime."
  (if (null event-group-list) ()
    (if (equal (start-time (first event-group-list)) ontime)
      (first event-group-list)
      (event-group-starting-at (rest event-group-list) ontime))))


(defun filter-event-groups-by-time (event-groups first-bar interval)
  (if (null event-groups) ()
    (if (zerop (mod (- (start-time (first event-groups)) first-bar) interval))
      (cons (first event-groups)
            (filter-event-groups-by-time (rest event-groups) first-bar interval))
      (filter-event-groups-by-time (rest event-groups) first-bar interval))))


(defun list-schenker-level (level event-groups)
  "Return a list containing, for each event group, either NIL or a list of level-n schenker notes given a level n"
  (mapcar #'(lambda (group) (schenker-level level group)) event-groups))

(defun list-harmonic-functions (event-groups)
  (mapcar #'(lambda (x) (list (second (first (possible-functions x)))
                              (inversion-index-to-name (inversion x)))) event-groups))

