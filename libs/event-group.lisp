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
    :initform 0)
   (duration
    :initarg :duration
    :initform 0)
   (cope-events :initform ())
   (root :initform ())
   (quality :initform ())))

; ---------------------------------------------------------
; Generic Method Declarations
; ---------------------------------------------------------

; Accessors

(defgeneric start-time (eg))
(defgeneric (setf start-time) (time eg))

(defgeneric duration (eg))
(defgeneric (setf duration) (d eg))

(defgeneric cope-events (eg))
(defgeneric (setf cope-events) (events eg))

(defgeneric root (eg))
(defgeneric (setf root) (r eg))

(defgeneric quality (eg))
(defgeneric (setf quality) (q eg))


(defgeneric add-cope-event-to-group (cope-event eg))

; Value Added

(defgeneric pitches (eg))
(defgeneric pitch-set (eg))
(defgeneric left-hand-events (eg &optional left-hand))
(defgeneric right-hand-events (eg))

; Print Methods

(defgeneric print-group (eg))


; ---------------------------------------------------------
; Method Implementations
; ----------------------------------------------------------

; Accessor Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod start-time ((eg event-group))
  (slot-value eg 'start-time))

(defmethod (setf start-time) (time (eg event-group))
  (setf (slot-value eg 'start-time) time))

(defmethod duration ((eg event-group))
  (slot-value eg 'duration))

(defmethod (setf duration) (d (eg event-group))
  (setf (slot-value eg 'duration) d))

(defmethod cope-events ((eg event-group))
  (slot-value eg 'cope-events))

(defmethod (setf cope-events) (events (eg event-group))
  (setf (slot-value eg 'cope-events) events))

(defmethod root ((eg event-group))
  (slot-value eg 'root))

(defmethod (setf root) (r (eg event-group))
  (setf (slot-value eg 'root) r))

(defmethod quality ((eg event-group))
  (slot-value eg 'quality))

(defmethod (setf quality) (q (eg event-group))
  (setf (slot-value eg 'quality) q))

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

; Printing Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod print-group ((eg event-group))
  (format t "Event-Group")
  (format t "~10t Count: ~a" (length (cope-events eg)))
  (format t "~10t Start: ~a" (start-time eg))
  (format t "~10t Duration: ~a ~%" (duration eg)))

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

