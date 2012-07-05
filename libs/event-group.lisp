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

; Value Added

(defgeneric pitches (eg))
(defgeneric pitch-set (eg))

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


; Printing Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod print-group ((eg event-group))
  (format t "~%Event-Group ~%")
  (format t "Start: ~a ~%" (start-time eg))
  (format t "Duration: ~a ~%" (duration eg)))

