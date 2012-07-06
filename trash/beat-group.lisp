; BEAT-GROUP OBJECT
; ---------------------------------------------------------
; This object defines a beat-group, meaning the set of notes
; falling within the same beat in the piece.

; ---------------------------------------------------------
; Class Definition
; --------------------------------------------------------
(defclass beat-group ()
  ((start-time
    :initarg :start-time
    :initform 0)
   (duration
    :initarg :duration
    :initform 0)
   (note-events :initform ())
   (pitch-set :initform ())
   (chord-probs :initform ())
   (best-chord :initform ())
   (best-chord-fn :initform ())
   (key-probs :initform ())))


; ---------------------------------------------------------
; Generic Method Declarations
; ---------------------------------------------------------

; Accessors
(defgeneric start-time (bg))
(defgeneric (setf start-time) (time bg))

(defgeneric duration (bg))
(defgeneric (setf duration) (d bg))

(defgeneric note-events (bg))
(defgeneric (setf note-events) (events bg))

(defgeneric pitch-set (bg))
(defgeneric (setf pitch-set) (ps bg))

(defgeneric chord-probs (bg))
(defgeneric (setf chord-probs) (cps bg))

(defgeneric best-chord (bg))
(defgeneric (setf best-chord) (bc bg))

(defgeneric best-chord-fn (bg))
(defgeneric (setf best-chord-fn) (bcf bg))

(defgeneric key-probs (bg))
(defgeneric (setf key-probs) (kps bg))


; Work Methods
(defgeneric print-beat (bg))
(defgeneric add-event (event bg))
(defgeneric find-chord-probs (bg))
(defgeneric find-pitch-set (bg))
(defgeneric find-chord-functions (key bg))

; ---------------------------------------------------------
; Method Implementations
; ----------------------------------------------------------

; Accessor Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod start-time ((bg beat-group))
  (slot-value bg 'start-time))

(defmethod (setf start-time) (time (bg beat-group))
  (setf (slot-value bg 'start-time) time))

(defmethod duration ((bg beat-group))
  (slot-value bg 'duration))

(defmethod (setf duration) (d (bg beat-group))
  (setf (slot-value bg 'duration) d))

(defmethod note-events ((bg beat-group))
  (slot-value bg 'note-events))

(defmethod (setf note-events) (events (bg beat-group))
  (setf (slot-value bg 'note-events) events))

(defmethod pitch-set ((bg beat-group))
  (slot-value bg 'pitch-set))

(defmethod (setf pitch-set) (ps (bg beat-group))
  (setf (slot-value bg 'pitch-set) ps))

(defmethod chord-probs ((bg beat-group))
  (slot-value bg 'chord-probs))

(defmethod (setf chord-probs) (cps (bg beat-group))
  (setf (slot-value bg 'chord-probs) cps))

(defmethod best-chord ((bg beat-group))
  (slot-value bg 'best-chord))

(defmethod (setf best-chord) (bc (bg beat-group))
  (setf (slot-value bg 'best-chord) bc))

(defmethod best-chord-fn ((bg beat-group))
  (slot-value bg 'best-chord-fn))

(defmethod (setf best-chord-fn) (bcf (bg beat-group))
  (setf (slot-value bg 'best-chord-fn) bcf))

(defmethod key-probs ((bg beat-group))
  (slot-value bg 'key-probs))

(defmethod (setf key-probs) (kps (bg beat-group))
  (setf (slot-value bg 'key-probs) kps))


; Work Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


; Printing Methods
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod print-beat ((bg beat-group))
  (format t "~%Beat ~%")
  (format t "Starting at: ~a ~%" (start-time bg))
  (format t "Containing: ~%")
  (print-events (note-events bg))
  (format t "Pitch set: (")
  (print-pitch-set-pitches (pitch-set bg))
  (format t ") ~%")
  (if (not (null (second (best-chord bg))))
    (format t "Best Chord: ~a ~a with probability ~a ~%"
            (pitch-class-to-note-name (second (best-chord bg)))
            (first (best-chord bg))
            (third (best-chord bg)))))

;  (print-chord-probs (chord-probs bg)))

(defun print-pitch-set-pitches (set)
  "Print the pitch of a set of pitches."
  (if (null set) T
    (progn
      (format t " ~a" (first set))
      (print-pitch-set-pitches (rest set)))))

(defun print-events (events)
  "Print a list of events, giving each one's starting pitch."
  (if (null events) T
    (progn
      (format t "Event: ~a ~%" (pitch-class-to-note-name (midi-pitch-to-pitch-class (second (first events)))))
      (print-events (rest events)))))

#|
(defun print-chord-probs (chord-probs)
  "Print out the list of chord probabilities from a beat group."
  (if (null chord-probs) T
    (progn
      (format t "Chord: ~a Prob: ~a ~%" (second chord-probs) (first chord-probs))
      (print-chord-probs (rest chord-probs)))))
|#

; Add Event to Beat Group
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod add-event (event (bg beat-group))
  "Add an event to a beat group."
  (setf (note-events bg) (append (note-events bg) (list event))))


; Find Pitch Sets
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defun find-pitch-set-helper (cope-events)
  "Helper function to determine a pitch set from a list of cope-events."
  (if (null cope-events) *FZ-EMPTY*
    (mapcar #'bounded-+
            (pitch-set-for-midi-pitch (second (first cope-events)))
            (find-pitch-set-helper (rest cope-events)))))

(defmethod find-pitch-set ((bg beat-group))
  "Return a pitch set given a list of cope-events."
  (setf (pitch-set bg) (find-pitch-set-helper (note-events bg))))


; Find Chord Probabilities
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod find-chord-probs ((bg beat-group))
  "Find the chord probabilities for this beat group and store them in this object."
  (setf (chord-probs bg) (compare-all-chords-to-pitch-set (pitch-set bg) *chord-templates*)))


(defmethod find-best-chord ((bg beat-group))
  "Find the best chord for this beat group by selecting the one with the highest probabilitiy."
  (let ((sorted-bg-chords (sort (chord-probs bg)
                                #'(lambda (x y) (> (third x) (third y))))))
    (setf (best-chord bg) (first sorted-bg-chords))))


; Find Roots
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


; Find Chord Functions
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
(defmethod find-chord-functions (key (bg beat-group))
  (if (null (best-chord bg)) ()
    (lookup-function-given-chord-and-key (second (best-chord bg)) key)))