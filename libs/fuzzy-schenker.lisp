


; create membership sets for each rule (membership sets organized by bar and by pitch class)

; use FZ-membership to look up score for each note in given set (i.e. set of near beginning)
;
;;; add up all results
;; stick in list that has one entry for every note
;; ltop will bring out highest scoring one



(defvar *fz-near-beginning* ())
(defvar *fz-near-end* ())
(defvar *fz-near-top-of-range* ())
(defvar *fz-near-bottom-of-range* ())


(defun beat-index-to-onset-time (beat)
	(+ *first-bar* (* beat *ms-per-beat*)))

(defun cope-event-to-beat-index (cope-event)
	(1- (floor (/ (- (first cope-event) *first-bar*) *ms-per-beat*))))

(defun run-schenker ()
	
	; Near the beginning of the piece (by beat)?
	(setq *fz-near-beginning*
		(fz-make-linear-down *near-in-beats* 10 *near-in-beats*))

	; Near the end of the piece (by beat)?
	(setq *fz-near-end*
		(fz-make-linear-up 	*near-in-beats* (- (- *total-beats* *near-in-beats*) 10) *total-beats*))
	
	; Near the top of the range (by MIDI pitch)?
	(setq *fz-near-top-of-range*
		(fz-make-linear-up *near-in-pitches* (- *highest-pitch* *near-in-pitches*) *highest-pitch*))

	; Near the bottom of the range (by MIDI pitch)?
	(setq *fz-near-bottom-of-range*
		(fz-make-linear-down *near-in-pitches* *lowest-pitch* (+ *near-in-pitches* *lowest-pitch*)))


	; Here is the flat set to start out.
	;(make-flat (length *surface-level-groups*) 0)

	(let* ((top-indexes (top-n-positions (weigh-rules-on-each-cope-event *input-events* (function is-second-urlinie-note)) 1))
		   (top-cope-events (mapcar #'(lambda (index) (nth index *input-events*)) top-indexes)))

		 top-cope-events)
	

	;(weigh-rules)
)



(defun weigh-rules-on-each-cope-event (cope-events rules-function)
	(if (null cope-events) ()
	  (progn
		(format t "value for event ~a ~a ~%" (first cope-events) (funcall rules-function (first cope-events)))
		(cons (funcall rules-function (first cope-events))
			  (weigh-rules-on-each-cope-event (rest cope-events) rules-function)))))


;; pick the first note of the urlinie
; (defun weigh-rules (cope-event)
; 	(let ((beat-index (cope-event-to-beat-index cope-event))
; 		  (pitch (second cope-event)))
; 	(+
; 		(* (fz-membership *fz-near-beginning* beat-index) 0.5)
; 		(* (fz-membership *fz-near-top-of-range* pitch) 1.0)
; 		(* (note-is-top-voice cope-event *surface-level-groups*) 1.0)
; 		(* (pitch-occurs-often pitch *input-events*) 0.8)
; 		(* (note-is-harmonized-by-scale-degree-n cope-event 0 *surface-level-groups*) 1.0)
; 		; also check: is the harmonizing chord in root position, or is there a nearby root position chord to "bind" to?
; 	)
; ))



(defun is-first-urlinie-note (cope-event)
	(let ((beat-index (cope-event-to-beat-index cope-event))
		  (pitch (second cope-event)))
	(+
		(* (fz-membership *fz-near-beginning* beat-index) 0.5)
		(* (fz-membership *fz-near-top-of-range* pitch) 1.0)
		(* (note-is-top-voice cope-event *surface-level-groups*) 1.0)
		(* (pitch-occurs-often pitch *input-events*) 0.8)
		(* (note-is-harmonized-by-scale-degree-n cope-event 0 *surface-level-groups*) 1.0)
		; also check: is the harmonizing chord in root position, or is there a nearby root position chord to "bind" to?
		(* (pitch-is-scale-degree-n pitch 4) 1) ; pitch could be 3
		;(* (pitch-is-scale-degree-n pitch 7) 0.5) ; or pitch could be 5 -- only allowing 3-lines for the time being
	)
))

(defun is-second-urlinie-note (cope-event)
	(let ((beat-index (cope-event-to-beat-index cope-event))
		  (pitch (second cope-event)))
	(+
		(* (fz-membership *fz-near-end* beat-index) 0.5)
		(* (fz-membership *fz-near-bottom-of-range* pitch) 0.1) ; could also check for middle of range
		(* (note-is-top-voice cope-event *surface-level-groups*) 1.0)
		(* (pitch-occurs-often pitch *input-events*) 0.0)
		(* (note-is-harmonized-by-scale-degree-n cope-event 7 *surface-level-groups*) 1.0)
		; also check: is the harmonizing chord in root position, or is there a nearby root position chord to "bind" to?
		(* (pitch-is-scale-degree-n pitch 11) 0.8) ; pitch could be 7
		(* (pitch-is-scale-degree-n pitch 2) 0.8) ; or pitch could be 2
	)
))
; (is-second-urlinie-note '(3000 48 1000 4 96))

(defun is-third-urlinie-note (cope-event)
	(let ((beat-index (cope-event-to-beat-index cope-event))
		  (pitch (second cope-event)))
	(+
		(* (fz-membership *fz-near-end* beat-index) 2.0)
		(* (fz-membership *fz-near-bottom-of-range* pitch) 0.1)
		(* (note-is-top-voice cope-event *surface-level-groups*) 1.0)
		(* (pitch-occurs-often pitch *input-events*) 0.0)
		(* (note-is-harmonized-by-scale-degree-n cope-event 0 *surface-level-groups*) 1.0)
		; also check: is the harmonizing chord in root position, or is there a nearby root position chord to "bind" to?
		(* (pitch-is-scale-degree-n pitch 0) 1) ; pitch must be 1
	)
))



(defun note-is-harmonized-by-scale-degree-n (cope-event scale-degree groups-list)
  "Returns 1 if note is harmonized by root n, 0 if not"
  (let ((group-at-event-time (event-group-starting-at groups-list (first cope-event))))
  	(if (null group-at-event-time) 0
  		(if (equal (scale-degree-of-root group-at-event-time) scale-degree) 1 0))))


(defun note-is-top-voice (cope-event event-groups-list)
  "Fuzzy function, returns 1 if note is in (or near) top voice, 0 if not"
  (let* ((group-at-event-time (event-group-starting-at event-groups-list (first cope-event)))
  		 (highest-pitch-in-group (find-highest-pitch-in-cope-events (cope-events group-at-event-time))))
  		 (cond ((null group-at-event-time) 0 )
  			  ((equal highest-pitch-in-group (second cope-event)) 1)
  			  (T 0))))

(defun pitch-occurs-often (pitch cope-events)
	"Fuzzy function, returns 1 if pitch occurs 'often' in the piece, 0 if not"
	(min 1 (/ (count pitch cope-events :key #'second) *occurring-often-for-pitches*)))


(defun pitch-is-scale-degree-n (pitch n)
  "Returns 1 if pitch is scale degree n within key"
	(if (equal (pitch-to-scale-degree-given-key pitch *key*) n) 1 0))


;(run-schenker)

