
;(defun note-is-in-key (cope-event)


(defun note-is-scale-degree-n (cope-event n)
  "Check to see if note is of scale degree n"
  (equal (midi-pitch-to-pitch-class (second cope-event)) n))

#|
note-is-harmonized-by-scale-degree-n
WORKING PROPERLY: returns 1 if it is harmonized by the given root, 0 if not
|#
(defun note-is-harmonized-by-scale-degree-n (cope-event scale-degree groups-list)
  "Returns 1 if note is harmonized by root n"
  (let ((group-at-event-time (event-group-starting-at groups-list (first cope-event))))
  	(if (null group-at-event-time) 0
  		(if (equal (scale-degree-of-root group-at-event-time) scale-degree) 1 0))))

(defun note-is-near-beginning (cope-event)
  "Fuzzy function, returns 1 if note is near beginning."
  	(FZ-MEMBERSHIP *near-beginning* (/1000 second (cope-event)))


  (let ((last-onset (start-time (first (last event-groups-list)))))
  	(FZ-MAKE-LINEAR-DOWN last-onset
  						 0
  						 4)

  	(- 1
	(linear-interpolate *first-bar*		;earliest time for a note
						(+ *first-bar* (* *ms-per-beat* 20)) ;20 beats in is not near beginning
						(first cope-event)))) ; the time of the event

(defun note-is-near-end (cope-event event-groups-list)
  "Fuzzy function, returns 1 if note is near the end."
  (let ((last-onset (start-time (first (last event-groups-list)))))
  	(- 1
  	(linear-interpolate last-onset
  						(- last-onset (* *ms-per-beat* 20))		;earliest time for a note
						(first cope-event)))))

(defun note-is-near-top-of-range (cope-event cope-events-list)
  "Fuzzy function, returns 1 if note is near top of range."
  (let ((highest-pitch-in-cope-events cope-events-list))
  		(FZ-MAKE-LINEAR-UP highest-pitch-in-cope-events
  							(- highest-pitch-in-cope-events 12)
  							highest-pitch-in-group)))

(defun note-is-top-voice (cope-event event-groups-list)
  "Fuzzy function, returns 1 if note is in (or near) top voice."
  (let* ((group-at-event-time (event-group-starting-at event-groups-list (first cope-event)))
  		 (highest-pitch-in-group (find-highest-pitch-in-cope-events (cope-events group-at-event-time))))
  		 (cond ((null group-at-event-time) 0 )
  			  ((equal highest-pitch-in-group (second cope-event)) 1)
  			  (T 0))))

(defun note-is-harmonized (cope-event)
  "Fuzzy function, returns 1 if note is properly harmonized within the key.")



; Function to set Schenker Level


;(defun check-each-event-group (event-groups)
;	(if (null event-groups) ()



;(defun select-second-level (cope-events-list event-groups-list)