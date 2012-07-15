;#!/usr/local/bin/sbcl --script
(format t "~%")
(format t "Fuzzy Schenker - Gratuitously Fuzzy Development Version")
(format t "-------------------------------")

(load "/Users/jwsm/Desktop/WACM/project/project-v2/libs/project-libs.lisp")
(setq *work-dir* "/Users/jwsm/Desktop/WACM/project/")

; OPTION 1
(setq *input-events* b306b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)

#|
; OPTION 2
(setq *input-events* b206b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)

; OPTION 3
(setq *input-events* (get-midi "invent8.mid"))
(setq *first-bar* 0)
(setq *bar-length* 1500)
(setq *beats-per-bar* 3)

OPTION 4
(setq *input-events* b606b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)
|#
;(saveit)




; ---------------------------------------------------------
; Main Program
; ----------------------------------------------------------

(defun run-analysis (analysis-level)
  (format t "~%Running Analysis...~%")

  ; Calculate the ms-per-beat
  (setq *ms-per-beat* (/ *bar-length* *beats-per-bar*))
  (setq *last-onset* (first (first (last *input-events*))))
  (setq *total-beats* (floor (/ *last-onset* *ms-per-beat*)))
  (setq *highest-pitch* (highest-pitch-in-cope-events *input-events*))
  (setq *lowest-pitch* (lowest-pitch-in-cope-events *input-events*))

  ; Find key using PQE histogram
  ;; TODO: add other methods here and choose the most frequently reported answer
  (let* ((key-guesses
         (sort (list 
          (histo-method *input-events*)
          (key-by-fuzzy-method (list-pcs (copes-to-p-list *input-events*)))
          (key-by-root-histo-method *input-events*)) #'<))
        (best-key (cond ((equal (first key-guesses) (second key-guesses)) (first key-guesses))
                        ((equal (second key-guesses) (third key-guesses)) (second key-guesses))
                        (T (first key-guesses)))))
        (setq *key* best-key))

  (format t "The key is: ~a ~%" *key*) 

  ;; Load the cope-events into event-group objects
  (setq *surface-level-groups* (cope-events-to-event-groups *input-events*))
  ;(setq *chord-root-groups* (chord-roots-to-cope-event-groups (calculate-roots *input-events*)))
  
  (mapcar #'self-analysis *surface-level-groups*)

  ; Run the schenkerian Analysis
  (run-schenker)

  ;; Filter the groups by beat
  (setf *surface-level-filtered-by-beat* (filter-event-groups-by-time *surface-level-groups* *first-bar* *ms-per-beat*))

  ;; print notes, key and functions to lily pond
  (lily-pond-write-key-to-file *key*)

  (lily-pond-file-from-events-list "soprano.txt" (split-cope-event-groups-by-register
                                                (list-schenker-level analysis-level *surface-level-filtered-by-beat*) 60 nil))
  (lily-pond-file-from-events-list "bass.txt" (split-cope-event-groups-by-register
                                               (list-schenker-level analysis-level *surface-level-filtered-by-beat*) 60 T))


  ;(list-roman-numerals 2 *surface-level-groups*)
  (lily-pond-harmonic-functions-from-events-list *surface-level-filtered-by-beat* analysis-level)

  T
)

;(run-analysis 2)

;(mapcar #'print-group *surface-level-filtered-by-beat*)
;(list-schenker-level 2 *surface-level-groups*)


