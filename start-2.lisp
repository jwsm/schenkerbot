(load "/Users/jwsm/Desktop/WACM/project/project-v2/libs/project-libs.lisp")
(setq *work-dir* "/Users/jwsm/Desktop/WACM/project/")

#|
; OPTION 1
(setq *input-events* b206b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)

; OPTION 3
|#
(setq *input-events* b306b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)

#|
; OPTION 2
(setq *input-events* (get-midi "invent8.mid"))
(setq *first-bar* 0)
(setq *bar-length* 1500)
(setq *beats-per-bar* 3)

(setq *surface-level-groups* (cope-events-to-event-groups *input-events*))
(print-groups *surface-level-groups*)
(left-hand-events (first *surface-level-groups*))
|#
;(saveit)




; ---------------------------------------------------------
; Main Program
; ----------------------------------------------------------

(defun run-analysis ()
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

  (setq *surface-level-groups* (cope-events-to-event-groups *input-events*))
  (setq *chord-root-groups* (chord-roots-to-cope-event-groups (calculate-roots *input-events*)))
  
  (mapcar #'self-analysis *surface-level-groups*)


  ;;(setf (schenker-levels (first *surface-level-groups*)) 2)
  
  ;(mapcar #'print-group *surface-level-groups*)

  (run-schenker)

  ;; print left hand to lily-pond
  (setf *surface-level-filtered-by-beat* (filter-event-groups-by-time *surface-level-groups* *first-bar* *ms-per-beat*))
  (lily-pond-file-from-event-groups *surface-level-filtered-by-beat* T)

  T
)

(mapcar #'print-group *surface-level-filtered-by-beat*)

(run-analysis)


(list-schenker-level 2 *surface-level-groups*)

