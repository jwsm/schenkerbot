(load "/Users/jwsm/Desktop/WACM/project/project-v2/libs/project-libs.lisp")
(setq *work-dir* "/Users/jwsm/Desktop/WACM/project/")

#|
(setq *input-events* b206b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)

(setq *surface-level-groups* (cope-events-to-event-groups *input-events*))
(print-groups *surface-level-groups*)
(left-hand-events (first *surface-level-groups*))

|#
;(saveit)

;(prompt-for-piece-info)
; TEST INFO
;; These parameters are piece-specific and should be read in at the prompt
;(setq *events* (get-midi "invent8.mid"))
;(setq *first-bar* 0)
;(setq *bar-length* 1500)
;(setq *beats-per-bar* 3)


; ---------------------------------------------------------
; Main Program
; ----------------------------------------------------------

(defun run-analysis ()
  ; Calculate the ms-per-beat
  (setq *ms-per-beat* (/ *bar-length* *beats-per-bar*))

  (setq *roots-list* ())
  (setq *roots-list* (calculate-roots *input-events*))

  (count-chord-roots *roots-list*)

  (setq *key* (find-key-of-piece *events*))
  T
)

;(run-analysis)
;*roots-list*
