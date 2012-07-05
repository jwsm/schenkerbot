(load "/Users/jwsm/Desktop/WACM/project/libs/project-libs.lisp")
(setq *work-dir* "/Users/jwsm/Desktop/WACM/project/")

#|
(setq *events* b206b)
(setq *first-bar* 0)
(setq *bar-length* 4000)
(setq *beats-per-bar* 4)
|#
;(saveit)

;(prompt-for-piece-info)
; TEST INFO
;; These parameters are piece-specific and should be read in at the prompt
(setq *events* (get-midi "invent8.mid"))
(setq *first-bar* 0)
(setq *bar-length* 1500)
(setq *beats-per-bar* 3)



; ---------------------------------------------------------
; Main Program
; ----------------------------------------------------------

(defun run-analysis ()
  ; Calculate the ms-per-beat
  (setq *ms-per-beat* (/ *bar-length* *beats-per-bar*))

  ; Load cope events into tree structure
  (setf *tree* ())
  (cope-events-to-beat-groups *events*)

  (mapcar #'find-pitch-set *tree*)
  (mapcar #'find-chord-probs *tree*)
  (mapcar #'find-best-chord *tree*)
  (mapcar #'(lambda (x) (find-chord-functions (find-key-of-piece *events*) x)) *tree*)

  (count-best-chord-roots *tree*)
  (count-all-pitches *events*)
  
  T
)

(run-analysis)

; Print out the tree structure
(print-tree *tree*)

(count-best-chord-roots *tree*)
(count-cope-events-within-all-scales-of-type *events* *MAJ-SCALE*)

(pitch-class-to-note-name (find-key-of-piece *events*))