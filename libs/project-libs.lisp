; ---------------------------------------------------------
; Define Global Vars
; ----------------------------------------------------------
(defvar *first-bar* ())
(defvar *bar-length* ())
(defvar *beats-per-bar* ())
(defvar *ms-per-beat* ())
(defvar *input-events* ())
(defvar *roots-list* ())
(defvar *key* ())

; ---------------------------------------------------------
; Define Global Path Vars
; ----------------------------------------------------------

(defvar *libs-base-path*)
(defvar *project-base-path*)
(defvar *project-libs-base-path*)


(setq *libs-base-path* "/Users/jwsm/Desktop/WACM/libs/")
(setq *project-base-path* "/Users/jwsm/Desktop/WACM/project/")
(setq *project-libs-base-path* "/Users/jwsm/Desktop/WACM/project/libs/")

; ---------------------------------------------------------
; Load Libraries
; ----------------------------------------------------------

;; WACM-provided libraries
(load (concatenate 'string *libs-base-path* "wacm/mgc.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/fuzzy.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/fuzzy_music11.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/fz_catch_chord.lisp"))

;; My libraries
;(load (concatenate 'string *project-libs-base-path* "user-prompt.lisp"))

(load (concatenate 'string *project-libs-base-path* "chords.lisp"))
(load (concatenate 'string *project-libs-base-path* "jsm-fuzzy.lisp"))
(load (concatenate 'string *project-libs-base-path* "fuzzy-chords.lisp"))
(load (concatenate 'string *project-libs-base-path* "fz-catch-key.lisp"))
(load (concatenate 'string *project-libs-base-path* "event-group.lisp"))
;(load (concatenate 'string *project-libs-base-path* "beat-group.lisp"))
(load (concatenate 'string *project-libs-base-path* "cope-event-functions.lisp"))
(load (concatenate 'string *project-libs-base-path* "set-functions.lisp"))
;(load (concatenate 'string *project-libs-base-path* "make-groups.lisp"))
(load (concatenate 'string *project-libs-base-path* "tree-parser.lisp"))
(load (concatenate 'string *project-libs-base-path* "macro-analysis.lisp"))
(load (concatenate 'string *project-libs-base-path* "lily-pond.lisp"))

;; Data
(load (concatenate 'string *project-base-path* "jsb1.lisp"))