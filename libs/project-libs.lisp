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

(defconstant *NUM-PITCH-CLASSES* 12)


; attributes
(defvar *last-onset* nil)
(defvar *total-beats* nil)
(defvar *highest-pitch* nil)
(defvar *lowest-pitch* nil)

(defconstant *near-in-beats* 20)
(defconstant *near-in-pitches* 12)
(defconstant *occurring-often-for-pitches* 30)

; groups
(defvar *CHORD-ROOT-GROUPS* ())
(defvar *SURFACE-LEVEL-GROUPS* ())
(defvar *surface-level-filtered-by-beat* ())


; ---------------------------------------------------------
; Define Global Path Vars
; ----------------------------------------------------------

(defvar *libs-base-path*)
(defvar *project-base-path*)
(defvar *project-libs-base-path*)
(defvar *data-file-path*)


(setq *libs-base-path* "/Users/jwsm/Desktop/WACM/libs/")
(setq *project-base-path* "/Users/jwsm/Desktop/WACM/project/project-v2/")
(setq *project-libs-base-path* "/Users/jwsm/Desktop/WACM/project/project-v2/libs/")
(setq *data-file-path* "/Users/jwsm/Desktop/WACM/project/")

; ---------------------------------------------------------
; Load Libraries
; ----------------------------------------------------------

;; WACM-provided libraries
(load (concatenate 'string *libs-base-path* "wacm/mgc.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/rand.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/fuzzy.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/fuzzy_music11.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/fuzzy-key.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/histo-key.lisp"))
;(load (concatenate 'string *libs-base-path* "wacm/fz_catch_chord.lisp"))
(load (concatenate 'string *project-libs-base-path* "fz_catch_chord-altered.lisp"))
(load (concatenate 'string *libs-base-path* "wacm/root-lister.lisp"))

;; My libraries
;(load (concatenate 'string *project-libs-base-path* "user-prompt.lisp"))

;(load (concatenate 'string *project-libs-base-path* "chords.lisp"))
(load (concatenate 'string *project-libs-base-path* "jsm-fuzzy.lisp"))
(load (concatenate 'string *project-libs-base-path* "fuzzy-chords.lisp"))
(load (concatenate 'string *project-libs-base-path* "fz-catch-key.lisp"))
(load (concatenate 'string *project-libs-base-path* "event-group.lisp"))
(load (concatenate 'string *project-libs-base-path* "cope-event-functions.lisp"))
(load (concatenate 'string *project-libs-base-path* "set-functions.lisp"))
;(load (concatenate 'string *project-libs-base-path* "tree-parser.lisp"))
;(load (concatenate 'string *project-libs-base-path* "macro-analysis.lisp"))
(load (concatenate 'string *project-libs-base-path* "lily-pond.lisp"))
(load (concatenate 'string *project-libs-base-path* "fuzzy-schenker.lisp"))

;; Data
(load (concatenate 'string *data-file-path* "jsb1.lisp"))