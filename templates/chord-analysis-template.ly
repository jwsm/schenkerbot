\version "2.14.2"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A function to create Roman numerals for harmonic analysis.
%%
%% Syntax: \markup \rN { ...list of symbols... }
%%
%% List symbols in this order (as needed): Roman numeral (or note-name)
%% quality, top number of inversion symbol, bottom number, "/" (if secondary
%% function), Roman numeral (or note-name).  Usually, you can skip unnecessary
%% items, though a spacer may be needed in some cases.  Use "" instead of the
%% initial symbol to start with the quality or inversion, for example.
%%
%% Preceding or following a symbol with English alterations (f, s, ff, ss, x, n)
%% will attach accidentals: "fVII" -> flat VII; "svi" -> sharp vi; "Af" -> A-flat;
%% "As" A-sharp
%%
%% Qualities: use "o" for diminished, "h" for half-diminished,
%% "+" for augmented, "f" for flat; other indications are possible such as
%% combinations of "M" and "m" (M, m, MM7, Mm, mm, Mmm9, etc.); add, add6, etc.
%%
%% To scale all numerals: \override  LyricText #'font-size = #2
%% or \override  TextScript #'font-size = #2
%% To scale individual numerals: \markup \override #'(font-size . 2) \rN { ... }
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INPUT FORMATTING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (split-list symbols splitter)
   ;; given (vii o 4 3 / V) --> ((vii o 4 3) (/ V))
   ;; given (vii o 4 3) --> ((vii o 4 3) ())
   (let ((lst '()))
     (define (helper symbols splitter)
         (if (equal? splitter (car symbols))
             (list lst symbols)
             (begin (set! lst (append lst (cons (car symbols) '())))
                    (if (null? (cdr symbols))
                        (list lst '())
                        (helper (cdr symbols) splitter)))))
     (helper symbols splitter)))

#(define (normalize symbols)
   ;; put input into usable format
   (let* ((segmented-list (split-list symbols "/"))
          (up-to-slash
            (append (car segmented-list)
                    (make-list (- 4 (length (car segmented-list))) "")))
          (slash-and-after
            (append (cadr segmented-list)
                    (make-list (- 2 (length (cadr segmented-list))) ""))))
     (if
       (or
         ;; all slots filled in input?
         (= 4 (length (car segmented-list)))
         ;; anything in second position other than certain numbers?
         (not (member (cadr up-to-slash) '("2" "4" "6" "7" "9" "11" "13")))
         ;; placeholder?
         (equal? "" (cadr up-to-slash)))
       ;; leave list alone
       (list up-to-slash slash-and-after)
       ;; otherwise shift inversion to right
       (list
         (list (car up-to-slash) "" (cadr up-to-slash) (caddr up-to-slash))
         slash-and-after))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%% NOTE NAMES / ACCIDENTALS %%%%%%%%%%%%%%%%%%%%%%%%%%
%% Based on English names.  For other languages, change the strings
%% in the three following definitions.

#(define notenames '("A" "a" "B" "b" "C" "c" "D" "d" "E" "e" "F" "f" "G" "g"))

#(define alterations '("f" "ff" "s" "ss" "x" "n"))

#(define (acc size-factor)
  `(("f" . ,(make-raise-markup (* 0.3 size-factor) (make-flat-markup)))
    ("ff" . ,(make-raise-markup (* 0.3 size-factor) (make-doubleflat-markup)))
    ("s" . ,(make-raise-markup (* 0.6 size-factor) (make-sharp-markup)))
    ("ss" . ,(make-raise-markup (* 0.3 size-factor) (make-doublesharp-markup)))
    ("x" . ,(make-raise-markup (* 0.3 size-factor) (make-doublesharp-markup)))
    ("n" . ,(make-raise-markup (* 0.6 size-factor) (make-natural-markup)))))

#(define (initial-accidental-test arg)
   ;; returns an alteration name or #f if none present
   (let ((index (1- (string-length arg))))

     (define (helper arg index)
       ;; find the longest prefix that matches an entry in list of alterations
       (or (find (lambda (x) (string= x (string-take arg index))) alterations)
           (if (> index 1)
               (helper arg (1- index)) #f)))

     (if (or (string-null? arg)
             (find (lambda (x) (string= x arg)) notenames) ; notename w/o accidental?
             (terminal-accidental-test arg)) ; can't have an accidental before and after
         #f
         (helper arg index))))

#(define (terminal-accidental-test arg)
   (let ((index (1- (string-length arg))))

     (define (helper arg index)
       ;; find longest accidental suffix such that prefix is a notename
       (or (and (find (lambda (x) (string= x (string-drop-right arg index))) notenames)
                (find (lambda (x) (string= x (string-take-right arg index))) alterations))
           (if (> index 1)
               (helper arg (1- index)) #f)))

     (if (< (string-length arg) 2)
         #f
         (helper arg index))))

#(define (drop-initial-accidental arg)
   (string-drop arg (string-length (initial-accidental-test arg))))

#(define (drop-end-accidental arg)
   (string-drop-right arg (string-length (terminal-accidental-test arg))))

#(define (big-char? arg) ; offset after awkward characters
   (let ((last-char (string-take-right arg 1)))
     (cond ((string= last-char "V") 0.1)
           ((string= last-char "f") 0.2)
           ((string= last-char "s") 0.2) ; sharp
           ((string= last-char "x") 0.2) ; double-sharp
           ((string= last-char "ss") 0.2) ; double-sharp
           (else 0.0))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BASE MARKUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-base-markup base size)
   (let* ((size-factor (magstep size))
          (init-acc (initial-accidental-test base))
          (end-acc (terminal-accidental-test base)))

       (cond (init-acc
               (make-concat-markup
                 (list (make-fontsize-markup -3 (assoc-ref (acc size-factor) init-acc))
                       (make-hspace-markup (* 0.2 size-factor))
                       (drop-initial-accidental base))))
             (end-acc
               (make-concat-markup
                 (list (drop-end-accidental base)
                       (make-hspace-markup (* size-factor (big-char? (drop-end-accidental base))))
                       (make-hspace-markup (* size-factor 0.2))
                       (make-fontsize-markup -3 (assoc-ref (acc size-factor) end-acc)))))
             (else
               (make-concat-markup
                 (list base
                       (make-hspace-markup (* size-factor
                                              (big-char? base)))))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% QUALITY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (dim size)
   (let* ((size-factor (magstep size))
          (r (* 0.3 size-factor))
          (th (* 0.1 size-factor)))
     (make-translate-markup
       (cons r r)
         (make-draw-circle-markup r th #f))))

#(define (half-dim size)
   (let* ((size-factor (magstep size))
          (x (* size-factor 0.35))
          (y (* size-factor 0.35))
          (r (* size-factor 0.3))
          (th (* size-factor 0.1)))
     (make-translate-markup
        (cons x y)
          (make-combine-markup
            (make-draw-circle-markup r th #f)
              (make-override-markup `(thickness . ,size-factor)
                 (make-combine-markup
                   (make-draw-line-markup (cons (- x) (- y)))
                   (make-draw-line-markup (cons x y))))))))

#(define (aug size)
   (let* ((size-factor (magstep size))
          (x (* size-factor 0.35))
          (y (* size-factor 0.35)))
     (make-override-markup `(thickness . ,size-factor)
       (make-translate-markup (cons x y)
         (make-combine-markup
           (make-combine-markup
             (make-draw-line-markup (cons (- x) 0))
             (make-draw-line-markup (cons 0 (- y))))
           (make-combine-markup
             (make-draw-line-markup (cons x 0))
             (make-draw-line-markup (cons 0 y))))))))

#(define (make-quality-markup size offset quality)
     (cond ((equal? quality "o") (make-raise-markup (* 1.25 offset) (dim size)))
           ((equal? quality "h") (make-raise-markup (* 1.25 offset) (half-dim size)))
           ((equal? quality "+") (make-raise-markup (* 1.25 offset) (aug size)))
           ((equal? quality "f") (make-raise-markup (* 1.5 offset)
                                                    (make-fontsize-markup (- size 5)
                                                                          (make-flat-markup))))
           (else (make-raise-markup offset (make-fontsize-markup -3 quality)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INVERSION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-inversion-markup size offset upper lower)
  (make-fontsize-markup -3
     (make-override-markup `(baseline-skip . ,(* 1.4 (magstep size)))
         (make-raise-markup offset
           (make-column-markup
             (list upper lower))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SECONDARY RN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (make-secondary-markup size second-part)
   (make-concat-markup
     (list
       (car second-part)
       (if (equal? (cadr second-part) "")
           empty-markup
           (make-concat-markup
             (list
               (make-hspace-markup (* 0.2 (magstep size)))
               (if (initial-accidental-test (cadr second-part))
                   (make-hspace-markup (* 0.2 (magstep size)))
                   empty-markup)
               (make-base-markup (cadr second-part) size)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SYNTHESIS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define-markup-command (rN layout props symbols) (markup-list?)
   #:properties ((font-size 1))
   (let* ((normalized (normalize symbols))
          (first-part (car normalized)) ; before slash
          (second-part (cadr normalized)) ; slash and what follows
          (base (car first-part))
          (quality (cadr first-part))
          (upper (caddr first-part)) ; inversion top
          (lower (cadddr first-part)) ; inversion bottom
          (size-factor (magstep font-size))
          ;; height of inversion and quality determined by midline of base
          (dy (* 0.5
                 (interval-length
                   (ly:stencil-extent
                     (interpret-markup layout props (if (equal? "" base)
                                                         "/"
                                                        (make-base-markup base font-size)))
                     Y)))))

     (interpret-markup layout props
       (make-concat-markup
         (list
          (if (equal? base "")
               empty-markup
               (make-concat-markup
                 (list
                   (make-base-markup base font-size)
                   (make-hspace-markup (* size-factor (big-char? base))))))
           (if (equal? quality "")
               empty-markup
               (make-concat-markup
                 (list
                   (make-hspace-markup (* 0.1 size-factor))
                   (make-quality-markup font-size dy quality))))
           (if (equal? upper "")
               empty-markup
               (make-concat-markup
                 (list (make-hspace-markup (* 0.1 size-factor))
                       (make-inversion-markup font-size dy upper lower))))
           (if (equal? (car second-part) "")
               empty-markup
               (make-concat-markup
                 (list
                   (if (equal? lower "")
                       ;; allows slash to tuck under if single inversion figure
                       (make-hspace-markup (* -0.2 size-factor))
                       ;; slightly more space given to slash
                       (make-hspace-markup (* 0.2 size-factor)))
                   (make-secondary-markup font-size second-part)))))))))

\header {
	title = "Analysis by Lisp"
}

SopranoMusic = {
       \clef treble
       \numericTimeSignature
       \time 4/4
       \key ##KEY_LC## \major
       \set Staff.printKeyCancellation = ##f
	   \override Stem #'transparent = ##t
	   \cadenzaOn
##SOPRANO##

      \bar"||"

}


BassMusic =  {
       \clef bass
       \numericTimeSignature
       \time 4/4
       \key ##KEY_LC## \major
       \set Staff.printKeyCancellation = ##f
	   \override Staff.NoteCollision
  	   #'merge-differently-headed = ##t
	   \override Stem #'transparent = ##t
	   \cadenzaOn
##BASS##
       
    	\bar"||"

}

analysis = \lyricmode {
  \set stanza = "##KEY##:  "

##CHORDS##

}

\score {
       <<
               \new PianoStaff <<
               \new Staff <<
               \clef treble
                       \override Score.TimeSignature #'stencil = ##f
                       \new Voice = "Soprano" <<
                       \voiceOne
                       \SopranoMusic
                       >>
                       >>
               \new Staff <<
               \clef bass
                       \new Voice = "Bass" <<
                       \voiceTwo
                       \BassMusic
                       >>
                       \new Lyrics \lyricsto "Bass" \analysis
                       >>
               >>

               \new PianoStaff <<
               \new Staff <<
               \clef treble
                       \override Score.TimeSignature #'stencil = ##f
                       \new Voice = "Soprano" <<
                       \voiceOne
                       \SopranoMusic
                       >>
                       >>
               \new Staff <<
               \clef bass
                       \new Voice = "Bass" <<
                       \voiceTwo
                       \BassMusic
                       >>
                       \new Lyrics \lyricsto "Bass" \analysis
                       >>
               >>

        >>
}

