(defun count-best-chord-roots (tree)
  "Count all of the best chord roots, showing the sums in a set by pitch class."
  (cond ((null tree) *FZ-EMPTY*)
        ((null (best-chord (first tree))) *FZ-EMPTY*)
        (T (mapcar #'+ (ror-n *FZ-NOTE* (second (best-chord (first tree))))
            (count-best-chord-roots (rest tree))))))


(defun count-all-pitches (cope-events)
  "Count all the pitches in the piece, showing the sums in a set by pitch class."
  (if (null cope-events) *FZ-EMPTY*
    (mapcar #'+ (pitch-set-for-cope-event (first cope-events))
            (count-all-pitches (rest cope-events)))))


(defun count-cope-events-within-scale (cope-events scale)
  "Count the cope events that fall within a given diatonic scale."
  (if (null cope-events) 0
    (+ 
     (apply #'+ (fz-intersection (pitch-set-for-cope-event (first cope-events)) scale))
     (count-cope-events-within-scale (rest cope-events) scale))))

(defun count-cope-events-within-all-scales-of-type (cope-events scale &optional (scale-count 0))
  "Count the cope events that fall in each diatonic scale of type scale, transposed to start on each pc."
  (if (equal scale-count *NUM-PITCH-CLASSES*) ()
    (cons (count-cope-events-within-scale cope-events (ror-n scale scale-count))
          (count-cope-events-within-all-scales-of-type cope-events scale (+ 1 scale-count)))))

(defun find-key-of-piece (cope-events)
  "Find the key for which the most cope-events are in the scale of that key."
  (first (top-n-positions (count-cope-events-within-all-scales-of-type cope-events *MAJ-SCALE*) 1)))