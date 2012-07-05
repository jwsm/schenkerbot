(defconstant *root-dist-in-key* '(1 0 0 0 0 0.2 0 1 0 0 0 0)) ; I, IV, V most common roots


(defun find-key-by-root-dist (root-dist &optional (current-root 0))
  "Rotate the 5 - 1 - 4 root distribution template to find the best match for a key."
  (if (equal current-root *NUM-PITCH-CLASSES*) ()
    (progn
      (format t "list for ~a ~a ~%" current-root (fz-intersection root-dist (ror-n *root-dist-in-key* current-root)))
      (cons (apply #'+ (fz-intersection root-dist (ror-n *root-dist-in-key* current-root)))
            (find-key-by-root-dist root-dist (+ 1 current-root))))))
    


(defun count-chord-roots (roots)
  "Count the chord roots from a list of (onset (root chord-type)),"
  "showing the sums in a set by pitch class."
  (cond ((null roots) *FZ-EMPTY*)
        (T (mapcar #'+ (ror-n *FZ-NOTE* (first (second (first roots))))
            (count-chord-roots (rest roots))))))


;(find-key-by-root-dist (fz-normalize-list (count-chord-roots *roots-list*)))