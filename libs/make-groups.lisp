(defun group-events-by-beat (cope-events)
  "group note events that start within the same beat into lists"
  (if (null cope-events) ()
    (let* ((recursive-result (group-events-by-beat (rest cope-events)))
           (last-result (first recursive-result))
           (last-event (first cope-events)))
      (cond
       ((null last-result)
        (cons (list (first cope-events)) recursive-result))

       ((






#|
group-by-simultaneous-start

group note events that start at the same time into lists. Note events that occur
on their own are put into their own list. The notes must be provided in order for
this function to work.

(group-by-simultaneous-start *events*)
|#
(defun group-events-by-simultaneous-start (cope-events)
  "group note events that start at the same time into lists"
  (if (null cope-events) ()
    ;; recursive-result: (list) the recursive result for the rest of the list
    ;; last-result: (list) the most recent tuple on the result list
    ;; last-value: (atom) the next value to be processed from input list
    (let* ((recursive-result (group-events-by-simultaneous-start (rest cope-events)))
           (last-result (first recursive-result))
           (last-note (first cope-events)))
      ;; check 3 possible conditions:
      (cond
            ;; last-result is nil (at end of list and need to add first tuple)
            ((null last-result)
             (cons (list (first cope-events)) recursive-result))
            ;; last-result matches last-value (add last value to adjacent tuple)
            ((= (first (first last-result)) (first last-note))
             (cons (cons last-note last-result) (rest recursive-result)))
            ;; otherwise, add a new tuple containing our non-matching number
            (T (cons (list last-note) recursive-result))))))