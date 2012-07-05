
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-piece-info ()
  (setq *first-bar* (parse-integer (prompt-read "First Bar")))
  (setq *bar-length* (parse-integer (prompt-read "Bar Length")))
  (setq *beats-per-bar* (parse-integer (prompt-read "Beats per Bar"))))

