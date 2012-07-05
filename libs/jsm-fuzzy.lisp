(defConstant *FZ-EMPTY* '(0 0 0 0 0 0 0 0 0 0 0 0))

(defun bounded-+ (a b)
  (min (+ a b) 1))