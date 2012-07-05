(defConstant *FZ-EMPTY* '(0 0 0 0 0 0 0 0 0 0 0 0))

(defVar *NUM-PITCH-CLASSES*)
(setq *NUM-PITCH-CLASSES* 12)

(defun bounded-+ (a b)
  (min (+ a b) 1))