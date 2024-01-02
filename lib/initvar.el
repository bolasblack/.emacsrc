(provide 'initvar)

(defun initvar (symbol &optional default-val)
  (unless (boundp symbol)
    (eval `(setq ,symbol nil))))
