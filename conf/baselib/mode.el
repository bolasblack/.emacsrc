(provide-me)

(defun user/when-mode-enabled (mode-sym fn)
  (when (and (boundp mode-sym)
             (symbol-value mode-sym))
    (funcall fn)))
