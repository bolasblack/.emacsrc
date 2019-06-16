(require 'dash)

(provide-me)

(defun activated-minor-modes ()
  "Get all current buffer activated minor modes"
  (--filter (and (boundp it)
                 (symbol-value it))
            minor-mode-list))
