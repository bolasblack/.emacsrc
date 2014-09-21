;; -*- Emacs-Lisp -*-

(defun match-paren (arg)
  "jump to matching paren"
  (interactive "p")
  (cond ((or (backward-char) (looking-at "[([{]") (forward-char))
         (forward-sexp))
        ((or (backward-char) (looking-at "[)]}]") (forward-char))
         (forward-char)
         (backward-sexp)
         (forward-char))
        (t (self-insert-command (or arg 1)))))
