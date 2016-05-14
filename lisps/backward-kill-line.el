;; -*- Emacs-Lisp -*-

(defun backward-kill-line (pos)
  "kill to the beginning of line"
  (interactive "d")
  (beginning-of-line)
  (delete-region pos (point)))
