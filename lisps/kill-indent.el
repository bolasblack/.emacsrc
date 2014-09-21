;; -*- Emacs-Lisp -*-

(defun kill-indent (old-pos)
  "kill indent of current line"
  (interactive "d")
  (back-to-indentation)
  (let* ((pos (point))
         (delta-pos (- old-pos pos)))
    (kill-to-beginning pos)
    (forward-char delta-pos)))
