;; -*- Emacs-Lisp -*-

(defun open-line (n)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (newline n)
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)))

(defun copy-lines (&optional arg)
  "copy the whole line"
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (set-mark (point))
    (if arg
        (next-line (- arg 1)))
    (end-of-line)
    (kill-ring-save (mark) (point))))

(defun concat-lines (&optional n)
  "Concat next line with current line, just like J in Vim"
  (interactive "*p")
  (dotimes (time n)
    (next-line)
    (back-to-indentation)
    (backward-kill-line (point))
    (delete-backward-char 1)
    (insert " ")))

(defun backward-kill-line (pos)
  "kill to the beginning of line"
  (interactive "d")
  (beginning-of-line)
  (delete-region pos (point)))

(defun open-next-line ()
  "Insert a newline"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-previous-line ()
  "Insert a newline"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))
