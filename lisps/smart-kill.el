;; -*- Emacs-Lisp -*-

;; From https://github.com/4DA/emacs-stuff/blob/20a5180eda54c76b7da3ecb1b0a640ea6c04c682/smart-kill.el
(defun backward-kill-word-or-delim (arg &optional killp)
  "Deletes a word or delimiter/punctuation symbol backwards"
  (interactive "*p\nP")
  (push-mark (point))
  (if (= 0 (skip-syntax-backward "w"))
      (if (= 0 (skip-chars-backward "\n"))
          (backward-delete-char-untabify arg killp)
        (insert-char ?\n)))
  (kill-region (point) (mark))
  (pop-mark))

(defun forward-kill-word-or-delim (arg &optional killflag)
  "Deletes a word or delimiter/punctuation symbol forward"
  (interactive "p\nP")
  (push-mark (point))
  (if (= 0 (skip-syntax-forward "w"))
      (delete-forward-char arg killflag))
  (kill-region (point) (mark))
  (pop-mark))
