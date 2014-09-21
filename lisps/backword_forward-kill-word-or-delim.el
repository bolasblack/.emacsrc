;; -*- Emacs-Lisp -*-

;; From https://github.com/4DA/emacs-stuff/blob/20a5180eda54c76b7da3ecb1b0a640ea6c04c682/smart-kill.el
(defun backward-kill-word-or-delim ()
  "Deletes a word or delimiter/punctuation symbol backwards"
  (interactive)
  (push-mark (point))
  (skip-syntax-backward "-_>")
  (when (= 0 (skip-syntax-backward "w\""))
    (skip-syntax-backward "_.'()(])[$<>"))
  (kill-region (point) (mark))
  (pop-mark))

(defun forward-kill-word-or-delim ()
  "Deletes a word or delimiter/punctuation symbol forward"
  (interactive)
  (push-mark (point))
  (skip-syntax-forward "-_>")
  (when (= 0 (skip-syntax-forward "w\""))
    (skip-syntax-forward "_.'()(])[$<>"))
  (kill-region (point) (mark))
  (pop-mark))
