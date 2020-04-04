;; -*- Emacs-Lisp -*-

(provide 'smart-delete)

;; From https://github.com/4DA/emacs-stuff/blob/3cf35ab4c1facd565d07d2c559da7f6b0c16a60b/smart-kill.el

(defun backward-delete-word-or-delim ()
  "Deletes a word or delimiter/punctuation symbol backwards"
  (interactive)
  (push-mark (point))
  (skip-syntax-backward "-_>")
  (when (= 0 (skip-syntax-backward "w\""))
    (skip-syntax-backward "_.'()(])[$<>"))
  (kill-region (point) (mark))
  (pop-mark))

(defun forward-delete-word-or-delim ()
  "Deletes a word or delimiter/punctuation symbol forward"
  (push-mark (point))
  (skip-syntax-forward "-_>")
  (when (= 0 (skip-syntax-forward "w\""))
    (skip-syntax-forward "_.'()(])[$<>"))
  (kill-region (point) (mark))
  (pop-mark))
