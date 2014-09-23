;; -*- Emacs-Lisp -*-

;; From https://github.com/4DA/emacs-stuff/blob/20a5180eda54c76b7da3ecb1b0a640ea6c04c682/smart-kill.el
(defun backward-kill-word-or-delim (arg &optional killp)
  "Deletes a word or delimiter/punctuation symbol backwards"
  (interactive "*p\nP")
  (push-mark (point))
  (if (= 0 (skip-syntax-backward "w"))
      (block nil
        ;; 选中多个连续的空行
        (let ((backword-break-line-count (skip-chars-backward "\n")))
          (unless (= 0 backword-break-line-count)
            ;; 如果选中的空行不只一个的话，那么就需要"留下"最后一个空行
            (unless (= -1 backword-break-line-count) (insert-char ?\n))
            (return)))
        ;; 选中多个连续的空白，比如空格，制表符
        (unless (= 0 (skip-syntax-backward "-"))
          (return))
        (backward-delete-char-untabify arg killp)))
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
