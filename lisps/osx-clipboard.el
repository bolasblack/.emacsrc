;; -*- Emacs-Lisp -*-

;; From http://stackoverflow.com/questions/22849281/on-emacs-for-osx-how-to-keep-kill-ring-and-clipboard-separate#answer-24249229
(defun osx-clipboard-kill-ring-save ()
  "Copy region to OS X system pasteboard."
  (interactive)
  (shell-command-on-region
   (region-beginning) (region-end) "pbcopy"))

(defun osx-clipboard-yank ()
  "Paste from OS X system pasteboard via `pbpaste' to point."
  (interactive)
  (shell-command-on-region
   (point) (if mark-active (mark) (point)) "pbpaste" nil t))

(defun osx-clipboard-cut ()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (pasteboard-copy)
  (delete-region (region-beginning) (region-end)))
