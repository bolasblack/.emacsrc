;;; -*- mode: emacs-lisp-mode; lexical-binding: t -*-

(provide 'osx-clipboard)

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

(defun osx-clipboard-kill-region ()
  "Cut region and put on OS X system pasteboard."
  (interactive)
  (osx-clipboard-kill-ring-save)
  (delete-region (region-beginning) (region-end)))

;; 解决 evil-mode 在终端下无法操作系统剪贴板的问题
;; https://github.com/jixiuf/vmacs/blob/5bca035dd7ed424b2be3a5d79c548ff97c935b67/conf/conf-evil-clipboard.el

;; "+p 从系统剪切板 paste 时会调到此处
;; 如果在 mac 终端下使用 emacs , 则使用 pbpaste 从 clipboard 获取内容
(defadvice gui-backend-get-selection (around get-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((default-directory "~/"))
      (setq ad-return-value (shell-command-to-string "pbpaste")))))

;; "+y 设置内容到系统 clipboard
;; 如果在 mac 终端下使用 emacs , 则使用 pbpaste 从 clipboard 获取内容
(defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((process-connection-type nil)   ; ; use pipe
          (default-directory "~/"))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (ad-get-arg 1))
        (process-send-eof proc)))))
