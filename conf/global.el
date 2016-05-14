;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 系统设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable lock-files
(setq create-lockfiles nil)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
;; From http://snarfed.org/gnu_emacs_backup_files
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; From: http://www.emacswiki.org/emacs/BackupDirectory
(setq version-control t) ; use versioned backups
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq backup-by-copying t) ; don't clobber symlinks

;; M-f/b 的时候 驼峰、连字符、下划线 都会被视为单词的分隔边界
(global-subword-mode t)

;; set the default file path
(setq default-directory "~/")

;; change yes or no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 设置缩进
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil)

;; 用户信息
(setq user-full-name "c4605")
(setq user-mail-address "bolasblack [at] gmail")

;; 把这些缺省禁用的功能打开。
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

;; 关闭烦人的出错时的提示声。
(setq visible-bell t)

;; 用一个很大的 kill ring. 这样防止不小心删掉重要的东西
(setq kill-ring-max 200)

;; 把 fill-column 设为 60. 这样的文字更好读。
(setq default-fill-column 80)

;; 如果设置为 t，光标在 TAB 字符上会显示为一个大方块 :)。
(setq x-stretch-cursor t)

;; 不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile
;; 的时候也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的
;; TAB 字符，并且加亮显示的。
(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)
;(setq tab-stop-list ())
;(loop for x downfrom 40 to 1 do
;      (setq tab-stop-list (cons (* x 4) tab-stop-list)))

;; 保存前移除行末空格
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 设置 sentence-end 可以识别中文标点
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; 不在 fill 时在句号后插入两个空格
(setq sentence-end-double-space nil)

(setq ansi-color-for-comint-mode t)

;; 默认倾向的编码
(prefer-coding-system 'utf-8)
;; 设置默认的文件名、终端、键盘的编码
(set-default-coding-systems 'utf-8)
;; 剪贴板的编码
(set-clipboard-coding-system 'utf-8)
;; 和其他 X 程序通信时的编码
(set-selection-coding-system 'utf-8)
;; 写文件的编码
(setq-default buffer-file-coding-system 'utf-8)
;; 标准输入输出时的编码
(setq-default default-process-coding-system '(utf-8 . utf-8))
;; 设置进程的 I/O 操作的编码
(modify-coding-system-alist 'process "*" 'utf-8)
