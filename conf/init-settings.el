;; -*- Emacs-Lisp -*-

(provide-me)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 系统设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-directory (expand-file-name "~/"))

;; 避免使用过期的 elc 文件
(setq load-prefer-newer t)

;; mini window 的最小高度，百分比
(setq max-mini-window-height 0.75)

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

;; change yes or no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 设置缩进
;; 不用 TAB 字符来 indent ，编辑 Makefile 的时候不用担心，因为
;; makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的。
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
(setq-default fill-column 80)

;; 设置 sentence-end 可以识别中文标点
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; 不在 fill 时在句号后插入两个空格
(setq sentence-end-double-space nil)

;; 如果设置为 t，光标在 TAB 字符上会显示为一个大方块 :)。
;; (setq x-stretch-cursor t)

;; (setq ansi-color-for-comint-mode t)

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

;; 关闭启动时的 “开机画面”
(setq inhibit-startup-message t)

;; 没有 menubar
(menu-bar-mode -1)

;; 高亮匹配的括号
(show-paren-mode t)

;; 显示列号
(column-number-mode t)

;;语法加亮
(global-font-lock-mode t)

;; 打开就启用 text 模式
(set-default major-mode 'text-mode)

;; 在标题栏显示buffer的名字，而不是 emacs@email.***这样没用的提示。
(setq frame-title-format "Emacs@%b")

(delete-selection-mode -1)

(when window-system
  ;; 控制是否显示行号和内容容器直接的间隔
  (set-fringe-style 0)

  ;; 控制 tip 是否在 minibuffer 显示（-1 为显示）
  (tooltip-mode t)

  ;; 没有 toolbar
  (tool-bar-mode -1)

  ;; 光标不闪，不恍花眼睛
  (blink-cursor-mode -1)
  (transient-mark-mode t)

  ;; 没有滚动条
  (scroll-bar-mode -1)

  ;; 让 Emacs 可以直接打开和显示图片。
  (auto-image-file-mode t))
