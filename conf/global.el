;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 系统设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 把 /user/loacl/bin 放入执行目录
(push "/usr/local/bin" exec-path)

;; change yes or no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; 设置缩进
(setq c-basic-offset 4)
(setq-default tab-width 2)

;; 使用系统剪切版
(setq x-select-enable-clipboard t)

;; 用户信息
(setq user-full-name "c4605")
(setq user-mail-address "bolasblack [at] gmail")

;;把这些缺省禁用的功能打开。
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)

;;关闭烦人的出错时的提示声。
(setq visible-bell t)

;;用一个很大的 kill ring. 这样防止不小心删掉重要的东西
(setq kill-ring-max 200)

;;把 fill-column 设为 60. 这样的文字更好读。
(setq default-fill-column 80)

;; 如果设置为 t，光标在 TAB 字符上会显示为一个大方块 :)。
(setq x-stretch-cursor t)

;;不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile
;;的时候也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的
;;TAB 字符，并且加亮显示的。
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
;(setq tab-stop-list ())
;(loop for x downfrom 40 to 1 do
;      (setq tab-stop-list (cons (* x 4) tab-stop-list)))

;;设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

;;set the default file path
(setq default-directory "~/")

;;写文件的编码
(set-buffer-file-coding-system 'utf-8)

;;新建文件的编码
(setq default-buffer-file-coding-system 'utf-8)

;;读取或写入文件名的编码方式
(setq file-name-coding-system 'utf-8)

;;终端方式的编码方式
(set-terminal-coding-system 'utf-8)

;;键盘输入的编码方式
(set-keyboard-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq ansi-color-for-comint-mode t)

