;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 外观相关
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if window-system
    (progn
      ;; 控制是否显示行号和内容容器直接的间隔
      (set-fringe-style 0)

      ;; 控制 tip 是否在 minibuffer 显示（-1 为显示）
      (tooltip-mode t)

      ;; 没有 toolbar
      (tool-bar-mode -1)

      ;; 光标不闪，不恍花眼睛
      (blink-cursor-mode -1)
      (transient-mark-mode t)

      ;; 没有滚动条 24.1
      (scroll-bar-mode -1)

      ;; 让 Emacs 可以直接打开和显示图片。
      (auto-image-file-mode t)

      ;; 显示时间
      (display-time)

      ;; 时间的格式
      (setq display-time-format "%H:%M @ %m.%d")))

;; 关闭启动时的 “开机画面”
(setq inhibit-startup-message t)

;; 没有 menubar
(menu-bar-mode -1)

;; 括号匹配时显示另外一边的括号，而不是烦人的跳到另一个括号。
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;; 显示列号
(column-number-mode t)

;;语法加亮
(global-font-lock-mode t)

;; 打开就启用 text 模式
(set-default major-mode 'text-mode)

;; 时间的变化频率
(setq display-time-interval 10)

;; 在标题栏显示buffer的名字，而不是 emacs@email.***这样没用的提示。
(setq frame-title-format "Emacs@%b")

;; 一些 24.4 的东西
(unless (version< emacs-version "24.4")
  (progn
    (add-hook 'lisp-mode-hook
              (lambda ()
                ;; 把 lambda 显示成 λ
                (push '("lambda" . ?λ) prettify-symbols-alist)))
    (global-prettify-symbols-mode t)))

(delete-selection-mode -1)

(setq-default indent-tabs-mode nil)

(provide 'face)
