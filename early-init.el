;;; -*- lexical-binding: t; -*-
;; 此文件需要被 ~/.emacs.d/early-init.el 加载:
;;   (load "~/.emacsrc/early-init.el")

;; --- GC: 启动期间禁用 ---
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 1.0)

;; --- file-name-handler-alist: 启动期间禁用以加速文件加载 ---
(defvar my--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; --- 禁止 package.el 初始化（straight.el 接管） ---
(setq package-enable-at-startup nil)

;; --- 禁用 site-run-file 和 default.el ---
(setq site-run-file nil)
(setq inhibit-default-init t)

;; --- UI: 在 frame 创建之前禁用，避免昂贵的重绘 ---
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; --- Frame 优化 ---
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)

;; --- 启动画面 ---
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq inhibit-startup-buffer-menu t)

;; --- 跳过 X resources 解析 ---
(setq inhibit-x-resources t)

;; --- 禁用双向文本渲染以提升性能 ---
(setq-default bidi-display-reordering 'left-to-right)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; --- 跳过 auto-mode-alist 的第二轮大小写不敏感匹配 ---
(setq auto-mode-case-fold nil)

;; --- 不 ping 看起来像域名的东西 ---
(setq ffap-machine-p-known 'reject)

;; --- 优先加载较新的文件 ---
(setq load-prefer-newer t)

;; --- 增大进程输出缓冲区（对 LSP 等有帮助） ---
(setq read-process-output-max (* 2 1024 1024))
(setq process-adaptive-read-buffering nil)

;; --- Native compilation 设置 ---
(when (and (featurep 'native-compile)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-warning-on-missing-source nil)
  (setq native-comp-async-jobs-number
        (max 1 (- (num-processors) 2)))
  (setq package-native-compile t))

;; --- 启动完成后恢复 GC 和 file-name-handler-alist ---
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024))
            (setq gc-cons-percentage 0.1)
            (setq file-name-handler-alist
                  (delete-dups
                   (append file-name-handler-alist
                           my--file-name-handler-alist))))
          101)
