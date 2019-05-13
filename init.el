;;; -*- lexical-binding: t -*-

(eval-and-compile
  (defconst dir-init (file-name-directory (or load-file-name "")))
  ;; set the default file path
  (defconst dir-rc dir-init)
  ;; Emacs 配置文件地址
  (defconst dir-conf (concat dir-rc "conf/"))
  ;; 自己写的 Lisp 库文件位置
  (defconst dir-lisp (concat dir-rc "lisps/"))
  ;; Snippet 文件地址
  (defconst dir-snippet (concat dir-rc "snippets/"))
  ;; flycheck 配置文件地址
  (defconst dir-flycheck (concat dir-rc "flycheck.conf/"))

  (add-to-list 'load-path dir-rc)
  (add-to-list 'load-path dir-conf)
  (add-to-list 'load-path dir-lisp))

(require 'init-straight)
(eval-when-compile
  (require 'init-load-relative))
(require 'init-theme-load-hook)
(require 'init-use-package)
(require 'init-deps)
(require 'init-keyboard)
(require 'init-settings)
(require 'init-emacs-server)
