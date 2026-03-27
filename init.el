;;; -*- lexical-binding: t; -*-

;; set the default file path
(defconst dir-rc (file-name-directory (or load-file-name "")))
;; 自己写的 Lisp 库文件位置
(defconst dir-lib (concat dir-rc "lib/"))
;; Emacs 配置文件地址
(defconst dir-conf (concat dir-rc "conf/"))
;; Snippet 文件地址
(defconst dir-snippet (concat dir-rc "snippets/"))
;; flycheck 配置文件地址
(defconst dir-flycheck (concat dir-rc "flycheck.conf/"))

(add-to-list 'load-path dir-rc)
(add-to-list 'load-path dir-lib)
(add-to-list 'load-path dir-conf)

;; GC 和 file-name-handler-alist 的优化已移至 early-init.el

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(when (featurep 'benchmark-init)
  (require 'benchmark-init)
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(require 'dump-utils)
(dump/unless-dumping
  (dump/eval-delayed-functions))

(require 'init-straight)
(require 'init-load-relative)
(require 'init-theme-load-hook)
(require 'init-use-package)
(require 'init-baselib)
(require 'init-deps)
(require 'init-keyboard)
(require 'init-settings)
(dump/unless-dumping
  (require 'init-emacs-server))
