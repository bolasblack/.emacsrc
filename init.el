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

(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)

  (require 'benchmark-init)
  (add-hook 'after-init-hook 'benchmark-init/deactivate)

  (require 'init-straight)
  (require 'init-load-relative)
  (require 'init-theme-load-hook)
  (require 'init-use-package)
  (require 'init-deps)
  (require 'init-keyboard)
  (require 'init-settings)
  (require 'init-emacs-server))
