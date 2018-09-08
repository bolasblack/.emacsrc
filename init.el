;;; -*- lexical-binding: t -*-

(eval-and-compile
  ;; set the default file path
  (setq default-directory (expand-file-name "~/"))
  ;; .emacs.d 文件地址
  (defconst dir-rc (concat default-directory ".emacsrc/"))
  ;; Emacs 配置文件地址
  (defconst dir-conf (concat dir-rc "conf/"))
  ;; 自己写的 Lisp 脚本文件位置
  (defconst dir-lisp (concat dir-rc "lisps/"))
  ;; Snippet 文件地址
  (defconst dir-snippet (concat dir-rc "snippets/"))
  ;; 一些其他脚本文件位置
  (defconst dir-scripts (concat dir-rc "scripts/"))

  (mapc #'(lambda (path)
            (add-to-list 'load-path path))
        (list dir-rc dir-conf dir-lisp)))

(require 'prepare-use-package)
(require 'init-theme-load-hook)

(require 'threads)
;; Load all config files
(->> (f-files dir-conf)
     (-map #'f-base)
     (-reject (lambda (filename) (s-starts-with? "." filename)))
     (-map (lambda (module-name)
             (require (intern module-name)))))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
