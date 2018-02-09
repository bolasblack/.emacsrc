;;; -*- lexical-binding: t -*-

(defmacro comment (&rest _))

(eval-and-compile
  ;; .emacs.d 文件地址
  (defconst dir-rc (expand-file-name "~/.emacsrc/"))
  ;; Emacs 配置文件地址
  (defconst dir-conf (concat dir-rc "conf/"))
  ;; 自己写的 Lisp 脚本文件位置
  (defconst dir-lisp (concat dir-rc "lisps/"))
  ;; Snippet 文件地址
  (defconst dir-snippet (concat dir-rc "snippets/"))

  (mapc #'(lambda (path)
            (add-to-list 'load-path path))
        (list dir-rc dir-conf dir-lisp)))

(defvar use-package-verbose t)
(defvar use-package-inject-hooks t)
(require 'prepare-package)
(require 'cl-lib)

;; https://github.com/jwiegley/use-package
;; https://github.com/magnars/dash.el
;; https://github.com/magnars/s.el
;; https://github.com/rejeep/f.el
(let* ((core-packages '(use-package bind-key dash s f))
       (uninstalled-packages (cl-remove-if #'package-installed-p core-packages)))
  (when uninstalled-packages
    (package-refresh-contents)
    (dolist (p uninstalled-packages)
      (package-install p)))
  (dolist (p core-packages)
    (require p)))

(dolist (conf-file '(global plugin keyboard mode-setting face))
  (require conf-file))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
