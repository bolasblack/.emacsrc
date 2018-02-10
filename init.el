;;; -*- lexical-binding: t -*-

(defvar use-package-verbose t)
(defvar use-package-inject-hooks t)

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
        (list dir-rc dir-conf dir-lisp))

  ;; https://github.com/raxod502/straight.el#integration-with-use-package-1
  ;; https://github.com/raxod502/straight.el#the-recipe-format
  (let ((bootstrap-file (concat user-emacs-directory "straight/repos/straight.el/bootstrap.el")))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  ;; https://github.com/jwiegley/use-package
  ;; https://github.com/magnars/dash.el
  ;; https://github.com/magnars/s.el
  ;; https://github.com/rejeep/f.el
  (let* ((core-packages '(use-package dash s f)))
    (dolist (p core-packages)
      (straight-use-package p)
      (require p)))

  ;; Load all config files
  (require 'threads)
  (->> (f-files dir-conf)
       (-map #'f-base)
       (-reject (lambda (filename) (s-starts-with? "." filename)))
       (-map (lambda (module-name)
               (require (intern module-name))))))

(require 'straight-lock-version)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
