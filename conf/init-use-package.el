;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'comment)
(require 'init-straight)

(provide-me)

(setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-inject-hooks t)

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(defvar c4:use/lazy-load t
  "Indicate `c4:use' to keep or ignore `use-package' defer related options")

(defun c4:use/filter-defer-opts (args)
  (let* ((defer-idx (cl-position :defer args))
         (rest-args (when defer-idx
                      (nthcdr (+ 2 defer-idx) args))))
    (cond
     ((not defer-idx) (append '(:demand t) args))
     ((zerop defer-idx) (append '(:demand t) rest-args))
     (t (append (seq-subseq args 0 defer-idx) '(:demand t) rest-args)))))

(defmacro c4:use (name &rest args)
  (declare (indent 1))
  (if c4:use/lazy-load
      `(use-package ,name ,@args)
    `(use-package ,name ,@(c4:use/filter-defer-opts args))))
(put 'c4:use 'lisp-indent-function 'defun)

(comment
 (macroexpand-1
  '(c4:use delight
     :defer .1
     :straight t))
 (macroexpand-1
  '(c4:use delight
     :straight t
     :defer .1))
 (macroexpand-1
  '(c4:use delight
     :straight t)))

(c4:use use-package-evil-bind)

(c4:use delight
  :straight t
  :defer t)

(c4:use use-package-ensure-system-package
  :straight t
  :defer t)

(c4:use auto-minor-mode
  :straight t
  :defer t)
