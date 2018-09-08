(eval-and-compile
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

  (straight-use-package 'f)
  (straight-use-package 'use-package))

(defun straight-lock-plugin-versions (&rest _)
  (interactive "P")
  (straight-freeze-versions t)
  (let ((filepath (f-join dir-rc "versions.el")))
    (and (f-exists? filepath)
         (f-delete filepath))
    (f-copy (straight--versions-file "default.el")
            filepath)
    (message "Wrote %s" filepath)))

(defvar use-package-verbose t)
(defvar use-package-inject-hooks t)

(use-package system-packages
  :straight t
  :defer)

(use-package use-package-ensure-system-package
  :straight t
  :defer)

(use-package bind-key
  :straight t
  :defer)

(use-package delight
  :straight t
  :defer)

(use-package dash
  :straight t)

(use-package s
  :straight t)

(use-package f
  :straight t)

(provide 'prepare-use-package)
