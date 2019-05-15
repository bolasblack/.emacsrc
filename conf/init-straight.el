;;; -*- lexical-binding: t -*-

(provide 'init-straight)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun straight-lock-plugin-versions (&rest _)
  (interactive "P")
  (straight-freeze-versions t)
  (let ((filepath (concat dir-rc "versions.el")))
    (and (file-exists-p filepath)
         (delete-file filepath))
    (copy-file (straight--versions-file "default.el")
               filepath)
    (message "Wrote %s" filepath)))
