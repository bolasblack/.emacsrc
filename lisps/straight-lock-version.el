;;; -*- lexical-binding: t -*-

(require 'straight)
(require 'f)

(defun lock-plugin-versions (&rest _)
  (interactive "P")
  (straight-freeze-versions t)
  (let ((filepath (f-join dir-rc "versions.el")))
    (and (f-exists? filepath)
         (f-delete filepath))
    (f-copy (straight--versions-file "default.el")
            filepath)
    (message "Wrote %s" filepath)))

(provide 'straight-lock-version)
