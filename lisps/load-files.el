;; -*- Emacs-Lisp -*-

(defun load-files (&rest files-lists)
  (dolist (files-list files-lists)
    (let ((root-path (symbol-value (car files-list)))
          (names-list (cdr files-list)))
      (add-to-list 'load-path root-path)
      (dolist (name names-list)
        (load (concat (symbol-name name) ".el"))))))
