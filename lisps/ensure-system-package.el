;; inspired by github:waymondo/use-package-ensure-system-package

(require 'system-packages)

(provide 'ensure-system-package)

(defun ensure-system-package--install-command (pack)
  "Return the default install command for `pack'."
  (let ((command
         (cdr (assoc 'install (cdr (assoc system-packages-packagemanager
                                          system-packages-supported-package-managers))))))
    (unless command
      (error (format "%S not supported in %S" 'install system-packages-packagemanager)))
    (unless (listp command)
      (setq command (list command)))
    (when system-packages-usesudo
      (setq command (mapcar (lambda (part) (concat "sudo " part)) command)))
    (setq command (mapconcat 'identity command " && "))
    (mapconcat 'identity (list command pack) " ")))

(defun ensure-system-package--consify (arg)
  "Turn `arg' into a cons of (`package-name' . `install-command')."
  (cond
   ((stringp arg)
    (cons arg (ensure-system-package--install-command arg)))
   ((symbolp arg)
    (cons arg (ensure-system-package--install-command (symbol-name arg))))
   ((consp arg) arg)))

(defmacro ensure-system-package (&rest pkgs)
  "Ensure some packages installed in system

Usage:

(ensure-system-package rg bat)
(ensure-system-package
  (rubocop     . \"gem install rubocop\")
  (ruby-lint   . \"gem install ruby-lint\")
  (ripper-tags . \"gem install ripper-tags\")
  (pry         . \"gem install pry\"))"
  (let ((pkginfos (mapcar #'ensure-system-package--consify pkgs)))
    (cons
     'progn
     (mapcar
      #'(lambda (cons)
          `(unless (executable-find (symbol-name ',(car cons)))
             (async-shell-command ,(cdr cons))))
      pkginfos))))

(macroexpand-1
 '(ensure-system-package rg bat))
(macroexpand-1
 '(ensure-system-package
   (rubocop     . "gem install rubocop")
   (ruby-lint   . "gem install ruby-lint")
   (ripper-tags . "gem install ripper-tags")
   (pry         . "gem install pry")))
