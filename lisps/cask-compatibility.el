;; -*- Emacs-Lisp -*-

(defvar cask-source-mapping
  '(("gnu"          . "http://elpa.gnu.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("marmalade"    . "http://marmalade-repo.org/packages/")
    ("SC"           . "http://joseito.republika.pl/sunrise-commander/")
    ("org"          . "http://orgmode.org/elpa/"))
  "Mapping of source name and url.")

(defvar package-contents-refreshed
  nil
  "Be t if package refreshed")

(package-initialize)

(defmacro source (source-name)
  `(let* ((source-name (symbol-name ',source-name))
          (mapping (assoc source-name cask-source-mapping)))
     (unless mapping
       (error "Unknown package archive: %s" source-name))
     (unless (member mapping package-archives)
       (message "package refresh")
       (setq package-contents-refreshed nil))
     (add-to-list 'package-archives mapping)))

(defmacro unsource (source-name)
  `(let* ((source-name (symbol-name ',source-name))
          (mapping (assoc source-name cask-source-mapping)))
     (setq package-archives (delete mapping package-archives))))

(defun depends-on (package-name)
  (unless package-contents-refreshed
    (package-refresh-contents)
    (setq package-contents-refreshed t))
  (let ((package-name-symbol (intern package-name)))
    (unless (package-installed-p package-name-symbol)
      (package-install package-name-symbol))))

;; (source melpa)
;; (unsource melpa)
;; (setq package-contents-refreshed t)
