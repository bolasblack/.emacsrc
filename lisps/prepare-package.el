(defvar package-archives-mapping
  '(("gnu"          . "http://elpa.gnu.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/")
    ("marmalade"    . "http://marmalade-repo.org/packages/")
    ("SC"           . "http://joseito.republika.pl/sunrise-commander/")
    ("org"          . "http://orgmode.org/elpa/"))
  "Mapping of package archives name and url.")

(defmacro add-package-archives (source-name)
  `(let* ((source-name (symbol-name ',source-name))
          (mapping (assoc source-name package-archives-mapping)))
     (unless mapping
       (error "Unknown package archive: %s" source-name))
     (unless (member mapping package-archives)
       (message "package refresh")
       (setq package-contents-refreshed nil))
     (add-to-list 'package-archives mapping)))

(require 'package)
(add-package-archives gnu)
(add-package-archives melpa)
(package-initialize)

(provide 'prepare-package)
