(require 'init-use-package)

(provide-me)

(use-package org
  :hook
  ((org-mode . c4:org-mode-hook))
  :config
  (defun c4:outline-show-children-and-entry ()
    (interactive)
    (outline-show-children)
    (outline-show-entry))
  (defun c4:org-mode-hook ()
    (bind-key "zo" 'c4:outline-show-children-and-entry evil-normal-state-local-map)
    (bind-key "zO" 'outline-show-subtree evil-normal-state-local-map)
    (bind-key "zc" 'outline-hide-subtree evil-normal-state-local-map)
    (bind-key "ze" 'outline-show-entry evil-normal-state-local-map)
    (bind-key "zE" 'outline-hide-entry evil-normal-state-local-map)))
