(require 'init-use-package)

(c4:use org
  :hook
  ((org-mode . c4:org-mode-hook))
  :config
  (defun c4:org-show-children-and-entry ()
    (interactive)
    (org-show-children)
    (org-show-entry))
  (defun c4:org-mode-hook ()
    (bind-key "zo" 'c4:org-show-children-and-entry evil-normal-state-local-map)
    (bind-key "zO" 'org-show-subtree evil-normal-state-local-map)
    (bind-key "zc" 'outline-hide-subtree evil-normal-state-local-map)
    (bind-key "ze" 'org-show-entry evil-normal-state-local-map)
    (bind-key "zE" 'outline-hide-entry evil-normal-state-local-map)))

(c4:use poly-org
  :straight t
  :after (polymode org)
  :mode
  ("\\.org\\'" . poly-org-mode))
