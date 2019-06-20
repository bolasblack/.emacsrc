(require 'init-use-package)
(require 'dash)

(provide-me)

(use-package lsp-mode
  :straight t
  :commands (lsp)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-auto-configure nil))

(use-package lsp-ui
  :straight t
  :commands (lsp-ui-mode)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :straight t
  :commands (company-lsp)
  :hook
  (company-mode . c4:company-lsp-company-mode-hook)
  (lsp-mode . c4:company-lsp-company-mode-hook)
  :init
  (defun c4:company-lsp-company-mode-hook ()
    (when (and (boundp 'lsp-mode) lsp-mode)
      (setq-local company-backends (delq 'company-capf company-backends))
      (add-to-list 'company-backends '(company-lsp company-yasnippet company-files)))))

(comment
 (use-package lsp-origami
   :straight t
   :after (origami)
   :hook
   ((origami . lsp-origami-mode))))
