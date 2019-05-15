(require 'init-use-package)

(provide-me)

(use-package lsp-mode
  :straight t
  :defer t
  :commands (lsp)
  :custom
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :straight t
  :after (lsp-mode flycheck)
  :commands (lsp-ui-mode))

(use-package company-lsp
  :straight t
  :after (lsp-mode company)
  :commands (company-lsp)
  :config
  (push 'company-lsp company-backends))
