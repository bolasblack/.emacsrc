(require 'init-use-package)
(require 'dash)
(require 'comment)

(provide-me)

(c4:use lsp-mode
  :straight t
  :commands (lsp)
  :custom
  (lsp-prefer-flymake nil)
  (lsp-auto-configure nil))

(comment
 (c4:use lsp-ui
   :straight t
   :commands (lsp-ui-mode)
   :hook
   (lsp-mode . lsp-ui-mode)))

(c4:use company-lsp
  :straight t
  :commands (company-lsp)
  :preface
  (defun c4:company-lsp-company-mode-hook ()
    (when (and (boundp 'lsp-mode) lsp-mode)
      (setq-local company-backends (delq 'company-capf company-backends))
      (add-to-list 'company-backends '(company-lsp company-yasnippet company-files))))
  :hook
  (company-mode . c4:company-lsp-company-mode-hook)
  (lsp-mode . c4:company-lsp-company-mode-hook))

(comment
 (c4:use lsp-origami
   :straight t
   :after (origami)
   :hook
   ((origami . lsp-origami-mode))))
