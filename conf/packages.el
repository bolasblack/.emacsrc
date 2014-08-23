
(let ((cask-paths (split-string (shell-command-to-string "cask load-path") ":")))
  (dolist (path cask-paths)
    (add-to-list 'load-path path)))

(require 'cask)
(cask-initialize "~/.emacsrc")

(require 'use-package)

(use-package color-theme)
(use-package color-theme-solarized
  :init
  (progn
    (load-theme 'solarized-dark t)))

(use-package tabbar-ruler
  :init
  (progn
    (setq tabbar-ruler-global-tabbar t)
    (tabbar-install-faces)))

(use-package flx-ido
  :init
  (progn
    (ido-mode)
    (ido-everywhere)
    (flx-ido-mode)
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)
    (bind-key "M-w" 'ido-copy-current-file-name ido-file-completion-map)
    (bind-key "C-w" 'ido-delete-backward-updir ido-file-completion-map)
    (bind-key "C-w" 'ido-delete-backward-updir ido-file-dir-completion-map)))

(use-package help-fns+)

(use-package window-numbering
  :init
  (window-numbering-mode))

(use-package flycheck
  :init
  (use-package flycheck-cask)
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(use-package yasnippet
  :init
  (progn
    (yas-global-mode)))

(use-package auto-complete
  :init
  (progn
    (use-package auto-complete-config)
    (ac-config-default)))

(use-package smartparens
  :init
  (progn
    (use-package smartparens-config)
    (smartparens-global-mode)))

(use-package browse-kill-ring
  :bind ("C-M-y" . browse-kill-ring))
