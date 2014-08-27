
(let ((cask-command (format "cask --path '%s' load-path" dir-rc)))
  (let ((cask-paths (split-string (shell-command-to-string cask-command) ":")))
    (dolist (path cask-paths)
      (add-to-list 'load-path path))))

(require 'cask)
(cask-initialize "~/.emacsrc")

(require 'use-package)

(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

(use-package color-theme
  :init
  (progn
    (use-package tabbar-ruler
      :init
      (progn
        (setq tabbar-ruler-global-tabbar t)
        (tabbar-install-faces)
        (tabbar-ruler-group-by-projectile-project)))
    ))

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

(use-package git-gutter
  :init
  (progn
    (custom-set-variables
     '(git-gutter:unchanged-sign " "))
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
))

(use-package helm
  :init
  (progn
    (helm-mode t)
    (bind-key "C-w" 'kill-region-or-backward-kill-word helm-map)
    (bind-key "C-M-y" 'helm-show-kill-ring)))

(use-package projectile
  :init
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'helm)
    (use-package helm-projectile
      :bind ("M-p" . helm-projectile))))

(use-package popwin
  :init
  (progn
    ; Config: https://github.com/m2ym/popwin-el#special-display-config
    (setq popwin:popup-window-height 15)

    (let ((c '(
               ;; Emacs
               ("*Procces List*" :height 20)
               ("*Warnings*" :height 20)
               ("*Messages*" :height 20)
               ("*Backtrace*" :height 20)
               ("*Compile-Log*" :height 20 :noselect t)
               ;; Helm
               ("^\*helm.*\*$" :regexp t)
               )))
      (dolist (config c)
        (push config popwin:special-display-config)))

    (popwin-mode t)))