(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package color-theme
  :disabled t
  :config
  (use-package tabbar-ruler
    :init
    (setq tabbar-ruler-global-tabbar t)
    :config
    (tabbar-install-faces)
    (tabbar-ruler-group-by-projectile-project)))

(use-package help-fns+)

(use-package pinyin-search)

(use-package window-numbering
  :config
  (window-numbering-mode))

(use-package flycheck
  :config
  (use-package flycheck-cask)
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))))
  (add-hook 'coffee-mode-hook (lambda ()
                                (setq flycheck-coffeelintrc (concat dir-rc "flycheck.conf/coffee.json"))))
)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs dir-snippet))

(use-package smartparens
  :config
  (use-package smartparens-config)
  (smartparens-global-mode))

(use-package git-gutter
  :config
  (custom-set-variables
   '(git-gutter:unchanged-sign " "))
  (global-git-gutter-mode t)
  (git-gutter:linum-setup))

(use-package helm
  :config
  (helm-mode t)
  :bind
  ("C-M-y" . helm-show-kill-ring)
  :bind-map
  ((:map helm-map ("C-w" . kill-region-or-backward-delete-word-or-delim))
   (:map helm-generic-files-map ("C-w" . kill-region-or-backward-delete-word-or-delim))))

(use-package projectile
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'helm)
  (use-package helm-projectile
    :bind ("M-p" . helm-projectile)))

(use-package popwin
  :config
  (popwin-mode t)

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
      (push config popwin:special-display-config))))

(use-package slime
  :init
  (setq inferior-lisp-program (or (getenv "LISP_PROGRAM") "clisp"))
  :config
  (slime-setup '(slime-fancy slime-company)))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package multiple-cursors
  :bind (
         ("C-c C-n" . mc/mark-next-lines)
         ("C-c C-p" . mc/mark-previous-lines)
         ("C-c n" . mc/mark-next-like-this)
         ("C-c p" . mc/mark-previous-like-this)
         ("C-c h" . mc/mark-all-like-this)
  ))

(use-package undo-tree
  :config
  (global-undo-tree-mode t))

(use-package robe
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package company
  :config
  (global-company-mode t)
  ;; 自动提示的最少字数
  (setq company-minimum-prefix-length 1)
  (push 'company-robe  company-backends)
  (push 'company-files company-backends)
  (use-package company-ycmd
    :config
    (company-ycmd-setup)))

(use-package ycmd
  :init
  (setq ycmd-server-command (list "python" (getenv "YCMD_PATH")))
  :config
  (ycmd-setup))
