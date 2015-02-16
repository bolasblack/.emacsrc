(use-package zenburn-theme
  :init
  (load-theme 'zenburn t))

;;(use-package color-theme
;;  :init
;;  (progn
;;    (use-package tabbar-ruler
;;      :init
;;      (progn
;;        (setq tabbar-ruler-global-tabbar t)
;;        (tabbar-install-faces)
;;        (tabbar-ruler-group-by-projectile-project)))
;;    ))

(use-package help-fns+)

(use-package pinyin-search)

(use-package window-numbering
  :init
  (window-numbering-mode))

(use-package flycheck
  :init
  (use-package flycheck-cask)
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
  (add-hook 'emacs-lisp-mode-hook (lambda ()
                                    (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))))
  (add-hook 'coffee-mode-hook (lambda ()
                                (setq flycheck-coffeelintrc (concat dir-rc "flycheck.conf/coffee.json"))))
)

(use-package yasnippet
  :init
  (progn
    (add-to-list 'yas-snippet-dirs dir-snippet)
    (yas-global-mode 1)))

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
    (git-gutter:linum-setup)))

(use-package helm
  :init
  (progn
    (helm-mode t)
    (bind-key "C-w" 'kill-region-or-backward-delete-word-or-delim helm-map)
    (bind-key "C-w" 'kill-region-or-backward-delete-word-or-delim helm-generic-files-map)
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

(use-package slime
  :init
  (progn
    (setq inferior-lisp-program (or (getenv "LISP_PROGRAM") "clisp"))
    (slime-setup '(slime-fancy slime-company))))

(use-package indent-guide
  :init
  (indent-guide-global-mode))

(use-package multiple-cursors
  :bind
  ("C-c C-n" . mc/mark-next-lines)
  ("C-c C-p" . mc/mark-previous-lines)
  ("C-c n" . mc/mark-next-like-this)
  ("C-c p" . mc/mark-previous-like-this)
  ("C-c h" . mc/mark-all-like-this))

(use-package undo-tree
  :init
  (global-undo-tree-mode t))

(use-package robe
  :init
  (add-hook 'ruby-mode-hook 'robe-mode))

(use-package company
  :init
  (global-company-mode t)
  ;; 自动提示的最少字数
  (setq company-minimum-prefix-length 1)
  (push 'company-robe  company-backends)
  (push 'company-files company-backends)
  (use-package company-ycmd
    :init
    (company-ycmd-setup))
  )

(use-package ycmd
  :init
  (set-variable 'ycmd-server-command '("python" (getenv "YCMD_PATH")))
  (ycmd-setup))
