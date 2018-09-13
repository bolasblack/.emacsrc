;;; -*- lexical-binding: t -*-

(require 'comment)
(require 'smart-delete)
(require 'straight)
(require 'use-package)

(provide-me)

(straight-override-recipe
 '(undo-tree :host github
             :repo "emacsmirror/undo-tree"))

;;;;;;;;;;;;;;;;;; 扩展库 ;;;;;;;;;;;;;;;;;;

(use-package edn
  :straight t)

(use-package dash
  :straight t)

(use-package s
  :straight t)

(use-package f
  :straight t)

;;;;;;;;;;;;;;;;;; Emacs 加强 ;;;;;;;;;;;;;;;;;;

;; 显示行号
(use-package display-line-numbers
  :if (fboundp 'global-display-line-numbers-mode)
  :config
  (global-display-line-numbers-mode t))
(use-package linum
  :if (not (fboundp 'global-display-line-numbers-mode))
  ;; 如果一开始就激活 global-linum-mode 会导致 emacs --daemon 崩溃，无法正常启动
  ;; https://github.com/kaushalmodi/.emacs.d/issues/4
  ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2010-07/msg00518.html
  :defer 1
  :straight t
  :config
  (setq linum-format "%3d ")
  (global-linum-mode t))

;; 给各个窗口编号
(use-package window-numbering
  :straight t
  :config
  (window-numbering-mode t))

;; 主题
(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))

;; 增强 Emacs 的帮助系统
;; http://www.emacswiki.org/emacs/HelpPlus#toc3
(use-package help-fns+
  :straight t
  :defer)

;; 自动调整提示窗口的位置的尺寸
(use-package popwin
  :straight t
  :config
  (popwin-mode t)

  ;; Config: https://github.com/m2ym/popwin-el#special-display-config
  (setq popwin:popup-window-height 15)

  (let ((c '(;; Emacs
             ("*Procces List*" :height 20)
             ("*Warnings*" :height 20)
             ("*Messages*" :height 20)
             ("*Backtrace*" :height 20)
             ("*Compile-Log*" :height 20 :noselect t)
             ;; Helm
             ("^\*helm.*\*$" :regexp t))))
    (dolist (config c)
      (push config popwin:special-display-config))))

;; 一种补全方式
(use-package ivy
  :straight t
  :defer 1
  :delight
  :bind
  (:map ivy-minibuffer-map ("C-w" . ivy-backward-kill-word))
  :config
  (ivy-mode t)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(ivy-current-match           ((t (                              :background ,zenburn-bg-1))))
     `(ivy-remote                  ((t (:foreground ,zenburn-blue     :background ,zenburn-bg))))
     `(ivy-subdir                  ((t (:foreground ,zenburn-yellow   :background ,zenburn-bg))))
     `(ivy-minibuffer-match-face-1 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold)))))))

;; 扩展 ivy
(use-package counsel
  :after ivy
  :straight t
  :bind
  (:map counsel-find-file-map ("C-w" . ivy-backward-kill-word)))

;; 另外一种外观更丰富的补全方式
(use-package helm
  :commands (helm-projectile-switch-to-grouped-buffer)
  :straight t
  :delight
  :init
  (bind-key "C-x b" 'helm-projectile-switch-to-grouped-buffer)
  :config
  (require 'helm-config)
  (require 'helm-projectile-switch-to-grouped-buffer))

;; 缩进辅助线
(use-package indent-guide
  :straight t
  :defer
  :delight
  :config
  (indent-guide-global-mode t))

;; 快速跳转到界面上某个地方
(use-package ace-jump-mode
  :straight t
  :bind
  ("C-c C-c" . ace-jump-word-mode))

;; 让 Emacs 支持在 Shell 里自定义的 PATH
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; 按了 prefix 一段时间后底部会弹出一个小窗口显示接下来可以按的键
(use-package which-key
  :straight t
  :delight
  :config
  (which-key-mode t))

;; 使用 C-p C-n 时平滑滚动，而不是直接向上/下一页跳几行
(use-package smooth-scrolling
  :straight t
  :config
  (smooth-scrolling-mode t))

;;;;;;;;;;;;;;;;;;;; Evil ;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :straight t
  :defer 1
  :config
  (evil-mode t)
  :bind
  (:map evil-normal-state-map ("C-u" . scroll-down-command)))

(use-package general
  :straight t
  :config
  (general-evil-setup))

(use-package evil-nerd-commenter
  :straight t
  :after (general evil)
  :general
  (general-nmap :prefix ","
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line))

(use-package evil-easymotion
  :straight t
  :after evil
  :config
  (setq avy-style 'at-full)
  (setq avy-background t)
  (define-key evil-motion-state-map (kbd ",") nil)
  (evilem-default-keybindings (kbd ",,")))

(use-package origami
  :straight t
  :after evil
  :bind
  (:map evil-normal-state-map ("zo" . origami-open-node))
  (:map evil-normal-state-map ("zO" . origami-open-node-recursively))
  (:map evil-normal-state-map ("zc" . origami-close-node))
  (:map evil-normal-state-map ("zC" . origami-close-node-recursively))
  (:map evil-normal-state-map ("za" . origami-toggle-node))
  (:map evil-normal-state-map ("zA" . origami-recursively-toggle-node)))

;;;;;;;;;;;;;;;;;;;; 编辑 ;;;;;;;;;;;;;;;;;;;;

;; 语法检查
(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode t)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
              (when (s-starts-with? dir-conf (buffer-file-name))
                (make-local-variable 'flycheck-emacs-lisp-load-path)
                (setq flycheck-emacs-lisp-load-path 'inherit))))
  (add-hook 'coffee-mode-hook
            (lambda () (setq flycheck-coffeelintrc (concat dir-flycheck "coffee.json")))))
(use-package flycheck-popup-tip
  :if (not (display-graphic-p))
  :after flycheck
  :straight t
  :config
  (flycheck-popup-tip-mode t))
(use-package flycheck-pos-tip
  :if (display-graphic-p)
  :after flycheck
  :straight t
  :config
  (flycheck-pos-tip-mode t))

;; 自动标点配对（不只是标点配对）
(use-package smartparens
  :straight t
  :delight
  :config
  (smartparens-global-mode t))

(use-package smartparens-config
  :after smartparens)

(use-package undo-tree
  :straight t
  :delight " undotree"
  :config
  (global-undo-tree-mode t))

(use-package company
  :straight t
  :delight
  :config
  (global-company-mode t)
  ;; 自动提示的最少字数
  (setq company-minimum-prefix-length 1)
  (setq company-backends
        '((company-files
           company-keywords
           company-dabbrev-code
           company-dabbrev
           company-yasnippet))))

;; snippet 引擎
(use-package yasnippet
  :straight t
  :defer
  :commands (yas-activate-extra-mode yas-deactivate-extra-mode)
  :config
  (add-to-list 'yas-snippet-dirs dir-snippet)
  (yas-global-mode t))

(use-package drag-stuff
  :straight t)

;; Sublime Text 的多光标模式
;; (use-package multiple-cursors
;;   :straight t
;;   :bind
;;   (("C-c C-n" . mc/mark-next-lines)
;;    ("C-c C-p" . mc/mark-previous-lines)
;;    ("C-c n" . mc/mark-next-like-this)
;;    ("C-c p" . mc/mark-previous-like-this)
;;    ("C-c h" . mc/mark-all-like-this)))

;; 便捷选区
(use-package expand-region
  :straight t)

;; 自动调整缩进
(use-package aggressive-indent
  :straight t)

;; EditorConfig
(use-package editorconfig
  :straight t
  :delight
  :ensure-system-package editorconfig
  :config
  (editorconfig-mode t))

;;;;;;;;;;;;;;;;;;;; 项目 ;;;;;;;;;;;;;;;;;;;;

;; CtrlP
(use-package projectile
  :straight t
  :after evil
  :delight
  '(:eval (concat " p[" (projectile-project-name) "]"))
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode t)
  (setq projectile-project-root-files-bottom-up
        (-union projectile-project-root-files-bottom-up
                '("package.json"
                  "shadow-cljs.edn")))
  :bind
  (:map evil-normal-state-map
        ("C-p" . projectile-find-file)))

;; 显示对比上次 commit 做了些什么修改
(use-package git-gutter
  :straight t
  :delight
  :config
  (global-git-gutter-mode t)
  (git-gutter:linum-setup)

  (set-face-attribute 'git-gutter:added nil
                      :inverse-video nil)
  (set-face-attribute 'git-gutter:deleted nil
                      :inverse-video nil)
  (set-face-attribute 'git-gutter:modified nil
                      :inverse-video nil)
  (set-face-attribute 'git-gutter:unchanged nil
                      :inverse-video nil)
  (set-face-attribute 'git-gutter:separator nil)

  (custom-set-variables
   '(git-gutter:unchanged-sign " ")))

(use-package magit
  :straight t
  :defer)

;;;;;;;;;;;;;;;;;;;; 其他文件的支持 ;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :straight t
  :defer)

(use-package coffee-mode
  :straight t
  :defer)

(use-package jade-mode
  :straight t
  :defer)

(use-package sass-mode
  :straight t
  :defer
  :mode ("\\.styl\\'"))

(use-package lua-mode
  :straight t
  :defer
  :mode ("\\.lua\\'")
  :interpreter "lua"
  :config
  (setq lua-indent-level 2))

(use-package less-css-mode
  :straight t
  :defer)

(use-package gitignore-mode
  :straight t
  :defer)

(use-package yaml-mode
  :straight t
  :defer)

(use-package nginx-mode
  :straight t
  :defer)

(use-package jsx-mode
  :straight t
  :defer)

(use-package typescript-mode
  :straight t
  :defer
  :delight "ts"
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'js-mode)
              (tide-setup)))
  :config
  (setq typescript-indent-level 2))

(use-package apples-mode ;; AppleScript
  :straight t
  :defer
  :mode ("\\.applescript\\'")
  :config
  ;; OS X Plist

  ;; Emacs provides jka-compr which decompresses a file to stdout for reading,
  ;; and compresses the data from stdin to write the file back out again.

  ;; Allow editing of binary .plist files.
  (add-to-list 'jka-compr-compression-info-list
               ["\\.plist$"
                "converting text XML to binary plist"
                "plutil"
                ("-convert" "binary1" "-o" "-" "-")
                "converting binary plist to text XML"
                "plutil"
                ("-convert" "xml1" "-o" "-" "-")
                nil nil "bplist"])
  ;;It is necessary to perform an update!
  (jka-compr-update))

(use-package web-mode
  :straight t
  :defer
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.erb\\'" "\\.html\\'" "\\.vue\\'")
  :interpreter ("node" "nodejs" "gjs" "rhino")
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (add-hook 'web-mode-hook
            (lambda ()
              (let ((file-ext (file-name-extension buffer-file-name)))
                (when (or (string-equal "tsx" file-ext)
                          (string-equal "ts" file-ext))
                  (tide-setup)))))
  (defadvice indent-for-tab-command (around web-mode-setup-yas-extra-mode activate)
    (interactive)
    (if (and (equal major-mode 'web-mode)
             (called-interactively-p 'interactive))
        (let ((curr-lang (web-mode-language-at-pos))
              (lang-mode-map '(("css"        . css-mode)
                               ("html"       . html-mode)
                               ("javascript" . js-mode)
                               ("jsx"        . js-mode))))
          (-reduce-from (lambda (matched-mode pair)
                          (if (string= curr-lang (car pair))
                              (progn
                                (yas-activate-extra-mode (cdr pair))
                                (cdr pair))
                            (progn
                              (if (not (equal (cdr pair) matched-mode))
                                  (yas-deactivate-extra-mode (cdr pair)))
                              matched-mode)))
                        nil lang-mode-map)))
    (let ((tab-key-fn (key-binding (kbd "<tab>"))))
      (if (and tab-key-fn
               (not (equal #'indent-for-tab-command tab-key-fn)))
          (call-interactively tab-key-fn)
        (call-interactively (ad-get-orig-definition 'indent-for-tab-command))))))

(use-package hcl-mode
  :straight t
  :defer
  :mode ("\\.tf\\'"))

(use-package dockerfile-mode
  :straight t
  :defer)

(use-package graphql-mode
  :straight t
  :defer
  :mode ("\\.gql\\'" "\\.graphql\\'"))

(use-package ledger-mode
  :straight t
  :defer
  :mode ("\\.beancount\\'" "\\.bean\\'")
  :init
  (add-hook 'ledger-mode-hook
            (lambda ()
              (bind-key "C-M-i" 'ledger-magic-tab ledger-mode-map))))

(use-package beancount-mode
  :straight (beancount :host github
                       :repo "beancount/beancount"
                       :files ("editors/emacs/*.el"))
  :defer
  :after ledger-mode
  :hook (ledger-mode . beancount-mode))

(use-package sh-script
  :defer
  :mode ("\\.zsh$" "zshrc$")
  :config
  (setq sh-basic-offset 2))

(use-package css-mode
  :defer
  :config
  (setq css-indent-offset 2))

(use-package js
  :defer
  :config
  (setq js-indent-level 2))

(use-package nxml-mode
  :defer
  :mode ("\\.aiml$"))

;;;;;;;;;;;;;;;;;;;; 开发环境 ;;;;;;;;;;;;;;;;;;;;

;; Common Lisp 开发环境
;; (use-package slime
;;   :straight t
;;   :init
;;   (setq inferior-lisp-program (or (getenv "LISP_PROGRAM") "clisp"))
;;   :config
;;   (slime-setup '(slime-fancy slime-company)))
;; (use-package slime-company
;;   :straight t)

;; Ruby 开发环境
;; (use-package robe
;;   :straight t
;;   :config
;;   (add-hook 'ruby-mode-hook 'robe-mode))
;; (use-package inf-ruby
;;   :straight t)

;; Clojure 开发环境
(use-package clojure-mode
  :straight t
  :defer
  :delight
  (clojure-mode "cl")
  (clojurescript-mode "cljs")
  :config
  (add-hook 'clojure-mode-hook
            (lambda () (setq-local prettify-symbols-alist nil)))
  (define-clojure-indent
    (fn-go 'defun)
    (go-let 'defun)
    (go-try-let 'defun)
    (alet 'defun)
    (mlet 'defun)
    (describe 'defun)
    (it 'defun)
    (cond-converge 'defun)))

(use-package inf-clojure
  :straight t
  :defer
  :hook (clojure-mode)
  :config
  (setq inf-clojure-generic-cmd "lumo -d")
  (setq inf-clojure-boot-cmd "lumo -d"))

(comment use-package cider
  :straight t
  :defer)

(use-package elisp-mode
  :delight
  (emacs-lisp-mode ("El" (lexical-binding ":Lex" ":Dyn")))
  :mode ("Cask"))

(use-package paredit
  :commands (enable-paredit-mode)
  :straight t
  :defer
  :delight
  :hook ((clojurescript-mode
          clojure-mode
          eval-expression-minibuffer-setup
          emacs-lisp-mode
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . enable-paredit-mode))

(use-package selected
  :straight t
  :delight
  :no-require t)
(use-package parinfer-smart
  :straight
  (parinfer-smart :host github
                  :branch "smart"
                  :repo "DogLooksGood/parinfer-mode"
                  :files ("*.el"))
  :delight
  (parinfer-mode (:eval (if (boundp 'parinfer--mode)
                            (progn
                              ((eq 'paren parinfer--mode) " P:Paren")
                              ((eq 'indent parinfer--mode) " P:Indent")
                              (t " P:Unknown"))
                          " parinfer")))
  :bind
  (:map paredit-mode-map
        (";" . self-insert-command))
  :hook ((clojure-mode
          emacs-lisp-mode
          common-lisp-mode
          scheme-mode
          lisp-mode) . parinfer-mode)
  :config
  (setq parinfer-partial-process t
        parinfer-extensions
        '(defaults        ; should be included.
           pretty-parens  ; different paren styles for different modes.
           evil           ; If you use Evil.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank))   ; Yank behavior depend on mode.
  (defun parinfer--lint ()
    (unless (string-prefix-p "*temp*" (string-trim (buffer-name)))
      (let ((err nil))
        (when (save-excursion (goto-char (point-min))
                              (search-forward "\t" (point-max) t))
          (setq err "Can't enable parinfer due to inconsistent indentation."))
        (let ((buffer-text (buffer-substring-no-properties (point-min) (point-max)))
              (mm major-mode))
          (with-temp-buffer
            (insert buffer-text)
            (funcall mm)
            (parinfer--initial-states)
            (condition-case ex
                (parinfer--process-buffer)
              (error
               (setq err (concat "Can't enable parinfer due to error: " (cadr ex)))))
            (unless (or (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                      buffer-text)
                        (yes-or-no-p "Enable parinfer will modify current buffer content, continue?"))
              (setq err "Can't enable parinfer due to buffer will be changed."))))
        err))))

(use-package rainbow-delimiters
  :straight t
  :defer
  :delight
  :hook ((clojurescript-mode
          clojure-mode
          emacs-lisp-mode
          common-lisp-mode
          scheme-mode
          lisp-mode) . rainbow-delimiters-mode))

;; 网页前端开发环境
(use-package rainbow-mode
  :straight t
  :defer
  :delight
  :hook (web-mode
         css-mode
         sass-mode
         scss-mode))

(use-package web-beautify
  :straight t
  :defer
  :ensure-system-package (js-beautify . "yarn global add js-beautify"))

(use-package tide
  :straight t
  :defer
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))
