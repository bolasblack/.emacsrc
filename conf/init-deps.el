;;; -*- lexical-binding: t -*-

(require 'comment)
(require 'straight)
(require 'use-package)
(require 'mode-helpers)

(provide-me)

(straight-override-recipe
 '(undo-tree :host github
             :repo "emacsmirror/undo-tree"))

;;;;;;;;;;;;;;;;;; Emacs 加强 ;;;;;;;;;;;;;;;;;;

;; 显示行号
(use-package display-line-numbers
  :if (fboundp 'global-display-line-numbers-mode)
  :config
  (global-display-line-numbers-mode t))
(use-package linum
  :if (not (fboundp 'global-display-line-numbers-mode))
  :straight t
  :custom
  (linum-format "%3d " "行号右边多一个空格")
  :config
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
  :straight help-fns-plus)

;; 自动调整提示窗口的位置的尺寸
(use-package popwin
  :straight t
  :custom
  (popwin:popup-window-height 15)
  :config
  (popwin-mode t)
  ;; Config: https://github.com/m2ym/popwin-el#special-display-config
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

;; 另外一种外观更丰富的补全方式
(use-package helm
  :straight t
  :defer t
  :delight
  :bind
  ("C-x b" . helm-projectile-switch-to-grouped-buffer)
  :config
  (require 'helm-config)
  (require 'helm-projectile-switch-to-grouped-buffer))

;; 缩进辅助线
(use-package indent-guide
  :straight t
  :delight
  :config
  (indent-guide-global-mode t))

;; 快速跳转到界面上某个地方
(use-package ace-jump-mode
  :straight t
  :defer t
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

;; 一个文件里支持多个主模式
(use-package polymode
  :straight t)

;;;;;;;;;;;;;;;;;;;; 编辑 ;;;;;;;;;;;;;;;;;;;;

;; 语法检查
(comment defun c4:flycheck-add-eslint/web-mode-hook ()
         (let ((file-ext (file-name-extension buffer-file-name)))
           (when (or
                  (string-equal "tsx" file-ext)
                  (string-equal "ts" file-ext))
             (cl-pushnew 'typescript-tide flycheck--automatically-enabled-checkers))
           (when (or
                  (string-equal "jsx" file-ext)
                  (string-equal "js" file-ext))
             (cl-pushnew 'javascript-eslint flycheck--automatically-enabled-checkers))))
(comment defun c4:flycheck-resize-id-column ()
         (setq flycheck-error-list-format
               `[("File" 6)
                 ("Line" 5 flycheck-error-list-entry-< :right-align t)
                 ("Col" 3 nil :right-align t)
                 ("Level" 8 flycheck-error-list-entry-level-<)
                 ("ID" 50 t)
                 (,(flycheck-error-list-make-last-column "Message" 'Checker) 0 t)]))
(use-package flycheck
  :straight t
  :defer t
  :config
  (global-flycheck-mode t)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))
              (when (s-starts-with? dir-conf (buffer-file-name))
                (make-local-variable 'flycheck-emacs-lisp-load-path)
                (setq flycheck-emacs-lisp-load-path 'inherit))))
  (add-hook 'coffee-mode-hook
            (lambda () (setq flycheck-coffeelintrc (concat dir-flycheck "coffee.json"))))
  (comment add-hook 'web-mode-hook 'c4:flycheck-add-eslint/web-mode-hook)
  (comment add-hook 'flycheck-error-list-mode-hook 'c4:flycheck-resize-id-column)
  (comment (flycheck-add-mode 'javascript-eslint 'web-mode)))
(use-package flycheck-popup-tip
  :straight t
  :if (not (display-graphic-p))
  :after flycheck
  :config
  (flycheck-popup-tip-mode t))
(use-package flycheck-pos-tip
  :straight t
  :if (display-graphic-p)
  :after flycheck)
(use-package flycheck-posframe
  :straight t
  :if (display-graphic-p)
  :after (flycheck-pos-tip)
  :config
  (if (posframe-workable-p)
      (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
    (flycheck-pos-tip-mode t)))

;; 自动标点配对（不只是标点配对）
(use-package smartparens
  :straight t
  :defer .1
  :delight
  :config
  (smartparens-global-mode t))

(use-package smartparens-config
  :after smartparens)

(use-package undo-tree
  :straight t
  :defer t
  :delight " undotree"
  :config
  (global-undo-tree-mode t))

(load-relative "./deps/company")

(load-relative "./deps/ivy")

;; snippet 引擎
(use-package yasnippet
  :straight t
  :commands (yas-global-mode
             yas-minor-mode
             yas-expand-snippet
             yas-activate-extra-mode
             yas-deactivate-extra-mode)
  :config
  (add-to-list 'yas-snippet-dirs dir-snippet)
  (yas-global-mode t))

(use-package drag-stuff
  :straight t
  :defer t)

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
  :straight t
  :defer t)

;; 自动调整缩进
(use-package aggressive-indent
  :straight t
  :defer t)

;; EditorConfig
(use-package editorconfig
  :straight t
  :defer t
  :delight
  :ensure-system-package editorconfig
  :config
  (editorconfig-mode t))

;;;;;;;;;;;;;;;;;;;; 项目 ;;;;;;;;;;;;;;;;;;;;

;; CtrlP
(use-package projectile
  :straight t
  :after (evil)
  :ensure-system-package (fd)
  :delight
  '(:eval (concat " p[" (projectile-project-name) "]"))
  :bind
  (:map evil-normal-state-map ("C-p" . projectile-find-file))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (require 'dash)
  (setq projectile-project-root-files-bottom-up
        (-union
         '("package.json"
           "shadow-cljs.edn")
         projectile-project-root-files-bottom-up))
  (projectile-mode t))

;; 显示对比上次 commit 做了些什么修改
(use-package git-gutter
  :straight t
  :defer t
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
  :defer t)

(comment
 (use-package vc-hooks
   :defer t
   :config
   (setcdr (assq 'vc-mode mode-line-format)
           '((:eval (->> (replace-regexp-in-string "^ Git:" " " vc-mode)
                         (replace-regexp-in-string "^ Git-" " " vc-mode)
                         (replace-regexp-in-string "feature/" "f/")))))))

(use-package color-rg
  :straight (color-rg :host github
                      :repo "manateelazycat/color-rg"
                      :files ("color-rg.el"))
  :defer t
  :commands (color-rg-search-input
             color-rg-search-symbol
             color-rg-search-symbol-with-type
             color-rg-search-input-in-current-file
             color-rg-search-symbol-in-current-file
             color-rg-search-project
             color-rg-search-input-in-project
             color-rg-search-symbol-in-project
             color-rg-search-project-rails
             color-rg-search-project-rails-with-type)
  :ensure-system-package (rg)
  :bind
  (:map evil-normal-state-map ("RET" . color-rg-open-file)))

;;;;;;;;;;;;;;;;;;;; 其他文件的支持 ;;;;;;;;;;;;;;;;;;;;

(use-package poly-markdown
  :straight t
  :after (polymode)
  :mode
  ("\\.md\\'" . poly-markdown-mode)
  ("\\.mdx\\'" . poly-markdown-mode)
  ("\\.markdown\\'" . poly-markdown-mode))

(use-package lua-mode
  :straight t
  :defer t
  :mode ("\\.lua\\'")
  :interpreter "lua"
  :custom
  (lua-indent-level 2))

(use-package gitignore-mode
  :straight t
  :defer t
  :mode ("gitignore$"))

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package nginx-mode
  :straight t
  :defer t)

(use-package nix-mode
  :straight t
  :defer t)

(use-package apples-mode ;; AppleScript
  :straight t
  :defer t
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

(use-package hcl-mode
  :straight t
  :defer t
  :mode ("\\.tf\\'"))

(use-package dockerfile-mode
  :straight t
  :defer t
  :mode ("dockerfile$"))

(use-package ledger-mode
  :straight t
  :defer t
  :mode ("\\.beancount\\'" "\\.bean\\'")
  :init
  (add-hook 'ledger-mode-hook
            (lambda ()
              (bind-key "C-M-i" 'ledger-magic-tab ledger-mode-map))))

(use-package beancount-mode
  :straight (beancount :host github
                       :repo "beancount/beancount"
                       :files ("editors/emacs/*.el"))
  :after ledger-mode
  :hook ledger-mode)

(use-package sh-script
  :defer t
  :mode
  ("zshrc$" . shell-script-mode)
  :custom
  (sh-basic-offset 2))
(comment
 progn
 (require 'sh-script)
 (define-hostmode poly-shell-script-hostmode
   :mode 'shell-script-mode)
 (define-innermode poly-shell-script-innermode
   :mode nil
   :fallback-mode 'host
   :head-mode 'host
   :tail-mode 'host)
 (define-auto-innermode poly-shell-script-fenced-code-innermode poly-shell-script-innermode
   :head-matcher (cons "^.*\\(<<LANG_[[:alpha:]]+\n\\)" 1)
   :tail-matcher (cons "^\\(LANG_[[:alpha:]]+\\)$" 1)
   :mode-matcher (cons "<<LANG_\\([[:alpha:]]+\\)" 1)
   :head-mode 'host
   :tail-mode 'host)
 (define-polymode poly-shell-script-mode
   :hostmode 'poly-shell-script-hostmode
   :innermodes '(poly-shell-script-fenced-code-innermode))
 (add-to-list 'auto-mode-alist '("\\.sh\\'" . poly-shell-script-mode)))

(use-package nxml-mode
  :defer t
  :mode ("\\.aiml$"))

;;;;;;;;;;;;;;;;;;;; Common Lisp ;;;;;;;;;;;;;;;;;;;;

;; Common Lisp 开发环境
;; (use-package slime
;;   :straight t
;;   :init
;;   (setq inferior-lisp-program (or (getenv "LISP_PROGRAM") "clisp"))
;;   :config
;;   (slime-setup '(slime-fancy slime-company)))
;; (use-package slime-company
;;   :straight t)

;;;;;;;;;;;;;;;;;;;; Ruby ;;;;;;;;;;;;;;;;;;;;

;; Ruby 开发环境
;; (use-package robe
;;   :straight t
;;   :config
;;   (add-hook 'ruby-mode-hook 'robe-mode))
;; (use-package inf-ruby
;;   :straight t)

;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;

;; Clojure 开发环境
(use-package clojure-mode
  :straight t
  :defer t
  :mode ("\\.clj\\'" "\\.cljs\\'")
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

;; (use-package inf-clojure
;;   :straight t
;;   :defer t
;;   :hook clojure-mode
;;   :custom
;;   (inf-clojure-generic-cmd "lumo -d")
;;   (inf-clojure-boot-cmd "lumo -d"))

(use-package cider
  :straight t
  :after (clojure-mode))

;;;;;;;;;;;;;;;;;;;; Emacs Lisp ;;;;;;;;;;;;;;;;;;;;

(use-package elisp-mode
  :defer t
  :delight
  (emacs-lisp-mode ("El" (lexical-binding ":Lex" ":Dyn")))
  :mode
  ("Cask"))

(use-package pretty-eval-last-sexp
  :commands
  (pretty-eval-last-sexp)
  :bind
  ("C-x C-e" . pretty-eval-last-sexp))

;;;;;;;;;;;;;;;;;;;; Lisp ;;;;;;;;;;;;;;;;;;;;

(use-package paredit
  :straight t
  :defer t
  :commands (enable-paredit-mode)
  :delight
  :hook ((clojurescript-mode
          clojure-mode
          clojurescript-mode
          eval-expression-minibuffer-setup
          emacs-lisp-mode
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . enable-paredit-mode))

(use-package selected
  :straight t
  :defer t
  :delight
  :no-require t)
(use-package parinfer-smart
  :straight
  (parinfer-smart :host github
                  :branch "smart"
                  :repo "DogLooksGood/parinfer-mode"
                  :files ("*.el"))
  :defer t
  :after (company)
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
          clojurescript-mode
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
  (defun parinfer---lint ()
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
  :defer t
  :delight
  :hook ((clojurescript-mode
          clojure-mode
          emacs-lisp-mode
          common-lisp-mode
          scheme-mode
          lisp-mode) . rainbow-delimiters-mode))

(load-relative "./deps/evil")
(load-relative "./deps/origami")
(load-relative "./deps/lsp")
(load-relative "./deps/frontend-support")
(load-relative "./deps/json-support")
(load-relative "./deps/org-mode")
