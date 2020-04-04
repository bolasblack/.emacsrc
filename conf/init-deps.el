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
(c4:use display-line-numbers
  :if (fboundp 'global-display-line-numbers-mode)
  :config
  (global-display-line-numbers-mode t))
(c4:use linum
  :if (not (fboundp 'global-display-line-numbers-mode))
  :straight t
  :custom
  (linum-format "%3d " "行号右边多一个空格")
  :config
  (global-linum-mode t))

;; 给各个窗口编号
(c4:use window-numbering
  :straight t
  :config
  (window-numbering-mode t))

;; 主题
(c4:use zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))

;; 增强 Emacs 的帮助系统
;; http://www.emacswiki.org/emacs/HelpPlus#toc3
(c4:use help-fns+
  :straight help-fns-plus)

;; 自动调整提示窗口的位置的尺寸
(c4:use popwin
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
(c4:use helm
  :straight t
  :defer t
  :delight
  :config
  (require 'helm-config))
(c4:use helm-projectile-switch-to-grouped-buffer
  :after (helm projectile)
  :bind
  ("C-x b" . helm-projectile-switch-to-grouped-buffer))

;; 缩进辅助线
(c4:use indent-guide
  :straight t
  :delight
  :config
  (indent-guide-global-mode t))

;; 快速跳转到界面上某个地方
(c4:use ace-jump-mode
  :straight t
  :defer t
  :bind
  ("C-c C-c" . ace-jump-word-mode))

;; 让 Emacs 支持在 Shell 里自定义的 PATH
(c4:use exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; 按了 prefix 一段时间后底部会弹出一个小窗口显示接下来可以按的键
(c4:use which-key
  :straight t
  :delight
  :config
  (which-key-mode t))

;; 使用 C-p C-n 时平滑滚动，而不是直接向上/下一页跳几行
(c4:use smooth-scrolling
  :straight t
  :config
  (smooth-scrolling-mode t))

;; 一个文件里支持多个主模式
(c4:use polymode
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
(c4:use flycheck
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
(c4:use flycheck-popup-tip
  :straight t
  :if (not (display-graphic-p))
  :after flycheck
  :config
  (flycheck-popup-tip-mode t))
(c4:use flycheck-pos-tip
  :straight t
  :if (display-graphic-p)
  :after flycheck)
(c4:use flycheck-posframe
  :straight t
  :if (display-graphic-p)
  :after (flycheck-pos-tip)
  :config
  (if (posframe-workable-p)
      (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
    (flycheck-pos-tip-mode t)))

;; 自动标点配对（不只是标点配对）
(c4:use smartparens
  :straight t
  :defer .1
  :delight
  :config
  (smartparens-global-mode t))

(c4:use smartparens-config
  :after smartparens)

(c4:use undo-tree
  :straight t
  :defer t
  :delight " utree"
  :config
  (global-undo-tree-mode t))

(load-relative "./deps/company")

(load-relative "./deps/ivy")

;; snippet 引擎
(c4:use yasnippet
  :straight t
  :commands (yas-global-mode
             yas-minor-mode
             yas-expand-snippet
             yas-activate-extra-mode
             yas-deactivate-extra-mode)
  :config
  (add-to-list 'yas-snippet-dirs dir-snippet)
  (yas-global-mode t))

(c4:use drag-stuff
  :straight t
  :defer t)

;; Sublime Text 的多光标模式
;; (c4:use multiple-cursors
;;   :straight t
;;   :bind
;;   (("C-c C-n" . mc/mark-next-lines)
;;    ("C-c C-p" . mc/mark-previous-lines)
;;    ("C-c n" . mc/mark-next-like-this)
;;    ("C-c p" . mc/mark-previous-like-this)
;;    ("C-c h" . mc/mark-all-like-this)))

;; 便捷选区
(c4:use expand-region
  :straight t
  :defer t)

;; 自动调整缩进
(c4:use aggressive-indent
  :straight t
  :defer t)

;; EditorConfig
(c4:use editorconfig
  :straight t
  :defer t
  :delight
  :ensure-system-package editorconfig
  :config
  (editorconfig-mode t))

;;;;;;;;;;;;;;;;;;;; 项目 ;;;;;;;;;;;;;;;;;;;;

;; CtrlP
(c4:use projectile
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
  (require 'cl-generic)
  (setq projectile-project-root-files-bottom-up
        (-union
         '("package.json"
           "shadow-cljs.edn")
         projectile-project-root-files-bottom-up))
  (projectile-mode t)
  (require 'project)
  (defun project-try-projectile (dir)
    (let ((root (projectile-project-root)))
      (and root (cons 'projectile root))))
  (cl-defmethod project-roots ((project (head projectile)))
    (list (cdr project)))
  (setq project-find-functions
        (-concat (list #'projectile-project-root) project-find-functions)))

;; 显示对比上次 commit 做了些什么修改
(c4:use git-gutter
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

(c4:use magit
  :straight t
  :defer t)

(comment
 (c4:use vc-hooks
   :defer t
   :config
   (setcdr (assq 'vc-mode mode-line-format)
           '((:eval (->> (replace-regexp-in-string "^ Git:" " " vc-mode)
                         (replace-regexp-in-string "^ Git-" " " vc-mode)
                         (replace-regexp-in-string "feature/" "f/")))))))

(c4:use color-rg
  :straight (color-rg :host github
                      :repo "manateelazycat/color-rg"
                      :files ("color-rg.el"))
  :defer t
  :after (evil)
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

(c4:use poly-markdown
  :straight t
  :after (polymode)
  :mode
  ("\\.md\\'" . poly-markdown-mode)
  ("\\.mdx\\'" . poly-markdown-mode)
  ("\\.markdown\\'" . poly-markdown-mode))

(c4:use lua-mode
  :straight t
  :defer t
  :mode ("\\.lua\\'")
  :interpreter "lua"
  :custom
  (lua-indent-level 2))

(c4:use gitignore-mode
  :straight t
  :defer t
  :mode ("gitignore$"))

(c4:use yaml-mode
  :straight t
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(c4:use nginx-mode
  :straight t
  :defer t)

(c4:use nix-mode
  :straight t
  :defer t)

(c4:use apples-mode ;; AppleScript
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

(c4:use hcl-mode
  :straight t
  :defer t
  :mode ("\\.tf\\'"))

(c4:use dockerfile-mode
  :straight t
  :defer t
  :mode ("dockerfile$"))

(c4:use ledger-mode
  :straight t
  :defer t
  :mode ("\\.beancount\\'" "\\.bean\\'")
  :init
  (add-hook 'ledger-mode-hook
            (lambda ()
              (bind-key "C-M-i" 'ledger-magic-tab ledger-mode-map))))

(c4:use beancount-mode
  :straight (beancount :host github
                       :repo "beancount/beancount"
                       :files ("editors/emacs/*.el"))
  :after ledger-mode
  :hook ledger-mode)

(c4:use sh-script
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

(c4:use nxml-mode
  :defer t
  :mode ("\\.aiml$"))

;;;;;;;;;;;;;;;;;;;; Common Lisp ;;;;;;;;;;;;;;;;;;;;

;; Common Lisp 开发环境
;; (c4:use slime
;;   :straight t
;;   :init
;;   (setq inferior-lisp-program (or (getenv "LISP_PROGRAM") "clisp"))
;;   :config
;;   (slime-setup '(slime-fancy slime-company)))
;; (c4:use slime-company
;;   :straight t)

;;;;;;;;;;;;;;;;;;;; Ruby ;;;;;;;;;;;;;;;;;;;;

;; Ruby 开发环境
;; (c4:use robe
;;   :straight t
;;   :config
;;   (add-hook 'ruby-mode-hook 'robe-mode))
;; (c4:use inf-ruby
;;   :straight t)

;;;;;;;;;;;;;;;;;;;; Rust ;;;;;;;;;;;;;;;;;;;;

(c4:use rustic
  :straight t
  :defer t
  :config
  (require 'lsp-rust nil t))

;;;;;;;;;;;;;;;;;;;; Clojure ;;;;;;;;;;;;;;;;;;;;

;; Clojure 开发环境
(c4:use clojure-mode
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

;; (c4:use inf-clojure
;;   :straight t
;;   :defer t
;;   :hook clojure-mode
;;   :custom
;;   (inf-clojure-generic-cmd "lumo -d")
;;   (inf-clojure-boot-cmd "lumo -d"))

(c4:use cider
  :straight t
  :after (clojure-mode))

;;;;;;;;;;;;;;;;;;;; Emacs Lisp ;;;;;;;;;;;;;;;;;;;;

(c4:use elisp-mode
  :defer t
  :delight
  (emacs-lisp-mode ("El" (lexical-binding ":Lex" ":Dyn")))
  :mode
  ("Cask"))

(c4:use pretty-eval-last-sexp
  :custom
  (pretty-eval-last-sexp-show-results-in-log t)
  :commands
  (pretty-eval-last-sexp)
  :bind
  ("C-x C-e" . pretty-eval-last-sexp))

;;;;;;;;;;;;;;;;;;;; Lisp ;;;;;;;;;;;;;;;;;;;;

(c4:use paredit
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

(c4:use selected
  :straight t
  :defer t
  :delight
  :no-require t)
(c4:use parinfer
  :straight t
  :defer t
  :delight
  (parinfer-mode (:eval (if (boundp 'parinfer--mode)
                            (cond
                             ((eq 'paren parinfer--mode) " P:Par")
                             ((eq 'indent parinfer--mode) " P:Ind")
                             (t " P:Unknown"))
                          " P:")))
  :bind
  (:map paredit-mode-map
        (";" . self-insert-command))
  :hook
  ((clojure-mode
    clojurescript-mode
    emacs-lisp-mode
    elisp-mode
    common-lisp-mode
    scheme-mode
    lisp-mode) . parinfer-mode)
  (parinfer-mode . user/when-parinfer-mode-enabled)
  :init
  (defun user/when-parinfer-mode-enabled ()
    (parinfer--switch-to-paren-mode))
  (setq parinfer-extensions
        '(defaults        ; should be included.
           pretty-parens  ; different paren styles for different modes.
           evil           ; If you use Evil.
           paredit        ; Introduce some paredit commands.
           smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
           smart-yank)))  ; Yank behavior depend on mode.

(c4:use rainbow-delimiters
  :straight t
  :defer t
  :delight
  :hook
  ((clojurescript-mode
    clojure-mode
    scheme-mode
    common-lisp-mode
    emacs-lisp-mode
    lisp-mode) . rainbow-delimiters-mode))

(load-relative "./deps/evil")
(load-relative "./deps/origami")
(load-relative "./deps/lsp")
(load-relative "./deps/frontend-support")
(load-relative "./deps/json-support")
(load-relative "./deps/org-mode")
