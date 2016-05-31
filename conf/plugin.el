;;;;;;;;;;;;;;;;;; Emacs 加强 ;;;;;;;;;;;;;;;;;;

(use-package linum
 ;; 如果一开始就激活 global-linum-mode 会导致 emacs --daemon 崩溃，无法正常启动
 ;; https://github.com/kaushalmodi/.emacs.d/issues/4
 ;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2010-07/msg00518.html
 :defer 1
 :ensure t
 :config
 ;; 调整行号栏的格式
 (setq linum-format "%3d ")
 ;; 显示行号
 (global-linum-mode t))

;; 给各个窗口编号
(use-package window-numbering
  :ensure t
  :config
  (window-numbering-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; 增强 Emacs 的帮助系统
;; http://www.emacswiki.org/emacs/HelpPlus#toc3
(use-package help-fns+ :ensure t)

;; 自动调整提示窗口的位置的尺寸
(use-package popwin
  :ensure t
  :config
  (popwin-mode t)

  ;; Config: https://github.com/m2ym/popwin-el#special-display-config
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

;; 另外一种补全方式
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(ivy-minibuffer-match-face-1 ((t (:foreground ,zenburn-green :weight bold))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,zenburn-green :weight bold))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,zenburn-green :weight bold))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,zenburn-green :weight bold))))
     `(ivy-current-match ((t :background ,zenburn-bg+1 :foreground ,zenburn-fg)))
     )))

;; 缩进辅助线
(use-package indent-guide
  :ensure t
  :defer 1
  :config
  (indent-guide-global-mode))

;; JavaScript CSS HTML 格式化
;;   npm install js-beautify -g
(use-package web-beautify :ensure t)

;; 快速跳转到界面上某个地方
(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-c C-c" . ace-jump-word-mode))

;; 让 Emacs 支持在 Shell 里自定义的 PATH
(use-package exec-path-from-shell :ensure t)

;; 按了 prefix 一段时间后底部会弹出一个小窗口显示接下来可以按的键
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; 使用 C-p C-n 时平滑滚动，而不是直接向上/下一页跳几行
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode))

;;;;;;;;;;;;;;;;;;;; Evil ;;;;;;;;;;;;;;;;;;;;
(use-package evil
  :ensure t
  :defer 1
  :bind
  (:map evil-normal-state-map ("C-u" . scroll-down-command))
  :config
  (evil-mode 1))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (eval-after-load 'evil-leader
    '(progn
       (evil-leader/set-key
         "ci" 'evilnc-comment-or-uncomment-lines
         "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
         "cc" 'evilnc-copy-and-comment-lines
         "cp" 'evilnc-comment-or-uncomment-paragraphs
         "cr" 'comment-or-uncomment-region
         "cv" 'evilnc-toggle-invert-comment-line-by-line
       ))))

(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader ","))

(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings (kbd ",")))

;;;;;;;;;;;;;;;;;;;; 编辑 ;;;;;;;;;;;;;;;;;;;;

;; 语法检查
(use-package flycheck
  :ensure t
  :config
  (use-package flycheck-cask :ensure t)
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-cask-setup)
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq flycheck-disabled-checkers '(emacs-lisp-checkdoc))))
  (add-hook 'coffee-mode-hook
            (lambda () (setq flycheck-coffeelintrc (concat dir-rc "flycheck.conf/coffee.json")))))

;; 自动标点配对（不只是标点配对）
(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  (smartparens-global-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t))

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  ;; 自动提示的最少字数
  (setq company-minimum-prefix-length 1)
  ;;(push 'company-robe  company-backends)
  (push 'company-files company-backends)
)

;; 方便的 Lisp 编辑
(use-package paredit
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      #'enable-paredit-mode))

;; snippet 引擎
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs dir-snippet))

(use-package drag-stuff :ensure t)

;; 光标移动到一个单词后会高亮所有相同的单词
(use-package auto-highlight-symbol
  :ensure t
  :config
  (auto-highlight-symbol-mode))

;; Sublime Text 的多光标模式
;; (use-package multiple-cursors
;;   :ensure t
;;   :bind
;;   (("C-c C-n" . mc/mark-next-lines)
;;    ("C-c C-p" . mc/mark-previous-lines)
;;    ("C-c n" . mc/mark-next-like-this)
;;    ("C-c p" . mc/mark-previous-like-this)
;;    ("C-c h" . mc/mark-all-like-this)))

;; 便捷选区
(use-package expand-region :ensure t)

;; 自动调整缩进
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'jade-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'coffee-mode))

;;;;;;;;;;;;;;;;;;;; 项目 ;;;;;;;;;;;;;;;;;;;;

;; CtrlP
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (setq projectile-completion-system 'ivy)
  :bind
  (:map evil-normal-state-map
   ("C-p" . projectile-find-file)))

;; 显示对比上次 commit 做了些什么修改
(use-package git-gutter
  :ensure t
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
   '(git-gutter:unchanged-sign " "))
)

;;;;;;;;;;;;;;;;;;;; 其他文件的支持 ;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode :ensure t)
(use-package coffee-mode :ensure t)
(use-package jade-mode :ensure t)
(use-package sass-mode
  :ensure t
  :mode "\\.styl\\'")
(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'"
  :interpreter "lua"
  :config
  (setq lua-indent-level 2))
(use-package less-css-mode :ensure t)
(use-package gitignore-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package nginx-mode :ensure t)
(use-package jsx-mode :ensure t)
(use-package apples-mode ;; AppleScript
  :ensure t
  :mode "\\.applescript\\'"
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
;; 编辑 html 模板时执行多种高亮的主模式
(use-package web-mode
  :ensure t
  :mode ("\\.js[x]?\\'" "\\.erb\\'")
  :config
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))

;;;;;;;;;;;;;;;;;;;; 开发环境 ;;;;;;;;;;;;;;;;;;;;

;; Common Lisp 开发环境
;; (use-package slime
;;   :ensure t
;;   :init
;;   (setq inferior-lisp-program (or (getenv "LISP_PROGRAM") "clisp"))
;;   :config
;;   (slime-setup '(slime-fancy slime-company)))
;; (use-package slime-company
;;   :ensure t)

;; Ruby 开发环境
;; (use-package robe
;;   :ensure t
;;   :config
;;   (add-hook 'ruby-mode-hook 'robe-mode))
;; (use-package inf-ruby
;;   :ensure t)

;; Clojure 开发环境
;; (use-package cider
;;   :ensure t
;;   :pin melpa)
