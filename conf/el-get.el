(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; El-Get 配置文件地址
(add-to-list 'el-get-recipe-path (concat dir-rc "recipes/"))

(setq el-get-sources '(
                       (:name ido-hacks
                              :after (progn
                                       (ido-mode)
                                       (setq ido-enable-flex-matching t)))
                       (:name enclose
                              :after (enclose-global-mode))
                       (:name projectile
                              :after (projectile-global-mode))
                       (:name tabbar-ruler
                              :after (progn
                                       (setq tabbar-ruler-global-tabbar t)
                                       (tabbar-install-faces)))
                       (:name emacs-color-theme-solarized
                              :after (load-theme 'solarized-dark t))
))

(setq my-packages
      (append
       ;; magit
       '(magit)
       ;; anything
       '(anything init-anything)
       ;; 各种 yasnippet
       '(yasnippet init-yasnippet)
       ;; 各种 ac
       '(auto-complete init-auto-complete auto-complete-emacs-lisp)
       ;; 各种 mode
       '(markdown-mode coffee-mode)
       ;; 各种 flymake
       '(flymake-coffee flymake-css flymake-shell)
       (mapcar 'el-get-source-name el-get-sources)
))

(el-get-cleanup my-packages)
(el-get 'sync my-packages)
