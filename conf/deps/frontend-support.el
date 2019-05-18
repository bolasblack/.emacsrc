;;; -*- lexical-binding: t -*-

(require 'init-use-package)

(provide-me)

(use-package coffee-mode
  :straight t
  :defer t
  :mode ("\\.coffee\\'"))

(use-package jade-mode
  :straight t
  :defer t
  :mode ("\\.jade\\'"))

(use-package css-mode
  :defer t
  :mode ("\\.css\\'" "\\.wxss\\'")
  :custom
  (css-indent-offset 2))

(use-package less-css-mode
  :straight t
  :defer t
  :mode ("\\.less\\'"))

(use-package sass-mode
  :straight t
  :defer t
  :mode ("\\.styl\\'"))

(use-package jsx-mode
  :straight t
  :defer t)

(use-package typescript-mode
  :straight t
  :defer t
  :delight "ts"
  :custom
  (typescript-indent-level 2))

(use-package graphql-mode
  :straight t
  :defer t
  :mode ("\\.gql\\'" "\\.graphql\\'"))

(use-package web-mode
  :straight t
  :defer t
  :mode ("\\.js\\'" "\\.jsx\\'" "\\.ts\\'" "\\.tsx\\'" "\\.erb\\'" "\\.html\\'" "\\.vue\\'" "\\.wxml\\'")
  :interpreter ("node" "nodejs" "gjs" "rhino")
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :config
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

(use-package js
  :defer t
  :custom
  (js-indent-level 2))

(use-package rainbow-mode
  :straight t
  :defer t
  :delight
  :hook (web-mode
         css-mode
         sass-mode
         scss-mode))

(use-package add-node-modules-path
  :straight t
  :defer t)

(defun prettier-js--enable-with-node-modules-path ()
  (add-node-modules-path)
  (prettier-js-mode))

(use-package prettier-js
  :straight t
  :defer t
  :hook ((typescript-mode . prettier-js--enable-with-node-modules-path)
         (javascript-mode . prettier-js--enable-with-node-modules-path)
         (html-mode . prettier-js--enable-with-node-modules-path)
         (css-mode . prettier-js--enable-with-node-modules-path)
         (web-mode . prettier-js--enable-with-node-modules-path))
  :custom
  (prettier-js-args '("--config-precedence" "prefer-file")))

(use-package tide
  :straight t
  :defer t
  :commands (tide-setup tide-hl-identifier-mode)
  :hook
  ((typescript-mode . (lambda ()
                        (yas-activate-extra-mode 'js-mode)
                        (tide-setup)
                        (tide-hl-identifier-mode)))
   (web-mode . (lambda ()
                 (let ((file-ext (file-name-extension buffer-file-name)))
                   (when (or (string-equal "tsx" file-ext)
                             (string-equal "ts" file-ext))
                     (tide-setup)
                     (tide-hl-identifier-mode)))))))

(comment
 ;; https://github.com/emacs-lsp/lsp-ui/issues/266
 (use-package typescript-lsp
   :no-require t
   :after (typescript-mode company flycheck web-mode lsp-mode)
   :hook
   ((typescript-mode . tide-hl-identifier-mode)
    (typescript-mode . (lambda ()
                         (yas-activate-extra-mode 'js-mode)
                         (lsp)))
    (web-mode . (lambda ()
                  (let ((file-ext (file-name-extension buffer-file-name)))
                    (when (or (string-equal "tsx" file-ext)
                              (string-equal "ts" file-ext))
                      (lsp))))))
   :ensure-system-package
   (typescript-language-server . "yarn global add typescript-language-server")))
