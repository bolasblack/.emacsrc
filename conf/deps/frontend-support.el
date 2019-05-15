;;; -*- lexical-binding: t -*-

(require 'init-use-package)

(provide-me)

(use-package jsx-mode
  :straight t
  :defer t)

(use-package typescript-mode
  :straight t
  :defer t
  :delight "ts"
  :custom
  (typescript-indent-level 2))

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
