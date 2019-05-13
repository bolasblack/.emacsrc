;;; -*- lexical-binding: t -*-

(require 'init-use-package)

(provide-me)

(use-package jsx-mode
  :straight t
  :defer)

(use-package typescript-mode
  :straight t
  :defer
  :delight "ts"
  :custom
  (typescript-indent-level 2))

(use-package js
  :defer
  :custom
  (js-indent-level 2))

(use-package add-node-modules-path
  :straight t
  :hook ((web-mode . add-node-modules-path)))

(use-package prettier-js
  :straight t
  :after (add-node-modules-path)
  :hook ((web-mode . prettier-js-mode)
         (css-mode . prettier-js-mode))
  :custom
  (prettier-js-args '("--config-precedence" "prefer-file")))

(use-package tide
  :straight t
  :defer
  :after (typescript-mode company flycheck web-mode)
  :hook
  ((typescript-mode . tide-hl-identifier-mode)
   (typescript-mode . (lambda ()
                        (yas-activate-extra-mode 'js-mode)
                        (tide-setup)))
   (web-mode . (lambda ()
                 (let ((file-ext (file-name-extension buffer-file-name)))
                   (when (or (string-equal "tsx" file-ext)
                             (string-equal "ts" file-ext))
                     (tide-setup)))))))

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

;; json support
