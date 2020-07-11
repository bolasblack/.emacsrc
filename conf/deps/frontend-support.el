;;; -*- lexical-binding: t -*-

(require 'init-use-package)
(require 'f)

(provide-me)

(c4:use coffee-mode
  :straight t
  :defer t
  :mode ("\\.coffee\\'"))

(c4:use jade-mode
  :straight t
  :defer t
  :mode ("\\.jade\\'"))

(c4:use css-mode
  :defer t
  :mode ("\\.css\\'" "\\.wxss\\'")
  :custom
  (css-indent-offset 2))

(c4:use less-css-mode
  :straight t
  :defer t
  :mode ("\\.less\\'"))

(c4:use sass-mode
  :straight t
  :defer t
  :mode ("\\.styl\\'"))

(c4:use rjsx-mode
  :straight t
  :defer t)

(c4:use typescript-mode
  :straight t
  :defer t
  :delight "ts"
  :custom
  (typescript-indent-level 2))

(c4:use graphql-mode
  :straight t
  :defer t
  :mode ("\\.gql\\'" "\\.graphql\\'"))

(defvar yas-activate-extra-mode-in-web-mode
  '(("css"        . css-mode)
    ("html"       . html-mode)
    ("javascript" . js-mode)
    ("jsx"        . js-mode)
    ("typescript" . typescript-mode)))

(defun c4:web-mode-indent-for-tab-command-advice (origin-fn &rest args)
  (interactive)
  (when (and (equal major-mode 'web-mode)
             (called-interactively-p 'interactive))
    (let* ((curr-lang (web-mode-language-at-pos)))
      (-reduce-from (lambda (matched-modes pair)
                      (let ((lang-name (car pair))
                            (mode (cdr pair)))
                        (if (string= curr-lang lang-name)
                            (progn
                              (yas-activate-extra-mode mode)
                              (cons mode matched-modes))
                          (progn
                            (unless (-contains? matched-modes mode)
                              (yas-deactivate-extra-mode mode))
                            matched-modes))))
                    nil yas-activate-extra-mode-in-web-mode)))
  (let ((tab-key-fn (key-binding (kbd "<tab>"))))
    ;; if yas expandable in current point, then <tab> bound function will
    ;; become `yas-expand', otherwise it is still `indent-for-tab-command'
    (if (and tab-key-fn
             (not (equal #'indent-for-tab-command tab-key-fn)))
        (call-interactively tab-key-fn)
      (call-interactively origin-fn))))

(c4:use web-mode
  :straight t
  :defer t
  :mode ("\\.js\\'"
         "\\.jsx\\'"
         "\\.ts\\'"
         "\\.tsx\\'"
         "\\.erb\\'"
         "\\.html\\'"
         "\\.vue\\'"
         "\\.wxml\\'")
  :interpreter ("node"
                "nodejs"
                "gjs"
                "rhino")
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :hook
  ((web-mode . c4:set-comment-line-break-function))
  :config
  (defun c4:set-comment-line-break-function ()
    (setq-local comment-line-break-function #'web-mode-comment-indent-new-line))
  (advice-add 'indent-for-tab-command
              :around
              'c4:web-mode-indent-for-tab-command-advice))

(c4:use js
  :defer t
  :custom
  (js-indent-level 2))

(c4:use rainbow-mode
  :straight t
  :defer t
  :delight
  :hook (web-mode
         css-mode
         sass-mode
         scss-mode))

(c4:use add-node-modules-path
  :straight t
  :defer t)

(c4:use prettier-js
  :straight t
  :defer t
  :hook ((prettier-js-mode . add-node-modules-path)
         (typescript-mode . prettier-js-mode)
         (web-mode . prettier-js-mode)
         (html-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (css-mode . prettier-js-mode))
  :custom
  (prettier-js-args '("--config-precedence" "prefer-file")))


(c4:use tide
  :straight t
  :after (typescript-mode company flycheck)
  :commands (tide-setup tide-hl-identifier-mode)
  :preface

  (defun c4:tide-mode/typescript-mode-hook ()
    (yas-activate-extra-mode 'js-mode)
    (tide-setup)
    (tide-hl-identifier-mode))
  (add-hook 'typescript-mode 'c4:tide-mode/typescript-mode-hook)

  (defun c4:tide-mode/web-mode-hook ()
    (let ((file-ext (file-name-extension buffer-file-name)))
      (when (or (string-equal "tsx" file-ext)
                (string-equal "ts" file-ext))
        (tide-setup)
        (tide-hl-identifier-mode))))
  (add-hook 'web-mode-hook 'c4:tide-mode/web-mode-hook)

  (add-hook 'flycheck-mode-hook
            (lambda ()
              (setf (flycheck-checker-get 'typescript-tide 'modes) '(web-mode typescript-mode))))

  (defun c4:tide-completion-source (name)
    (-when-let* ((response (tide-completion-entry-details name)))
      (-> response
          (plist-get :body)
          (car)
          (plist-get :source)
          (car)
          (plist-get :text))))
  (defun c4:tide-completion-annotation@around (tide-completion-annotation name)
    (let* ((tide-completion-detailed nil)
           (source (c4:tide-completion-source name))
           (old-anno (funcall tide-completion-annotation name)))
      (if source
          (format "%s %s" old-anno source)
        old-anno)))
  (advice-add 'tide-completion-annotation :around 'c4:tide-completion-annotation@around)

  :config)

(comment
 ;; https://github.com/emacs-lsp/lsp-ui/issues/266
 (c4:use typescript-lsp
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
