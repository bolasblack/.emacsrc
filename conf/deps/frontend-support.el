;;; -*- lexical-binding: t -*-

(require 'init-use-package)
(require 'f)

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

(let ((adviced nil))
  (defun web-mode-advice-indent-for-tab-command ()
    (unless adviced
      (setq adviced t)
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
            (call-interactively (ad-get-orig-definition 'indent-for-tab-command))))))))

(use-package web-mode
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
  :config
  (web-mode-advice-indent-for-tab-command))

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

(use-package prettier-js
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
                     (tide-hl-identifier-mode))))))
  :config
  (defun tide-load-tsconfig (path loaded-paths)
    (when (member path loaded-paths)
      (error "tsconfig file has cyclic dependency, config file at %S is already loaded." path))
    (when (not (file-exists-p path))
      (error "tsconfig file not found at %S." path))
    (let ((config (tide-safe-json-read-file path)))
      (-if-let (extends (plist-get config :extends))
          (tide--get-extends-config config extends path (cons path loaded-paths))
        config))))

(defun tide--get-extends-config (config extends-path main-file-path loaded-paths)
  (let ((path extends-path)
        (main-file-folder (file-name-directory main-file-path)))
    ;; If the file in "extends" starts with `/`, `./`, `../`, then try to resolve like a relative
    ;; module, or try to resolve like a node_modules module
    (if (or (s-starts-with? "/" extends-path)
            (s-starts-with? "./" extends-path)
            (s-starts-with? "../" extends-path))
        (progn
          (setq path (f-join main-file-folder extends-path))
          ;; If the file does not exist, then slaps an extension on it, and tries to load that file.
          (unless (or (file-exists-p path)
                      (string= (file-name-extension path) "json"))
            (setq path (concat path ".json"))))
      (-if-let (extends-full-path
                (tide--find-up
                 (list
                  (f-join "node_modules" extends-path)
                  (f-join "node_modules" (concat extends-path ".json"))
                  (f-join "node_modules" extends-path "tsconfig.json"))
                 main-file-folder))
        (setq path extends-full-path)))
    ;; We don't recheck the path's existence: tide-load-tsconfig will fail if the path does not exist.
    (let* ((extension (tide-load-tsconfig path loaded-paths))
           (compiler-options (tide-combine-plists (plist-get extension :compilerOptions)
                                                  (plist-get config :compilerOptions))))
      (tide-combine-plists
       extension
       config
       `(:compilerOptions ,compiler-options)))))

(defun tide--find-up (filenames start-path)
  (let* ((found-path
          nil)
         (find-first-exists-filename-in-path
          (lambda (path)
            (-first
             (lambda (filename)
               (let ((checking-path (f-join path filename)))
                 (when (f-file? checking-path)
                   (setq found-path checking-path)))
               found-path)
             filenames))))
    (f-traverse-upwards
     find-first-exists-filename-in-path
     start-path)
    found-path))

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
