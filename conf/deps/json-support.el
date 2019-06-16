(require 's)
(require 'load-relative)
(require 'ensure-system-package)

(provide-me)

(defvar c4:json-support/json-lsp-client-registered nil)

(defun c4:json-support/lsp-json-lsp-activate-p (file-name &rest args)
  (and (stringp file-name)
       (s-ends-with? ".json" file-name)))

(defun c4:json-support/register-json-lsp-client ()
  (unless c4:json-support/json-lsp-client-registered
    (setq c4:json-support/json-lsp-client-registered t)

    (ensure-system-package
     (vscode-json-languageserver . "yarn global add vscode-json-languageserver"))

    (require 'lsp-mode)

    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection
                                       (lambda () '("vscode-json-languageserver" "--stdio")))
                      :priority -1
                      :activation-fn 'c4:json-support/lsp-json-lsp-activate-p
                      :server-id 'json-ls))))

(defun c4:json-support/find-file-hook ()
  (when (c4:json-support/lsp-json-lsp-activate-p buffer-file-name)
    (c4:json-support/register-json-lsp-client)
    (lsp)))

(add-hook 'find-file-hook 'c4:json-support/find-file-hook)
