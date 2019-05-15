;;; -*- lexical-binding: t -*-

(require 's)
(require 'load-relative)
(require 'ensure-system-package)

(provide-me)

(let* ((lsp-json-lsp-activate-p
        (lambda (file-name &rest args)
          (and (stringp file-name)
               (s-ends-with? ".json" file-name))))

       (json-lsp-client-registered
        nil)

       (register-json-lsp-client
        (lambda ()
          (unless json-lsp-client-registered
            (setq json-lsp-client-registered t)

            (ensure-system-package
             (vscode-json-languageserver . "yarn global add vscode-json-languageserver"))

            (require 'lsp-mode)

            (lsp-register-client
             (make-lsp-client :new-connection (lsp-stdio-connection
                                               (lambda () '("vscode-json-languageserver" "--stdio")))
                              :priority -1
                              :activation-fn lsp-json-lsp-activate-p
                              :server-id 'json-ls))))))

  (add-hook
   'find-file-hook
   #'(lambda ()
       (when (funcall lsp-json-lsp-activate-p buffer-file-name)
         (funcall register-json-lsp-client)
         (lsp)))))
