(defcustom pretty-eval-last-sexp-show-results-in-log nil
  "Should still show results in *Message* buffer?")

(defvar pretty-eval-last-sexp--ov nil
  "Temp overlay object.")

(defun pretty-eval-last-sexp--hide ()
  (when (overlayp pretty-eval-last-sexp--ov)
    (delete-overlay pretty-eval-last-sexp--ov))
  (remove-hook 'pre-command-hook 'pretty-eval-last-sexp--hide t))

(defun pretty-eval-last-sexp--show (v)
  (pretty-eval-last-sexp--hide)
  (setq pretty-eval-last-sexp--ov (make-overlay (point) (point)))
  (let* ((output-str (format " ;; => %s" v))
         (output (propertize
                  output-str
                  'face
                  '(:inherit font-lock-comment-face
                             :underline nil
                             :overline nil
                             :strike-through nil
                             :box nil
                             :slant normal
                             :width normal))))
    (overlay-put
     pretty-eval-last-sexp--ov
     'before-string
     output))
  (add-hook 'pre-command-hook 'pretty-eval-last-sexp--hide nil t))

(defun pretty-eval-last-sexp--eval (eval-last-sexp-arg-internal)
  (eval-expression-get-print-arguments eval-last-sexp-arg-internal)
  (let* ((origin-message-log-max message-log-max)
         ;; from eldoc: https://github.com/emacs-mirror/emacs/blob/83095a89f69f92833401ee437c8e455a4834c2c6/lisp/emacs-lisp/eldoc.el#L276
         ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
         ;; are recorded in a log.  Do not put eldoc messages in that log since
         ;; they are Legion.
         ;; Emacs way of preventing log messages.
         (message-log-max (if pretty-eval-last-sexp-show-results-in-log
                              origin-message-log-max
                            nil)))
    (elisp--eval-last-sexp-print-value
     (eval (eval-sexp-add-defvars (elisp--preceding-sexp)) lexical-binding)
     t
     t)))

(defun pretty-eval-last-sexp (eval-last-sexp-arg-internal)
  "Evaluate sexp before point; display value in a overlay.

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger."
  (interactive "P")
  (if (null eval-expression-debug-on-error)
      (pretty-eval-last-sexp--show
       (pretty-eval-last-sexp--eval eval-last-sexp-arg-internal))
    (let ((value (let ((debug-on-error elisp--eval-last-sexp-fake-value))
                   (cons (pretty-eval-last-sexp--eval eval-last-sexp-arg-internal)
                         debug-on-error))))
      (unless (eq (cdr value) elisp--eval-last-sexp-fake-value)
        (setq debug-on-error (cdr value)))
      (pretty-eval-last-sexp--show (car value)))))

(provide 'pretty-eval-last-sexp)
