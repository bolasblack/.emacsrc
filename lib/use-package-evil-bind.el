;; https://emacs-china.org/t/topic/3927/3
;;
;; Usage:
;;
;; (use-package xxx
;;   :evil-bind
;;   (normal xx-mode-map (kbd "key") 'fn))

(provide 'use-package-evil-bind)

(add-to-list 'use-package-keywords :evil-bind t)

(defalias 'use-package-normalize/:evil-bind 'use-package-normalize-forms)

(defun use-package-handler/:evil-bind (name keyword arg rest state)
  (let* ((body (use-package-process-keywords name rest state))
         (name-symbol (use-package-as-symbol name))
         (config-body
          (use-package-concat
           (use-package-hook-injector (symbol-name name-symbol)
                                      :config `((evil-define-key ',(caar arg) ,@(cdar arg))))
           body
           (list t))))
    config-body))
