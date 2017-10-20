;; -*- Emacs-Lisp -*-

(defun am-intern (&rest strings)
  "`intern' use STRINGS. from dea"
  (intern
   (apply
    'concat
    (mapcar (lambda (element)
              (if (stringp element)
                  element
                (symbol-name element)))
            strings))))

;; 定义一个函数，如果表达式执行为真，则执行函数1，否则执行函数2
(defmacro do-if (exp if-do else-do &optional func-name)
  "defun a function eval if-do if exp is true else eval else-do,
default function name is \"if-do\"-or-\"else-do\"

\(fn exp 'if-do 'else-do [\"function-name\"])"
  (let* ((if-do-name (symbol-name (eval if-do)))
         (else-do-name (symbol-name (eval else-do)))
         (func-result-name (if (null func-name)
                               (am-intern (concat if-do-name "-or-" else-do-name))
                             (intern func-name))))
    `(defun ,func-result-name ()
       ,(concat "if " (format "%s" exp) " than eval `" if-do-name "', else eval `" else-do-name "'")
       (interactive)
       (if (eval ,exp)
           (call-interactively ,if-do)
         (call-interactively ,else-do)))))

(defmacro do-if-region (if-do else-do)
  "eval if-do if `mark-action' else eval else-do
See also `do-if'

\(fn 'if-do-func 'else-do-func)"
  `(do-if (symbol-value mark-active) ,if-do ,else-do))

(provide 'do-if)
