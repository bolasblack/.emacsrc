;; -*- Emacs-Lisp -*-

;; * require-extensions
(defun require-extensions (action lst)
  "`require' plugins of list

\(fn 'require '(aaa bbb ccc ...))"
  (mapcar (lambda(ext)
            (funcall action ext)) lst))

;; 批量映射快捷键
;; TODO: 测试第二第三种定义方式是否完成
(defun define-keys (keymap key-defs &optional group)
  "\(define-keys global-map '((\"C-a\" 'some-command) (\"C-b\" 'another-command)))
\(define-keys global-map '(\"a\" \"b\" \"c\" ...) 'group-command)
\(define-keys global-map '(\"a\" (\"C-a\" 'some-command)) 'group-command)

See also `def-keys'."
  (let ((def-key (lambda (key-obj func-or-command)
                   (let ((key (cond ((eq (type-of key-obj) (type-of [home])) key-obj)
                                    (t (eval `(kbd ,key-obj))))))
                     (define-key keymap key func-or-command)))))
    (dolist (key-def key-defs)
      (if (listp key-def)
          (funcall def-key (car key-def) (nth 1 key-def))
        (unless (null group) 
          (funcall def-key key-def group))))))

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



(defun unindent-region ()
  (interactive)
  (save-excursion
	(if (< (point) (mark)) (exchange-point-and-mark))
	(let ((save-mark (mark)))
	  (if (= (point) (line-beginning-position)) (previous-line 1))
	  (goto-char (line-beginning-position))
	  (while (>= (point) save-mark)
		(goto-char (line-beginning-position))
		(if (= (string-to-char "\t") (char-after (point))) (delete-char 1))
		(previous-line 1)))))

;; from http://ignaciopp.wordpress.com/2009/06/17/emacs-indentunindent-region-as-a-block-using-the-tab-key/
(defun indent-block()
  (shift-region c-basic-offset)
  (setq deactivate-mark nil))

(defun unindent-block()
  (shift-region (- c-basic-offset))
  (setq deactivate-mark nil))

(defun indent-or-complete ()
  "Indent region selected as a block; if no selection present either indent according to mode,
or expand the word preceding point. "
  (interactive)
  (if mark-active
      (indent-block)
    (if (looking-at "\\>")
        (hippie-expand nil)
      (insert "\t"))))

(defun shift-region(numcols)
  "my trick to expand the region to the beginning and end of the area selected 
much in the handy way I liked in the Dreamweaver editor."
  (if (< (point) (mark))
      (if (not (bolp))
          (progn (beginning-of-line) (exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line) (exchange-point-and-mark) (beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

(defun my-unindent()
  "Unindent line, or block if it's a region selected.
When pressing Shift+tab, erase words backward (one at a time) up to the beginning of line.
Now it correctly stops at the beginning of the line when the pointer is at the first char of 
an indented line. Before the command would (unconveniently) kill all the white spaces, as 
well as the last word of the previous line."
  (interactive)
  (if mark-active
      (unindent-block)
    (progn
      (unless(bolp)
        (if (looking-back "^[ \t]*")
            (progn
              ;;"a" holds how many spaces are there to the beginning of the line
              (let ((a (length(buffer-substring-no-properties (point-at-bol) (point)))))
                (progn
                  ;; delete backwards progressively in my-tab-width steps, but without going further of the beginning of line.
                  (if (> a my-tab-width)
                      (delete-backward-char my-tab-width)
                    (backward-delete-char a)))))
          ;; delete tab and spaces first, if at least 2 exist, before removing words
          (progn
            (if (looking-back "[ \t]\\{2,\\}")
                (delete-horizontal-space)
              (backward-kill-word 1))))))))

(if (not (eq major-mode 'org-mode))
    (progn
      (define-keys global-map
        `(([backtab] my-unindent)))))






;; ** lib **

(defun eval-body (forms)
  (when (listp forms)
    (if (functionp (car forms))
        (eval-body (eval forms))
      (dolist (form forms)
        (eval-body form)))))

;; 如果是在 emacs-daemon 下，则在 after-make-frame-functions 事件后运行函数
(defmacro daemon-run (&rest body)
  "run func `after-make-frame-functions' if emacs daemon

\(fn (origin-form) (another-form) ...)"
  `(message "daemon-running")
  `(if (and (fboundp 'daemonp) (daemonp))
       (add-hook 'after-make-frame-functions
                 (function (lambda (frame)
                             (with-selected-frame frame
                               (eval-body ',body)))))
     (eval-body ',body)))

;; (print x(macroexpand '(daemon-run (blink-cursor-mode nil))))

;; (eval-body '((blink-cursor-mode nil)))
;; (print (macroexpand '(eval-body '((blink-cursor-mode nil)))))
;; (print (macroexpand '(eval-body (blink-cursor-mode nil))))

;; (listp '(blink-cursor-mode nil))
;; (car '(blink-cursor-mode nil))

;; (daemon-run (blink-cursor-mode nil))
;; (eval (blink-cursor-mode nil))
;; (listp (car '(blink-cursor-mode nil)))

(provide 'macro-lisp)
