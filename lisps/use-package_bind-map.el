
(defun use-package--dotted-list-p (x)
  (not (listp (cdr x))))

(defun use-package--keep-dottted-list (x)
  (if (use-package--dotted-list-p x)
      (list x)
    x))

(defun use-package--bind-key-map-p (arg)
  (and (symbolp (car arg))
       (equal :map (car arg))
       ;; get (:map mapname ...)
       (let ((pairs (cddr arg)))
         (cl-every #'identity (mapcar #'use-package-is-sympair pairs)))))

(defun use-package--bind-key-get-command (arg)
  (cdr (nth 2 arg)))

(defun use-package--bind-map-normalize-help (name-symbol label arg)
  (cond
   ((use-package--bind-key-map-p arg)
    (list arg))
   ((and (listp arg) (listp (cdr arg)))
    (mapcar #'(lambda (x) (car (use-package--bind-map-normalize-help name-symbol label x))) arg))
   (t
    (use-package-error
     (concat label " wants config like (:map mapname (string . symbol)) or list of these")))))

(defun use-package-normalize/:bind-map (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (use-package--bind-map-normalize-help name-symbol label arg))))

(defun use-package-handler/:bind-map (name-symbol keyword args rest state &optional override)
  (let ((commands (mapcar #'use-package--bind-key-get-command args)))
    (use-package-concat
     (use-package-process-keywords name-symbol
       (use-package-sort-keywords
        (use-package-plist-maybe-put rest :defer t))
       (use-package-plist-append state :commands commands))
     `((ignore (progn
                 ,@(mapcar #'(lambda (bind-key-seq)
                               `(,(if override 'bind-keys* 'bind-keys) ,@bind-key-seq))
                           arg)))))))

(defun use-package-normalize/:bind-map* (name-symbol keyword args)
  (use-package-normalize/:bind-map name-symbol keyword args))

(defun use-package-handler/:bind-map* (name-symbol keyword args rest state)
  (use-package-handler/:bind-map name-symbol keyword args rest state t))

(mapcar #'(lambda (keyword)
            (unless (member keyword use-package-keywords)
              (setq use-package-keywords
                    (append use-package-keywords (list keyword)))))
        '(:bind-map :bind-map*))
