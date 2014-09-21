;; -*- Emacs-Lisp -*-

;;临时记号
;;有时你需要跳到另一个文件进行一些操作，然后很快的跳回来。你当然可以使用 bookmark 或者寄存器
;;但是这些实在是太慢了。你多想拥有vi那样的 ma, mb, 'a, 'b 的操作。现在你可以用几行 elisp 达到类似的目的
;(define-keys global-map
;  `(("C-." ska-point-to-register)
;    ("C-," ska-jump-to-register)))
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (message "point location regisered: %s" (point-to-register 100)))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 100)
        (set-register 8 tmp)))
