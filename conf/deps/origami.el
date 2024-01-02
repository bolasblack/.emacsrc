;;; -*- lexical-binding: t -*-
(require 'init-use-package)

(c4:use origami
  :straight t
  :defer .1
  :after (evil)
  :bind
  (:map evil-normal-state-map ("zo" . origami-open-node))
  (:map evil-normal-state-map ("zO" . origami-open-node-recursively))
  (:map evil-normal-state-map ("zc" . origami-close-node))
  (:map evil-normal-state-map ("zC" . origami-close-node-recursively))
  (:map evil-normal-state-map ("za" . origami-toggle-node))
  (:map evil-normal-state-map ("zA" . origami-recursively-toggle-node))
  :config
  (global-origami-mode t)
  (add-to-list 'origami-parser-alist '(c-style . origami-c-style-parser)))

(defun origami-switch-style ()
  (interactive)
  (ivy-read
   "Select origami style:"
   `("C style"
     "Triple braces"
     "Indent")
   :require-match t
   :action
   (lambda (style)
     (setq-local
      origami-fold-style
      (cond
       ((s-equals? style "C style") 'c-style)
       ((s-equals? style "Triple braces") 'triple-braces)))
     (origami-setup-local-vars (current-buffer)))))
