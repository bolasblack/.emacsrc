(require 'init-use-package)

(provide-me)

(c4:use company
  :straight t
  :delight
  :defer .1
  :custom
  (company-minimum-prefix-length 1 "自动提示的最少字数")
  (company-backends '((company-yasnippet
                       company-files
                       company-capf
                       company-keywords
                       company-dabbrev-code
                       company-dabbrev)))
  (evil-complete-previous-func 'c4:company-complete-previous-func)
  (evil-complete-next-func 'c4:company-complete-next-func)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  (defun c4:company-complete-previous-func (&rest args)
    (company-complete))
  (defun c4:company-complete-next-func (&rest args)
    (company-complete))
  :config
  (global-company-mode t))

(c4:use company-quickhelp
  :straight t
  :delight
  :after (company)
  :if (display-graphic-p)
  :config
  (company-quickhelp-mode))

(c4:use company-echo-doc
  :after (company)
  :if (not (display-graphic-p))
  :custom
  (company-echo-doc-disable 'c4:company-echo-doc-disable)
  :init
  (defun c4:company-echo-doc-disable ()
    (-intersection (activated-minor-modes)
                   '(tide-mode
                     emacs-lisp-mode)))
  :config
  (company-echo-doc-mode))

;; 给补全选项排序
(c4:use company-prescient
  :straight t
  :after (company)
  :hook ((company-mode) . company-prescient-mode))
