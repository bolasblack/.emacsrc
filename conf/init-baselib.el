(require 'use-package)

(provide-me)

;; https://github.com/magnars/dash.el
(c4:use dash
  :straight t)

;; https://github.com/magnars/s.el
(c4:use s
  :straight t)

;; https://github.com/rejeep/f.el
(c4:use f
  :straight t)

;; https://github.com/Wilfred/ht.el
(c4:use ht
  :straight t)

(c4:use edn
  :straight t
  :defer t)

(load-relative "./baselib/mode")
