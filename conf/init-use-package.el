;;; -*- lexical-binding: t -*-

(require 'init-straight)

(provide-me)

(defvar use-package-verbose t)
(defvar use-package-compute-statistics t)
(defvar use-package-inject-hooks t)

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(require 'use-package-evil-bind)

(use-package use-package-ensure-system-package
  :straight t
  :defer t)

(use-package bind-key
  :straight t
  :defer t)

(use-package delight
  :straight t
  :defer t)
