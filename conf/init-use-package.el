;;; -*- lexical-binding: t -*-

(require 'init-straight)
(require 'init-melpa)

(provide-me)

(defvar use-package-verbose t)
(defvar use-package-compute-statistics t)
(defvar use-package-inject-hooks t)

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

(use-package system-packages
  :straight t
  :defer)

(use-package use-package-ensure-system-package
  :straight t
  :defer)

(use-package bind-key
  :straight t
  :defer)

(use-package delight
  :straight t
  :defer)
