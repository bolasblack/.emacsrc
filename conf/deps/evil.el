(require 'init-use-package)

(c4:use evil
  :straight t
  :defer .1
  :bind
  (:map evil-normal-state-map ("C-u" . scroll-down-command))
  :config
  (evil-mode 1))

(c4:use general
  :straight t
  :config
  (general-evil-setup))

(c4:use evil-nerd-commenter
  :straight t
  :after (general evil)
  :general
  (general-nmap :prefix ","
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line))

(c4:use evil-easymotion
  :straight t
  :after (evil)
  :custom
  (avy-style 'at-full)
  (avy-background t)
  :config
  (define-key evil-motion-state-map (kbd ",") nil)
  (evilem-default-keybindings (kbd ",,")))
