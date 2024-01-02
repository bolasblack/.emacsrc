(require 'init-use-package)

(c4:use evil
  :straight t
  :defer .1
  :bind
  (:map evil-normal-state-map ("C-u" . scroll-down-command))
  (:map evil-insert-state-map
        ("M-p" . evil-complete-previous)
        ("M-n" . evil-complete-next))
  :config
  (evil-mode 1)
  (defun c4:evil:remap-bind (&rest _)
    (let ((bufname (buffer-name (current-buffer))))
      (cond
       ((string= bufname "*color-rg*")
        (bind-key "RET" #'color-rg-open-file evil-normal-state-map))
       
       ((string= bufname "*xref*")
        (bind-key "RET" #'xref-goto-xref evil-normal-state-map))
       
       (t
        (bind-key "RET" #'evil-ret evil-normal-state-map)))))
  (add-hook 'window-selection-change-functions 'c4:evil:remap-bind)
  (add-hook 'window-buffer-change-functions 'c4:evil:remap-bind))

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
