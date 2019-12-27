(require 'init-use-package)

(provide-me)

;; 一种补全方式
(use-package ivy
  :straight t
  :defer .1
  :delight
  :bind
  (:map ivy-minibuffer-map
        ("C-w" . ivy-backward-kill-word)
        ("TAB" . c4:ivy-tab))
  :custom
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode t)
  ;; https://honmaple.me/articles/2018/06/%E8%87%AA%E5%AE%9A%E4%B9%89helm%E5%BC%8F%E7%9A%84ivy.html#org-46223305
  (defun c4:ivy-tab ()
    (interactive)
    (let ((dir ivy--directory))
      (ivy-partial)
      (when (string= dir ivy--directory)
        (ivy-insert-current)
        (when (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
                   (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
          (ivy--cd dir)
          (setq this-command 'ivy-cd)))))
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(ivy-current-match           ((t (                              :background ,zenburn-bg-1))))
     `(ivy-remote                  ((t (:foreground ,zenburn-blue     :background ,zenburn-bg))))
     `(ivy-subdir                  ((t (:foreground ,zenburn-yellow   :background ,zenburn-bg))))
     `(ivy-minibuffer-match-face-1 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,zenburn-yellow   :background ,zenburn-blue-3 :weight bold)))))))

;; 扩展 ivy
(use-package counsel
  :straight t
  :after (ivy)
  :bind
  (:map counsel-find-file-map ("C-w" . ivy-backward-kill-word)))

;; 给 ivy 选项排序
(use-package ivy-prescient
  :straight t
  :after (counsel)
  :config
  (ivy-prescient-mode))
