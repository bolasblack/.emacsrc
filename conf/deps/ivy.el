(require 'init-use-package)

;; 一种补全方式
(c4:use ivy
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
  (when (memq 'zenburn custom-enabled-themes)
    (zenburn-with-color-variables
      (set-face-attribute 'ivy-current-match nil
                          :background zenburn-bg+2)))
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
          (setq this-command 'ivy-cd))))))

;; 扩展 ivy
(c4:use counsel
  :straight t
  :after (ivy)
  :bind
  (:map counsel-find-file-map ("C-w" . ivy-backward-kill-word)))

;; 给 ivy 选项排序
(c4:use ivy-prescient
  :straight t
  :after (counsel)
  :custom
  (ivy-prescient-sort-commands
   '(:not
     ;; default value
     swiper swiper-isearch ivy-switch-buffer
     ;; in dump mode, ivy opened very slowly when calling execute-extended-command
     execute-extended-command))
  :config
  (ivy-prescient-mode))
