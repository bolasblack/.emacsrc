(load "~/.emacsrc/lib/dump-utils.el")

(add-to-list 'dump/stash-modes '(global-undo-tree-mode))
(add-to-list 'dump/stash-modes '(menu-bar-mode))
(add-to-list 'dump/stash-modes '(global-company-mode))
(add-to-list 'dump/stash-modes '(window-numbering-mode))
(add-to-list 'dump/stash-modes '(evil-mode))

(setq dump/mode 'dumping)
(setq c4:use/lazy-load 'nil)
(dump/after-dumped dump-init/restore-env
  (dump/restore-env)
  (global-font-lock-mode t)
  (transient-mark-mode t))
(load "~/.emacsrc/init.el")
(message "Dumping Emacs...")
(dump/save-env)
(setq dump/mode 'dumped)

(garbage-collect)
