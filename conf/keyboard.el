;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 按键设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 删除到行首
(bind-key "M-k" 'backward-kill-line)

;; C+w 绑定为：如果有标记则删除标记（C-@），如果没有则向前删除一词
(do-if-region 'kill-region 'backward-delete-word-or-delim)
(bind-key "C-w" 'kill-region-or-backward-delete-word-or-delim)

;; C-c C-e : 如果有标记则执行标记，否则执行最近的 S 表达式
(do-if-region 'eval-region 'eval-last-sexp)
(bind-key "C-c C-e" 'eval-region-or-eval-last-sexp)

;; 设置 C-x C-g 为 goto-line
(bind-key "C-x C-g" 'goto-line)

;; Super-c/v/x 对应系统的复制粘贴剪切
(bind-key "S-c" 'osx-clipboard-kill-ring-save)
(bind-key "S-v" 'osx-clipboard-yank)
(bind-key "S-x" 'osx-clipboard-cut)

;; Undo 和 Redo
(bind-key "C-/" 'undo)
(bind-key "M-/" 'undo-tree-redo)
