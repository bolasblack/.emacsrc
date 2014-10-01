;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 按键设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 合并当前行和下一行，类似于 Vim 的 J 键
(bind-key "M-j" 'concat-lines)

;; 删除到行首
(bind-key "M-k" 'backward-kill-line)

;; C+w 绑定为：如果有标记则删除标记（C-@），如果没有则向前删除一词
(do-if-region 'kill-region 'backward-delete-word-or-delim)
(bind-key "C-w" 'kill-region-or-backward-delete-word-or-delim)

;; C-c C-e : 如果有标记则执行标记，否则执行最近的 S 表达式
(do-if-region 'eval-region 'eval-last-sexp)
(bind-key "C-c C-e" 'eval-region-or-eval-last-sexp)

;; 将 M+x 绑定为 C+x C+m 与 C+c C+m
(bind-key "C-x C-m" 'execute-extended-command)

;; 设置 C-x C-g 为goto-line
(bind-key "C-x C-g" 'goto-line)

;; C-% 跳转到对应标点
(bind-key "C-%" 'match-paren)

;; C-o 和 M-o 分别在下方和上方新建一行
(bind-key "C-o" 'open-next-line)
(bind-key "M-o" 'open-previous-line)

;; Super-c/v/x 对应系统的复制粘贴剪切
(bind-key "S-c" 'osx-clipboard-kill-ring-save)
(bind-key "S-v" 'osx-clipboard-yank)
(bind-key "S-x" 'osx-clipboard-cut)

;; C-c a 然后按你要找到的字母 x ，按几次就会向后找几次
(bind-key "C-c a" 'go-to-char)

;; Undo 和 Redo
(bind-key "C-/" 'undo)
(bind-key "M-/" 'undo-tree-redo)

(provide 'keyboard)
