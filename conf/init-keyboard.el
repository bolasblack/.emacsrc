;; -*- Emacs-Lisp -*-
(require 'do-if)
(require 'osx-clipboard)
(require 'smart-delete)
(require 'bind-key)

(provide-me)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 按键设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 删除到行首
(bind-key "M-k" 'backward-kill-line)

;; C-w 绑定为：如果有标记则删除标记（C-@），如果没有则向前删除一词
(do-if-region 'kill-region 'backward-delete-word-or-delim)
(bind-key "C-w" 'kill-region-or-backward-delete-word-or-delim)

;; C-a 绑定为：移动光标到缩进处，如果已经在缩进处了就移动到行首
(bind-key "C-a" 'smart-move-beginning-of-line)

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

;; C-x C-m 替代 M-x
(bind-key "C-x C-m" 'execute-extended-command)

(defun backward-kill-line (pos)
  "kill to the beginning of line"
  (interactive "d")
  (beginning-of-line)
  (delete-region pos (point)))

;; From https://github.com/syl20bnr/spacemacs/blob/master/layers/better-defaults/funcs.el
(defun smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))
  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))
