;; -*- Emacs-Lisp -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 按键设置
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 让 Ctrl + Space 无效
(global-set-key (kbd "C-SPC") 'nil)

;; 全局设定
(define-keys global-map
  `(;; C+w 绑定为：如果有标记则删除标记（C-@），如果没有则向前删除一词
    ("C-w" kill-region-or-backward-kill-word)
    ;; C-c C-e : 如果有标记则执行标记，否则执行最近的 S 表达式
    ("C-c C-e" eval-region-or-eval-last-sexp)
    ;; 设置 C+x C+u 为 Undo
    ("C-x C-u" undo)
    ;; 将 M+x 绑定为 C+x C+m 与 C+c C+m
    ("C-x C-m" execute-extended-command)
    ;; copy-lines
    ("C-x w" copy-lines)
    ;; 设置 C-x C-g 为goto-line
    ("C-x C-g" goto-line )
    ;; C-% 跳转到对应标点
    ("C-%" match-paren)
    ;; C-x C-c 在 deamon 模式时 delete-frame, 在普通模式时 save-buffers-kill-emacs
    ("C-x C-c" kill-current-emacs)
    ;; C-x C-d 删除整行
    ("C-x C-d" kill-whole-line)
    ;; 删除到行头
    ("C-S-k" kill-to-beginning)
    ;; 删除到行末
    ("C-k" kill-line)
    ;; 删除缩进
    ([C-S-iso-lefttab] kill-indent)
    ;; 中断当前行在上方开新行且自动缩进
    ("M-RET" newline-and-indent)
    ;; 中断当前行在下方开新行且自动缩进
    ("M-S-RET" open-line)
    ;; 在上方开一个新行并缩进
    ("C-S-o" open-prev-line)
    ;; 在下方开一个新行并缩进
    ("C-o" open-next-line)
))

;; 选择区域的时候执行命令1，否则命令2
(do-if-region 'kill-region 'backward-kill-word)
(do-if-region 'eval-region 'eval-last-sexp)

;; 在 deamon 模式时 delete-frame ,在普通模式时 save-buffers-kill-emacs
(do-if (and (fboundp 'daemonp) (daemonp)) 
       'delete-frame 
       'save-buffers-kill-emacs 
       "kill-current-emacs")

(defun open-next-line ()
  "Insert a newline"
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-prev-line ()
  "Insert a newline"
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun open-line (n)
  "Insert a newline and leave point before it.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
	 (do-left-margin (and (bolp) (> (current-left-margin) 0)))
	 (loc (point))
	 ;; Don't expand an abbrev before point.
	 (abbrev-mode nil))
    (newline n)
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
	     (if do-left-margin (indent-to (current-left-margin)))
	     (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)))

(defun kill-indent (old-pos)
  "kill indent of current line"
  (interactive "d")
  (back-to-indentation)
  (let* ((pos (point))
         (delta-pos (- old-pos pos)))
    (kill-to-beginning pos)
    (forward-char delta-pos)))

(defun kill-to-beginning (pos)
  "kill to the beginning of line"
  (interactive "d")
  (beginning-of-line)
  (delete-region pos (point)))

(defun match-paren (arg)
  "jump to matching paren"
  (interactive "p")
  (cond ((or (backward-char) (looking-at "[([{]") (forward-char))
         (forward-sexp))
        ((or (backward-char) (looking-at "[)]}]") (forward-char)) 
         (forward-char) 
         (backward-sexp) 
         (forward-char))
        (t (self-insert-command (or arg 1)))))

(defun copy-lines (&optional arg) 
  "copy the whole line"
  (interactive "p")
  (save-excursion 
    (beginning-of-line) 
    (set-mark (point)) 
    (if arg 
        (next-line (- arg 1))) 
    (end-of-line) 
    (kill-ring-save (mark) (point)))) 

;;这个函数是一个 vi 的 "f" 命令的替代品。vi的用户知道，vi有 一个特别好的命令 "f"。当你按 "fx",
;; x 是任意一个字符时, 光标就会移动到下一个 "x" 处。这之后只要按 ";"(分号)，光标就到再下一个 "x"。
;;举个例子说明这个命令的用途。比如我们有这样一行字，光标在 行首。
;;(setq unread-command-events (list last-input-event)))
;;                                               ^^^^^
;;我们希望迅速的到达最后那个 event 处，于是我在 vi 里按 "fe"。结果光标到了 "setq" 的那个 e 上面
;;，这时候我接着按 ";", 不一会儿就到了我们想要的地方。很方便吧？可能起初不觉得，后来 你发现这真的非常好！
;;有了这段代码之后，当你按 C-c a x (x 是任意一个字符) 时，光 标就会到下一个 x 处。再次按 x，光标就到
;;下一个 x。比如 C-c a w w w w ..., C-c a b b b b b b ...
(defun go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Example C-c a b ,the you get the first
Typing `go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
		     char)
    (search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "C-c a") 'go-to-char)

;;临时记号
;;有时你需要跳到另一个文件进行一些操作，然后很快的跳回来。你当然可以使用 bookmark 或者寄存器
;;但是这些实在是太慢了。你多想拥有vi那样的 ma, mb, 'a, 'b 的操作。现在你可以用几行 elisp 达到类似的目的
(define-keys global-map
  `(("C-." ska-point-to-register)
    ("C-," ska-jump-to-register)))
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (message "point location regisered: %s" (point-to-register 100)))
 
(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 100)
        (set-register 8 tmp)))

;; kill-ring-search
(autoload 'kill-ring-search "kill-ring-search"
  "Search the kill ring in the minibuffer."
  (interactive))
(global-set-key "\M-\C-y" 'kill-ring-search)

