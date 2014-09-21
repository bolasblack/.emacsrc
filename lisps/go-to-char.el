;; -*- Emacs-Lisp -*-

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
