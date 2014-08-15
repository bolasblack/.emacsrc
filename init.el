;; .emacs.d 文件地址
(defconst dir-rc "~/.emacsrc/")
;; Emacs 配置文件地址
(defconst dir-conf (concat dir-rc "conf/"))
;; Emacs 配色文件地址
(defconst dir-theme (concat dir-rc "theme/"))

(add-to-list 'load-path dir-rc)
(add-to-list 'load-path dir-conf)
(add-to-list 'load-path dir-theme)

(defun load-files (&rest files-lists)
  (dolist (files-list files-lists)
    (let ((root-path (symbol-value (car files-list)))
          (names-list (cdr files-list)))
      (dolist (name names-list)
        (load (concat root-path name))))))

(load-files
 ;; 一些小函数
 `(dir-rc "macro-lisp.el")
 `(dir-conf
   ;; 全局设定
   "global.el"
   ;; El-Get
   "el-get.el"
   ;; 快捷键设置
   "keyboard.el"
   ;; 外观设置
   "face.el"
   )
 )

