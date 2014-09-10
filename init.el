;; .emacs.d 文件地址
(defconst dir-rc (expand-file-name "~/.emacsrc/"))
;; Emacs 配置文件地址
(defconst dir-conf (concat dir-rc "conf/"))
;; Snippet 文件地址
(defconst dir-snippet (concat dir-rc "snippets/"))

(add-to-list 'load-path dir-rc)
(add-to-list 'load-path dir-conf)

(defun load-files (&rest files-lists)
  (dolist (files-list files-lists)
    (let ((root-path (symbol-value (car files-list)))
          (names-list (cdr files-list)))
      (add-to-list 'load-path root-path)
      (dolist (name names-list)
        (load (concat (symbol-name name) ".el"))))))

(load-files
 ;; 一些小函数
 '(dir-rc macro-lisp)
 '(dir-conf
   ;; 全局设定
   global
   ;; 一些插件的设置
   plugin
   ;; 快捷键设置
   keyboard
   ;; 文件识别
   mode-mapping
   ;; 外观设置
   face
   )
 )

(provide 'init)
