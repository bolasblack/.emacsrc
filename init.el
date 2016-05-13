
;; .emacs.d 文件地址
(defconst dir-rc (expand-file-name "~/.emacsrc/"))
;; Emacs 配置文件地址
(defconst dir-conf (concat dir-rc "conf/"))
;; 自己写的 Lisp 脚本文件位置
(defconst dir-lisp (concat dir-rc "lisps/"))
;; 一些不是自己写的 Lisp 脚本
(defconst vendor-lisp (concat dir-rc "vendors/"))
;; Snippet 文件地址
(defconst dir-snippet (concat dir-rc "snippets/"))

(dolist (file '("lisps/prepare-package.el"))
  (load (concat dir-rc file)))

(require 'f)
(dolist (folder `(,dir-lisp ,vendor-lisp))
  (if (f-exists? folder)
      (-map 'load (f-files folder))))

(load-files
 '(dir-conf
   ;; 全局设定
   global
   ;; 一些插件的设置
   plugin
   ;; 快捷键设置
   keyboard
   ;; Mode 相关的一些设置，比如文件识别
   mode-setting
   ;; 外观设置
   face
   )
 )

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
