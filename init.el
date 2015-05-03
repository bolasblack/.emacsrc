;; .emacs.d 文件地址
(defconst dir-rc (expand-file-name "~/.emacsrc/"))
;; Emacs 配置文件地址
(defconst dir-conf (concat dir-rc "conf/"))
;; 自己写的 Lisp 脚本文件位置
(defconst dir-lisp (concat dir-rc "lisps/"))
;; Snippet 文件地址
(defconst dir-snippet (concat dir-rc "snippets/"))

(let ((cask-command (format "cask --path '%s' load-path" dir-rc)))
  (let ((cask-paths (split-string (shell-command-to-string cask-command) ":")))
    (dolist (path cask-paths)
      (add-to-list 'load-path path))))

(require 'cask)
(cask-initialize dir-rc)
(eval-when-compile
  (require 'use-package))

(dolist (folder `(,dir-lisp))
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

(provide 'init)
