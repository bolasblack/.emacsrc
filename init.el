;; set the default file path
(defconst dir-rc (file-name-directory (or load-file-name "")))
;; 自己写的 Lisp 库文件位置
(defconst dir-lib (concat dir-rc "lib/"))
;; Emacs 配置文件地址
(defconst dir-conf (concat dir-rc "conf/"))
;; Snippet 文件地址
(defconst dir-snippet (concat dir-rc "snippets/"))
;; flycheck 配置文件地址
(defconst dir-flycheck (concat dir-rc "flycheck.conf/"))

(add-to-list 'load-path dir-rc)
(add-to-list 'load-path dir-lib)
(add-to-list 'load-path dir-conf)

(let (
      ;; 加载的时候临时增大 `gc-cons-threshold' 以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))
  
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  
  (when (featurep 'benchmark-init)
    (require 'benchmark-init)
    (add-hook 'after-init-hook 'benchmark-init/deactivate))
  
  (require 'dump-utils)
  (dump/unless-dumping
    (dump/eval-delayed-functions))
  
  (require 'init-straight)
  (require 'init-load-relative)
  (require 'init-theme-load-hook)
  (require 'init-use-package)
  (require 'init-baselib)
  (require 'init-deps)
  (require 'init-keyboard)
  (require 'init-settings)
  (dump/unless-dumping
    (require 'init-emacs-server)))
