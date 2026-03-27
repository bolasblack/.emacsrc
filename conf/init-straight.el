;;; -*- lexical-binding: t -*-

(provide 'init-straight)

(defvar bootstrap-version)

;; 不在每次启动时跑 find(1) 检查包修改，仅在保存时检查
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; 缓存所有包的 autoload 到单一文件，减少磁盘 IO
(setq straight-cache-autoloads t)

(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-disable-compile nil)
