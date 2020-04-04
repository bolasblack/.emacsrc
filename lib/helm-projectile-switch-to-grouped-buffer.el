;;; -*- lexical-binding: t -*-

(require 's)
(require 'f)
(require 'dash)
(require 'helm)
(require 'projectile)

(provide 'helm-projectile-switch-to-grouped-buffer)

(defun c4:buffer-name (buffer)
  (s-trim (buffer-name buffer)))

(defun c4:get-project-from-buffer (buffer)
  (-find (lambda (proj)
           (projectile-project-buffer-p buffer (f-long proj)))
         projectile-known-projects))

(defun helm-projectile-switch-to-grouped-buffer ()
  (interactive)
  (let* ((buffer-groups
          (->> (buffer-list)
               (-group-by #'c4:get-project-from-buffer)
               ((lambda (groups)
                  (let* ((nil-group-idx (--find-index (not (cl-first it)) groups))
                         (cleaned-groups (-remove-at nil-group-idx groups)))
                    (-concat cleaned-groups (list (nth nil-group-idx groups))))))))

         (buffer-source
          (-map (lambda (group)
                  (helm-build-sync-source (or (cl-first group)
                                              "Unkonwn")
                    :candidates (cl-rest group)
                    :candidate-transformer
                    (lambda (buffers) (-map #'c4:buffer-name buffers))
                    :action
                    #'switch-to-buffer))
                buffer-groups)))
    (helm :sources buffer-source
          :buffer "*Switch buffer*"
          :prompt "Switch to buffer: "
          :preselect (c4:buffer-name (other-buffer (current-buffer) 1)))))
