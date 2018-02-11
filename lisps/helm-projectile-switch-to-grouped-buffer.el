(require 's)
(require 'dash)
(require 'helm)
(require 'projectile)

(defun self--buffer-name (buffer)
  (s-trim (buffer-name buffer)))

(defun self--get-project-from-buffer (buffer)
  (-find (lambda (proj)
           (projectile-project-buffer-p buffer (f-long proj)))
         projectile-known-projects))

(defun helm-projectile-switch-to-grouped-buffer ()
  (interactive)
  (let* ((buffer-source
          (->> (buffer-list)
               (-group-by #'self--get-project-from-buffer)
               ((lambda (groups)
                  (let* ((nil-group-idx (--find-index (not (cl-first it)) groups))
                         (cleaned-groups (-remove-at nil-group-idx groups)))
                    (-concat cleaned-groups (list (nth nil-group-idx groups))))))
               (-map (lambda (group)
                       (helm-build-sync-source (or (cl-first group)
                                                   "Unkonwn")
                         :candidates (cl-rest group)
                         :candidate-transformer
                         (lambda (buffers) (-map #'self--buffer-name buffers))
                         :action
                         #'switch-to-buffer))))))
    (helm :sources buffer-source
          :buffer "*Switch buffer*"
          :prompt "Switch to buffer: "
          :preselect (self--buffer-name (current-buffer)))))

(provide 'helm-projectile-switch-to-grouped-buffer)
