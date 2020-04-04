;;; -*- lexical-binding: t -*-
;;; Inspired from https://github.com/syl20bnr/spacemacs/blob/c3f13d03910e6ae55e5c617ce7c7bd9cb9861d70/core/core-dumper.el

(require 'cl-lib)
(provide 'dump-utils)

(defvar dump/store-vars '((load-path)
                          (gc-cons-percentage))
  "Stored env info before dump")

(defvar dump/stash-modes '()
  "Stash modes activation state and disable temporary before dump")

(defun dump/save-env ()
  (setq dump/store-vars
        (mapcar (lambda (c)
                  (let ((var (car c)))
                    (cons var
                          (when (boundp var)
                            (symbol-value var)))))
                dump/store-vars))
  (setq dump/stash-modes
        (mapcar (lambda (c)
                  (let ((mode (car c)))
                    (cons mode
                          (cond
                           ((not (boundp mode)) -1)
                           ((symbol-value mode) (progn (funcall mode -1)
                                                       t))
                           (t (progn (funcall mode t)
                                     -1))))))
                dump/stash-modes)))

(defun dump/restore-env ()
  (dolist (c dump/store-vars)
    (set (car c) (cdr c)))
  (dolist (c dump/stash-modes)
    (when (fboundp (car c))
      (funcall (car c) (cdr c)))))



(defvar dump/mode 'not-dumped
  "Dump mode, can be `not-dumped', `dumped' or `dumping'.")

(defvar dump/delayed-functions '()
  "List of function to execute once the dump file has been loaded.")

(defmacro dump/require (&rest args)
  "Require feature if dumping."
  (dump/when-dumping-strict `(require ,@args)))

(defun dump/dumping-p ()
  "Return non-nil if Emacs is dumping."
  (eq 'dumping dump/mode))

(defun dump/dumped-p ()
  "Return non-nil if Emacs is running from a dump."
  (eq 'dumped dump/mode))

(defmacro dump/when-dumping (&rest body)
  "Execute body if dumping.
This function considers that we are always dumping if dumping is not supported.
You should always use this function."
  (declare (indent defun))
  `(when (not (dump/dumped-p))
     ,@body))

(defmacro dump/when-dumping-strict (&rest body)
  "Execute body if we are really dumping.
You should not used this function, it is reserved for some specific process."
  (declare (indent defun))
  `(when (dump/dumping-p)
     ,@body))

(defmacro dump/unless-dumping (&rest body)
  "Execute body if not dumping"
  (declare (indent defun))
  `(unless (dump/dumping-p)
     ,@body))

(defmacro dump/after-dumped (funcname &rest body)
  "Execute BODY if not dumping, delay BODY evaluation after the dump is loaded.
FUNCNAME is the name of the function that will be created and evaluated at
the end of the loading of the dump file."
  (declare (indent defun))
  (let ((curr-file (when (fboundp '__FILE__) (__FILE__))))
    (cl-flet ((wrap-file-fn (body)
                            `(let ((prev-file-fn (symbol-function '__FILE__))
                                   (prev-max-lisp-eval-depth max-lisp-eval-depth)
                                   (prev-max-specpdl-size max-specpdl-size))
                               (fset '__FILE__ (lambda () (or ,curr-file (when prev-file-fn (prev-file-fn)))))
                               (setq max-lisp-eval-depth (+ 100 max-lisp-eval-depth)
                                     max-specpdl-size (+ 100 max-specpdl-size))
                               ,@body
                               (when prev-file-fn
                                 (fset '__FILE__ prev-file-fn))
                               (setq max-lisp-eval-depth prev-max-lisp-eval-depth
                                     max-specpdl-size prev-max-specpdl-size))))
      (if (dump/dumping-p)
          (let ((funcname2 (intern (format "dump//after-dump-%S" funcname))))
            `(progn
               (defun ,funcname2 nil ,(wrap-file-fn body))
               (add-to-list 'dump/delayed-functions ',funcname2)))
        (wrap-file-fn body)))))

(defun dump/eval-delayed-functions ()
  "Evaluate delayed functions."
  (dolist (func dump/delayed-functions)
    (funcall func)))



(defun dump/dump-emacs-portable (filename &optional track-referrers)
  (unless (functionp 'dump-emacs-portable)
    (error "We need emacs support `dump-emacs-portable' function"))
  (dump-emacs-portable filename track-referrers))

(defvar dump/dump-process nil
  "The process object to dump Emacs.")

(defvar dump/dump-file-path "~/.emacs.d/dumps/emacs.pdump"
  "The directory to store dump file")

(defconst dump/dump-buffer-name "*Dumper*")

(defun dump/dump-emacs ()
  "Dump emacs in a subprocess."
  (interactive)
  (when dump/dump-process
    (message "Cancel running dumping process to start a new one.")
    (delete-process dump/dump-process))
  (when-let ((buf (get-buffer dump/dump-buffer-name)))
    (with-current-buffer buf
      (erase-buffer)))
  (make-directory (file-name-directory dump/dump-file-path) t)
  (let* ((dump-file (expand-file-name dump/dump-file-path))
         (dump-file-temp (concat dump-file ".new")))
    (setq dump/dump-process
          (make-process
           :name "emacs-dumper"
           :buffer dump/dump-buffer-name
           :sentinel
           (lambda (proc event)
             (when (not (process-live-p proc))
               (if (and (eq (process-status proc) 'exit)
                        (= (process-exit-status proc) 0))
                   (with-current-buffer dump/dump-buffer-name
                     (rename-file dump-file-temp dump-file t)
                     (goto-char (point-max))
                     (insert (format "Done!\n" dump-file-temp dump-file)))
                 (with-current-buffer dump/dump-buffer-name
                   (delete-file dump-file-temp nil)
                   (goto-char (point-max))
                   (insert "Failed\n")))
               (delete-process dump/dump-process)
               (setq dump/dump-process nil)))
           :command
           (list "emacs"
                 "--batch"
                 "-l" (concat dir-rc "dump-init.el")
                 "-eval" (concat "(dump/dump-emacs-portable \"" dump-file-temp "\")"))))
    (pop-to-buffer dump/dump-buffer-name)))
