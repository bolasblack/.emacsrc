;;; company-echo-doc.el --- Show documentation in echo area for completion candidates

;; Copyright (C) 2019, c4605

;; Author: c4605 <bolasblack@gmail.com>
;; URL: https://www.github.com/bolasblack
;; Keywords: company echoarea documentation
;; Version: 2.2.0
;; Package-Requires: ((emacs "24.3") (company "0.8.9"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;;  put (company-echo-doc-mode) in your init.el to activate
;;  `company-echo-doc-mode'.

;;; Code:
(require 'company)
(require 'cl-lib)

(defgroup company-echo-doc nil
  "Show documentation in echo area for `company-mode'"
  :group 'company)

(defcustom company-echo-doc-max-lines nil
  "When not NIL, limits the number of lines in the echo area."
  :type '(choice (integer :tag "Max lines to show in echo area")
                 (const :tag "Don't limit the number of lines shown" nil))
  :group 'company-echo-doc)

(defcustom company-echo-doc-disable (lambda () nil)
  "A function to predicate should show document in echo area or not."
  :type '(predicate)
  :group 'company-echo-doc)

(defun company-echo-doc/company-frontend (command)
  "`company-mode' front-end showing documentation in echo area."
  (pcase command
    (`post-command (company-echo-doc/show-documention))))

(defun company-echo-doc/skip-footers-backwards ()
  "Skip backwards over footers and blank lines."
  (beginning-of-line)
  (while (and (not (= (point-at-eol) (point-min)))
              (or
               ;; [back] appears at the end of the help elisp help buffer
               (looking-at-p "\\[back\\]")
               ;; [source] cider's help buffer contains a link to source
               (looking-at-p "\\[source\\]")
               (looking-at-p "^\\s-*$")))
    (forward-line -1)))

(defun company-echo-doc/goto-max-line ()
  "Go to last line to display in echo area."
  (if company-echo-doc-max-lines
      (forward-line company-echo-doc-max-lines)
    (goto-char (point-max))))

(defun company-echo-doc/docstring-from-buffer (start)
  "Fetch docstring from START."
  (goto-char start)
  (company-echo-doc/goto-max-line)
  (let ((truncated (< (point-at-eol) (point-max))))
    (company-echo-doc/skip-footers-backwards)
    (list :doc (buffer-substring-no-properties start (point-at-eol))
          :truncated truncated)))

(defun company-echo-doc/completing-read (prompt candidates &rest rest)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (car candidates))

(defun company-echo-doc/fetch-docstring (backend)
  "Fetch docstring from BACKEND."
  (let ((popup-tip-str (company-call-backend 'popup-tip-string backend)))
    (if (stringp popup-tip-str)
        (with-temp-buffer
          (insert popup-tip-str)
          (company-echo-doc/docstring-from-buffer (point-min)))
      (let ((doc (company-call-backend 'doc-buffer backend)))
        (when doc
          ;; The company backend can either return a buffer with the doc or a
          ;; cons containing the doc buffer and a position at which to start
          ;; reading.
          (let ((doc-buffer (if (consp doc) (car doc) doc))
                (doc-begin (when (consp doc) (cdr doc))))
            (with-current-buffer doc-buffer
              (company-echo-doc/docstring-from-buffer (or doc-begin (point-min))))))))))

(defun company-echo-doc/doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-echo-doc/completing-read))
    (let* ((doc-and-meta (company-echo-doc/fetch-docstring selected))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (member doc '(nil ""))
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

(defun company-echo-doc/curr-doc ()
  (let* ((selected (nth company-selection company-candidates))
         (doc (let ((inhibit-message t))
                (company-echo-doc/doc selected))))
    doc))

(defun company-echo-doc/show-documention ()
  (unless (funcall company-echo-doc-disable)
    (run-with-idle-timer
     0.1
     nil
     (lambda ()
       ;; from eldoc: https://github.com/emacs-mirror/emacs/blob/83095a89f69f92833401ee437c8e455a4834c2c6/lisp/emacs-lisp/eldoc.el#L276
       ;; In emacs 19.29 and later, and XEmacs 19.13 and later, all messages
       ;; are recorded in a log.  Do not put eldoc messages in that log since
       ;; they are Legion.
       ;; Emacs way of preventing log messages.
       (let ((message-log-max nil))
         (message (company-echo-doc/curr-doc)))))))

(defun company-echo-doc/enable ()
  (make-local-variable 'company-frontends)
  (add-to-list 'company-frontends 'company-echo-doc/company-frontend :append))

(defun company-echo-doc/disable ()
  (setq-local company-frontends (delq 'company-echo-doc/company-frontend company-frontends)))

;;;###autoload
(define-minor-mode company-echo-doc-local-mode
  "Provides documentation in echo area for `company-mode'."
  :global nil
  (if company-echo-doc-local-mode
      (company-echo-doc/enable)
    (company-echo-doc/disable)))

;;;###autoload
(define-globalized-minor-mode company-echo-doc-mode
  company-echo-doc-local-mode company-echo-doc-local-mode)

(provide 'company-echo-doc)

;;; company-echo-doc.el ends here
