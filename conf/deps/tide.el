;;; -*- lexical-binding: t -*-

(require 'init-use-package)
(require 'f)

(defun c4:tide-completion-annotation (name)
  (c4:tide-completion-append-source
   (c4:tide-completion-annotation-trans-mark name)
   name))

(defun c4:tide-completion-append-source (text name)
  (-if-let* ((completion
              (get-text-property 0 'completion name))

             (raw-source
              (plist-get completion :source)))
      (tide-join (list text " " (c4:tide-normalize-source raw-source)))
    text))

(defun c4:tide-normalize-source (source)
  (-->
   source
   (if (and (stringp it)
            (file-name-absolute-p it))
       (file-relative-name it (buffer-file-name))
     it)
   (if (s-contains? "/node_modules/" it)
       (->> it
            (s-split "/node_modules/")
            (-last-item))
     it)
   (s-chop-suffix "/index" it)
   (if (s-starts-with? "@types/" it)
       (-as-> (s-chop-prefix "@types/" it) itt
              (if (s-contains? "__" itt)
                  (->> itt
                       (s-replace "__" "/")
                       (s-concat "@"))
                itt))
     it)))

(defun c4:tide-completion-annotation-trans-mark (name)
  (pcase (plist-get (get-text-property 0 'completion name) :kind)
    ("keyword" " k")
    ("module" " M")
    ("class" " C")
    ("interface" " I")
    ("type" " T")
    ("enum" " E")
    ("var" " v")
    ("local var" " v")
    ("function" " ƒ")
    ("local function" " ƒ")
    ("method" " m")
    ("getter" " m")
    ("setter" " m")
    ("property" " p")
    ("constructor" " c")
    ("call" " m")
    ("index" " i")
    ("construct" " m")
    ("parameter" " p")
    ("type parameter" " T")
    ("primitive type" " T")
    ("label" " l")
    ("alias" " A")
    ("const" " c")
    ("let" " l")
    (_ nil)))

(defun c4:tide-setup ()
  (tide-setup)
  (tide-hl-identifier-mode))

;; use eglot instead
(comment
 (c4:use tide
   :straight t
   :after (company flycheck)
   :commands (tide-setup tide-hl-identifier-mode)
   :hook
   ((typescript-mode . (lambda ()
                         (yas-activate-extra-mode 'js-mode)
                         (c4:tide-setup)))
    (web-mode . (lambda ()
                  (let ((file-ext (file-name-extension buffer-file-name)))
                    (when (or (string-equal "tsx" file-ext)
                              (string-equal "ts" file-ext))
                      (c4:tide-setup))))))
   :config
   (flycheck-remove-next-checker 'typescript-tide 'typescript-tslint)
   (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)
   (push 'web-mode (flycheck-checker-get 'typescript-tide 'modes))
   (push 'web-mode (flycheck-checker-get 'javascript-eslint 'modes))
   (fset 'tide-completion-annotation 'c4:tide-completion-annotation)
   (fset 'tide-completion-annotation-trans-mark 'c4:tide-completion-annotation-trans-mark)))
