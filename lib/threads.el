;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(provide 'threads)

;; https://github.com/clojure/clojure/blob/131c5f71b8d65169d233b03d39f7582a1a5d926e/src/clj/clojure/core.clj#L1669
(defmacro -> (x &rest forms)
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  (let* ((x x)
         (forms forms))
    (while forms
      (let* ((form (cl-first forms))
             (threaded (if (listp form)
                           `(,(cl-first form) ,x ,@(cl-rest form))
                         (list form x))))
        (setq x threaded
              forms (cl-rest forms))))
    x))

;; https://github.com/clojure/clojure/blob/131c5f71b8d65169d233b03d39f7582a1a5d926e/src/clj/clojure/core.clj#L1685
(defmacro ->> (x &rest forms)
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  (let* ((x x)
         (forms forms))
    (while forms
      (let* ((form (cl-first forms))
             (threaded (if (listp form)
                           `(,(cl-first form) ,@(cl-rest form) ,x)
                         (list form x))))
        (setq x threaded
              forms (cl-rest forms))))
    x))
