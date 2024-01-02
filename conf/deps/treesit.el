(require 'init-use-package)
(require 'initvar)

(setq
 c4:treesit-language-source-alist
 '(;; see also https://github.com/tree-sitter/tree-sitter/blob/master/docs/index.md#parsers
   ;; frontend
   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
   (jsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
   (css . ("https://github.com/tree-sitter/tree-sitter-css"))
   (css-in-js . ("https://github.com/orzechowskid/tree-sitter-css-in-js"))
   (scss . ("https://github.com/serenadeai/tree-sitter-scss"))
   (html . ("https://github.com/tree-sitter/tree-sitter-html"))
   (vue . ("https://github.com/ikatyang/tree-sitter-vue"))
   (svelte . ("https://github.com/Himujjal/tree-sitter-svelte"))
   (wast . ("https://github.com/wasm-lsp/tree-sitter-wasm" nil "wast/src"))
   (wat . ("https://github.com/wasm-lsp/tree-sitter-wasm" nil "wat/src"))
   (wgsl . ("https://github.com/mehmetoguzderin/tree-sitter-wgsl"))
   (jsdoc . ("https://github.com/tree-sitter/tree-sitter-jsdoc"))

   ;; lisps
   (lisp . ("https://github.com/AbstractMachinesLab/tree-sitter-sexp"))
   (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
   (scheme . ("https://github.com/6cdh/tree-sitter-scheme"))
   (racket . ("https://github.com/6cdh/tree-sitter-racket"))
   (common-lisp . ("https://github.com/theHamsta/tree-sitter-commonlisp"))
   (clojure . ("https://github.com/sogaiu/tree-sitter-clojure"))
   (clojurescript . ("https://github.com/sogaiu/tree-sitter-clojure"))

   ;; other languages
   (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
   (go . ("https://github.com/tree-sitter/tree-sitter-go"))
   (go-mod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
   (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
   (python . ("https://github.com/tree-sitter/tree-sitter-python"))
   (solidity . ("https://github.com/JoranHonig/tree-sitter-solidity"))
   (haskell . ("https://github.com/tree-sitter/tree-sitter-haskell"))
   (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml"))
   (ocaml-interface . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "interface"))
   (lua . ("https://github.com/Azganoth/tree-sitter-lua"))

   ;; script languages
   (nix . ("https://github.com/nix-community/tree-sitter-nix"))
   (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))

   ;; DSLs
   (regex . ("https://github.com/tree-sitter/tree-sitter-regex"))
   (gitignore . ("https://github.com/shunsambongi/tree-sitter-gitignore"))
   (hcl . ("https://github.com/MichaHoffmann/tree-sitter-hcl"))
   (ledger . ("https://github.com/cbarrete/tree-sitter-ledger"))
   (beancount . ("https://github.com/zwpaper/tree-sitter-beancount"))
   (protobuf . ("https://github.com/mitchellh/tree-sitter-proto"))
   (graphql . ("https://github.com/bkegley/tree-sitter-graphql"))))

(comment c4:use treesit
         :init
         (initvar 'treesit--install-language-grammar-out-dir-history)
         (initvar 'treesit-language-source-alist)
         :config
         (add-to-list 'treesit--install-language-grammar-out-dir-history
                      (concat user-emacs-directory "treesit/"))
         (-map
          (lambda (s)
            (add-to-list 'treesit-language-source-alist s))
          c4:treesit-language-source-alist))

(c4:use treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
