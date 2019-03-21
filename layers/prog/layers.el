(configuration-layer/declare-layers
 '(;; WEB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   javascript
   html
   elm
   docker
   sql

   ;; DECLARATIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   json
   yaml
   graphviz
   LilyPond
   csv

   ;; PROGRAMMING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   python
   emacs-lisp
   (haskell :variables
            haskell-enable-hindent t
            haskell-completion-backend 'intero)
   ocaml
   rust
   clojure
   ))
