(configuration-layer/declare-layers
 '(;; WEB ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   javascript
   html
   elm
   docker

   ;; DECLARATIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   json
   yaml
   graphviz
   LilyPond

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
