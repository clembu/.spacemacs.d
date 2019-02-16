(configuration-layer/declare-layers
 '(;; TOOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   typography
   templates
   (spell-checking :variables
                   enable-flyspell-auto-completion t)

   deft

   ;; FORMATS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (org :variables
        org-enable-org-journal-support t)
   markdown

   ;; LATEX ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   latex
   bibtex

   ))
