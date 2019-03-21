(setq display-packages
      '(;; OWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (hasklig-mode :location
                      (recipe
                       :fetcher github
                       :repo "minad/hasklig-mode"))
        ;; UNOWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; LOCAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (fp-theme :location local)
        (pretty-fonts :location local)
        ))

(defun display/init-hasklig-mode ()
  (use-package hasklig-mode :hook (haskell-mode)))

(defun display/init-fp-theme ()
  (use-package fp-theme))

(defun display/init-pretty-fonts ()
  (use-package pretty-fonts
    :config
    ;; !! This is required to avoid segfault when using emacs as daemon !!
    (spacemacs|do-after-display-system-init
     (pretty-fonts-add-hook 'prog-mode-hook pretty-fonts-fira-code-alist)
     (pretty-fonts-add-hook 'org-mode-hook  pretty-fonts-fira-code-alist)
     (pretty-fonts-setup-emojis)
     (pretty-fonts-set-fontsets-for-fira-code)
     (pretty-fonts-set-fontsets
      '(;; All-the-icons fontsets
        ("fontawesome"
         ;;                         
         #xf07c #xf0c9 #xf0c4 #xf0cb #xf017 #xf101)

        ("all-the-icons"
         ;;    
         #xe907 #xe928)

        ("github-octicons"
         ;;                               
         #xf091 #xf059 #xf076 #xf075 #xe192  #xf016 #xf071)

        ("material icons"
         ;;              
         #xe871 #xe918 #xe3e7  #xe5da
         ;;              
         #xe3d0 #xe3d1 #xe3d2 #xe3d4))))))
