(setq display-packages
      '(;; OWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (hasklig-mode :location
                      (recipe
                       :fetcher github
                       :repo "minad/hasklig-mode"))
        ;; UNOWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; LOCAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (fp-theme :location local)
        ))

(defun display/init-hasklig-mode ()
  (use-package hasklig-mode :hook (haskell-mode)))

(defun display/init-fp-theme ()
  (use-package fp-theme))
