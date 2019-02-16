(setq text-packages
      '(; UNOWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        deft
                                        ; OWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; LOCAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ))

(defun text/pre-init-deft ()
  (setq deft-directory '("~/notes")))
