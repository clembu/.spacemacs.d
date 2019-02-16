(setq tools-packages
      '(; UNOWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ranger
                                        ; OWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; LOCAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ))

(defun tools/pre-init-ranger ()
  (setq ranger-cleanup-on-disable t)
  (setq ranger-enter-with-minus t))
