(setq tools-packages
      '(; UNOWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ranger
                                        ; OWNED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        keychain-environment
                                        ; LOCAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ))

(defun tools/pre-init-ranger ()
  (setq ranger-cleanup-on-disable t)
  (setq ranger-enter-with-minus t))

(defun tools/init-keychain-environment ()
  (use-package keychain-environment
    :init (call-interactively #'keychain-refresh-environment)))
