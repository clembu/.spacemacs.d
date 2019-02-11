                                        ; INIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dotspacemacs/init ()
  "Initialization of Spacemacs core settings.
Check `dotspacemacs/get-variable-string-list' for all vars you can configure."
  (setq-default
                                        ; DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-default-font '("Hasklig"
                               :size 13
                               :weight normal
                               :width normal)
   dotspacemacs-line-numbers '(:relative t
                               :disabled-for-modes dired-mode
                                                   doc-view-mode
                                                   markdown-mode
                                                   org-mode
                                                   pdf-view-mode
                                                   text-mode)
   dotspacemacs-pretty-docs t

                                        ; REPO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-elpa-timeout 1

                                        ; HOME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-enable-server t
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

                                        ; EDITING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-editing-style '(vim :variables
                                    vim-style-visual-feedback t
                                    vim-style-remap-Y-to-y$ t)
   dotspacemacs-folding-method 'origami
   dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-large-file-size 5

                                        ; UNCHANGED ;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-emacs-leader-key  "M-m"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-leader-key        "SPC"
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)
   ))

                                        ; LAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
                                        ; LAYERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-configuration-layers '()

                                        ; PATH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")

                                        ; ADDITIONAL ;;;;;;;;;;;;;;;;;;;;;;;;;;
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   ))

                                        ; ENV ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

                                        ; UINIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration."
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  (add-to-list 'configuration-layer-elpa-archives '("melpa-stable" . "stable.melpa.org/packages/"))
  (add-to-list 'package-pinned-packages '(ensime . "melpa-stable"))
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

                                        ; UCFG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded.")


