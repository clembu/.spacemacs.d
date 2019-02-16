(configuration-layer/declare-layers
 '(; QoL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   better-defaults
   helm
   (auto-completion :variables
                    auto-completion-return-key-behavior 'complete
                    auto-completion-tab-key-behavior 'complete
                    auto-completion-enable-snippets-in-popup t)
   git
   github
   (version-control :variables
                    version-control-global-margin t)

   imenu-list
   (ibuffer :variables
            ibuffer-group-buffers-by 'projects)

   (shell :variables
          shell-default-shell 'eshell)
                                        ; HELP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   debug

   syntax-checking
   dash

   ranger
   ))
