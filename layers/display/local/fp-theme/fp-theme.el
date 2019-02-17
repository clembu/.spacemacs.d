;;; fp-theme.el --- My theme -*- lexical-binding: t -*-

;; Copyright (C) 2019  Clement Busschaert

;; Author: Clement Busschaert <clement.busschaert@gmail.com>

(defun colir-join (r g b)
  "Build a color from R G B.
Inverse of `color-values'."
  (format "#%02x%02x%02x"
          (ash r -8)
          (ash g -8)
          (ash b -8)))

(defun colir-blend (c1 c2 &optional alpha)
  "Blend the two colors C1 and C2 with ALPHA.
C1 and C2 are in the format of `color-values'.
ALPHA is a number between 0.0 and 1.0 which corresponds to the
influence of C1 on the result."
  (setq alpha (or alpha 0.5))
  (apply #'colir-join
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          c1 c2)))

(defun col/mix (c1 c2 &optional alpha)
  (colir-blend (color-values c1) (color-values c2) alpha))

(defun w/bg (bg fg &optional weight)
  (setq weight (or weight 0.5))
  `(:background ,bg
                :foreground ,(colir-blend (color-values fg) (color-values bg) weight)))

(defmacro +alpha (fg α)
  `(lambda (bg) (col/mix ,fg bg ,α)))

(deftheme fp)

(let* ((class '((class color) (min-colors 89)))

       ;; BACKGROUNDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (bg "#263238")
       (base "#29353b")
       (bg2 "#28343a")
       (bg-light "#2f3d44")
       (bg-dark "#222d32")
       (bg-dark2 "#243035")
       (bg-sel "#00bcd4")
       (bg-info "#1de9b6")
       (bg-question "#ffea00")
       (bg-warning "#ff9100")
       (bg-error "#ff1744")
       (bg-panel "#13191c")

       ;; FOREGROUND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (fg "#cfd8dc")
       (fg2 (+alpha fg 0.87))
       (fg3 (+alpha fg 0.54))
       (fg-sel "#ffffff")
       (fg-sel2 (+alpha fg-sel 0.87))
       (fg-sel3 (+alpha fg-sel 0.7))
       (ph "#78909c")
       (ln "#03a9f4")
       (lnv "#9c27b0")
       (lnlb "#55bcea")
       (lnlbv "#b06ec2")
       (wng "#ff9800")
       (err "#f44336")
       (succ "#00e676")
       (sugg "#009688")
       (destr "#ff5252")

       ;; ACCENT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (acc "#4db6ac")
       (accl "#81c4bf")
       (acc2 (+alpha acc 0.87))
       (accl2 (+alpha accl 0.87))

       ;; EDGES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (bd (+alpha "#000000" 0.13))
       (bd-light "#212b30")
       (bd-dark (col/mix bg-dark "#000000" 0.06))
       (top-edge base)
       (sep "#141a1e") ;; PANEL SEPARATOR - Very Dark

       ;; COLORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       (red "#f44336")
       (yellow "#ffeb3b")
       (green "#4caf50")
       (blue "#2196f3"))
  (custom-theme-set-faces
   'fp

   ;; BASICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(cursor ;; Default cursor. Will be overridden by mode color
     ((,class (:background ,bg-sel :foreground ,fg))))
   `(default ;; Default face. Back ground is the buffer background.
      ((,class (:background ,bg-dark :foreground ,fg))))
   `(shadow ;; Less noticeable face
     ((,class (:foreground ,(funcall fg3 bg-dark)))))
   `(hl-line ;; Current line when highlighted
     ((,class (:background ,bg))))
   `(escape-glyph ;; Anything that corresponds to a control character.
     ;; Like, `^C' or `^@'
     ((,class (:foreground ,wng :weight bold))))
   `(region ;; Selected region.
     ((,class (:background ,(col/mix bg-sel bg-dark 0.2)))))
   `(trailing-whitespace ;; Trailing white-space when highlighted
     ((,class (:background ,wng))))

   `(error ((,class (:foreground ,err :weight bold))))
   `(success ((,class (:foreground ,succ :weight bold))))
   `(warning ((,class (:foreground ,wng :weight bold))))
   `(tooltip ((,class (:foreground ,bg-dark2 :foreground ,fg))))
   `(completions-annotations ((,class (:foreground ,fg :background ,bg-light))))

   `(vertical-border ;; Separator between windows
     ((,class (:foreground ,bd-dark))))
   ;}}}

   ;; MARGINS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(line-number ((,class (:background ,bg-dark2 :foreground ,(funcall fg3 bg-dark2)))))
   `(line-number-current-line
     ((,class (:foreground ,fg))))
   `(fringe ((,class (:background ,bg-dark2 :foreground ,fg))))
                                        ; GIT GUTTER ;;;;;;;;;;;;;;;;;;;;;;;;;;
   `(git-gutter+-added ((,class (:foreground ,succ))))
   `(git-gutter+-deleted ((,class (:foreground ,destr))))
   `(git-gutter+-modified ((,class (:foreground ,wng))))
   `(git-gutter+-separator ((,class (:foreground ,bg-sel))))
   ;}}}

   ;; MODE-LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(mode-line-buffer-id ((,class (:foreground ,acc :weight bold))))
   `(mode-line
     ((,class (:foreground ,fg
                           :background ,bg-dark
                           :box (:color ,bd-light :line-width -1 :style released-button)
                           ))
      (t :inverse-video t)))
   `(powerline-active1
     ((,class (:foreground ,bg-light
                           :background ,acc
                           :box (:color ,bd-light :line-width -1 :style released-button)
                           ))))
   `(powerline-active2
     ((,class (:foreground ,bg-light
                           :background ,acc
                           :box (:color ,bd-light :line-width -1 :style released-button)
                           ))))
                                        ; INACTIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   `(mode-line-inactive
     ((,class (:foreground ,(funcall fg3 bg-dark)
                           :background ,bg-dark
                           :box nil
                           ))))
   `(powerline-inactive1
     ((,class (:foreground ,(funcall fg3 acc)
                           :background ,(col/mix acc bg-panel 0.02)
                           :box nil))))
   `(powerline-inactive2
     ((,class (:foreground ,(funcall fg3 bg-dark)
                           :background ,(col/mix acc bg-panel 0.02)
                           :box nil))))
   ;}}}

   ;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(isearch ((,class (:background ,bg-sel :foreground ,bg-dark))))
   `(isearch-fail ((,class (:background ,bg-error :foreground ,fg))))
   `(lazy-highlight ((,class (:background ,bg-light :foreground ,ph))))
   ;}}}

   ;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(highlight ((,class (:background ,fg :foreground ,bg-light))))
   `(button ((,class (:foreground ,fg :background ,bg2 :underline t))))
   `(link ((,class (:foreground ,ln :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,lnv :underline t :weight normal))))
   `(widget-field ((,class (:foreground ,ph :background ,bg-light))))

   `(header-line ((,class (:foreground ,acc :background ,bg-panel))))
   ;}}}

   ;; FONT LOCKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
                                        ; TEXT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(font-lock-comment-face ((,class (,@(w/bg bg-dark2 fg 0.3) :italic t))))
   `(font-lock-comment-delimiter-face ((,class (,@(w/bg bg-dark2 acc 0.5) :weight bold))))
   `(font-lock-doc-face ((,class (:foreground ,accl :background ,bg))))
                                        ;}}}

                                        ; WORDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(font-lock-keyword-face ((,class (:foreground ,acc :weight bold :italic t))))
   `(font-lock-builtin-face ((,class (:foreground ,(funcall acc2 bg-dark)))))
   `(font-lock-function-name-face ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,yellow :italic t))))
                                        ;}}}

                                        ; VALUES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(font-lock-constant-face ((,class (:foreground ,green))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-type-face ((,class (:foreground ,blue))))
   `(font-lock-negation-char-face ((,class (:foreground ,green))))
                                        ;}}}

                                        ; HELPERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(font-lock-preprocessor-face ((,class (:foreground ,red))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,yellow :weight bold))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,green :weight bold))))
   `(font-lock-warning-face ((,class (:foreground ,wng :weight bold))))
                                        ;}}}
   ;}}}

   ;; COMPANY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(company-tooltip ; Company's auto-completions list background
     ((,class (:background ,(col/mix bg-info bg-dark 0.3)
                           :foreground ,(funcall fg2 bg-info)))))
   `(company-scrollbar-bg ; Company's auto-completions list scroll-bar background
     ((,class (:background ,(col/mix bg-info bg-dark 0.2)))))
   `(company-scrollbar-fg ; Company's auto-completions list scroll-bar foreground
     ((,class (:background ,(col/mix bg-info bg-dark 0.4)))))
   `(company-tooltip-selection ; Company's auto-completion selected item
     ((,class (:background ,bg-info :foreground ,bg-panel))))
   `(company-tooltip-common ; Company's auto-completions base-match
     ((,class (:foreground ,yellow))))
   `(company-tooltip-common-selection ; Company's auto-completions base-match on selection
     ((,class (:foreground ,red))))
   `(company-tooltip-annotation ; Company's auto-completions comments (e.g. snippet name)
     ((,class (:foreground ,wng))))
   `(company-tooltip-annotation-selection ; Company's auto-completions comments on selection
     ((,class (:foreground ,bg-light))))
   `(company-preview ; Company's auto-completion preview
     ((,class (:background ,acc :foreground ,bg-panel))))
   `(company-preview-common ; Company's auto-completion preview base-match
     ((,class (:foreground ,bg-panel))))
                                        ;}}}

   ;; TUAREG ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
                                        ; WORDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(tuareg-font-lock-governing-face ; Ocaml code-flow keywords
     ((,class (:foreground ,accl :weight bold))))
   `(tuareg-font-lock-constructor-face ; Ocaml type constructor
     ((,class (:foreground ,(col/mix blue fg)))))
   `(tuareg-font-lock-module-face ; Ocaml module
     ((,class (:foreground ,blue :weight bold :italic t))))
   `(tuareg-font-lock-label-face ; Ocaml named arguments
     ((,class (:foreground ,accl))))
                                        ;}}}

                                        ; SYMBOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(tuareg-font-double-colon-face ; Ocaml sequence terminator
     ((,class (:foreground ,(funcall fg2 bg-dark) :weight bold))))
   `(tuareg-font-lock-operator-face ; Ocaml operators
     ((,class (:foreground ,yellow))))
                                        ;}}}

                                        ; ERRORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(tuareg-font-lock-error-face ; Ocaml errors
     ((,class (:foreground ,fg-sel :weight bold :background ,bg-error))))
   `(tuareg-font-lock-interactive-error-face
     ((,class (:foreground ,err :weight bold))))
                                        ;}}}

                                        ; ANNOTATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(tuareg-font-lock-attribute-face ; Ocaml attribute annotations ( [@@attributes])
     ((,class (:foreground ,wng))))
   `(tuareg-font-lock-extension-node-face ; Ocaml extensions ([%%extension])
     ((,class (:foreground ,wng :weight bold))))
   `(tuareg-font-lock-infix-extension-node-face ; Ocaml extensions (let% ?)
     ((,class (:foreground ,wng))))
                                        ;}}}
   ;}}}

   ;; TREEMACS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;{{{
   `(treemacs-root-face
     ((,class (:foreground ,blue :height 140 :weight bold))))
   `(treemacs-directory-face
     ((,class (:foreground ,accl))))
   `(treemacs-term-node-face
     ((,class (:foreground ,green))))
                                        ; GIT STATES ;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;{{{
   `(treemacs-git-added-face
     ((,class (:foreground ,succ))))
   `(treemacs-git-conflict-face
     ((,class (:foreground ,err :weight bold))))
   `(treemacs-git-ignored-face
     ((,class (:foreground ,(funcall fg3 bg-dark)))))
   `(treemacs-git-modified-face
     ((,class (:foreground ,wng))))
   `(treemacs-git-renamed-face
     ((,class (:foreground ,wng :italic t :weight bold))))
   `(treemacs-git-untracked-face
     ((,class (:foreground ,(funcall acc2 bg-dark)))))
                                        ;}}}

   ;}}}
   ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'fp)
(provide 'fp-theme)

;; Local Variables:
;; origami-fold-style: triple-braces
;; End:
