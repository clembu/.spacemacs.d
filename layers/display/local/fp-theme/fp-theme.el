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
   `(cursor ((,class (:background ,bg-sel :foreground ,fg))))
   `(default ((,class (:background ,bg-dark :foreground ,fg))))
   `(hl-line ((,class (:background ,bg))))
   `(fringe ((,class (:background ,bg-dark2 :foreground ,fg))))
   `(escape-glyph ((,class (:foreground ,wng :weight bold))))

   `(success ((,class (:foreground ,succ :weight bold))))
   `(warning ((,class (:foreground ,wng :weight bold))))
   `(tooltip ((,class (:foreground ,bg-dark2 :foreground ,fg))))
   `(completions-annotations ((,class (:foreground ,fg :background ,bg-light))))

   ;; ISEARCH ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   `(isearch ((,class (:background ,bg-sel :foreground ,bg-dark))))
   `(isearch-fail ((,class (:background ,bg-error :foreground ,fg))))
   `(lazy-highlight ((,class (:background ,bg-light :foreground ,ph))))

   ;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   `(highlight ((,class (:background ,fg :foreground ,bg-light))))
   `(button ((,class (:foreground ,fg :background ,bg2 :underline t))))
   `(link ((,class (:foreground ,ln :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,lnv :underline t :weight normal))))
   `(widget-field ((,class (:foreground ,ph :background ,bg-light))))

   ;; FONT LOCKS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   `(font-lock-comment-face ((,class (,@(w/bg bg-dark2 fg 0.3) :italic t))))
   `(font-lock-keyword-face ((,class (:foreground ,acc :weight bold :italic t))))
   `(font-lock-builtin-face ((,class (:foreground ,(funcall acc2 bg-dark)))))))

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
