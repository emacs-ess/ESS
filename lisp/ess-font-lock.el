;;; ess-font-lock.el --- font-lock color options

;; Copyright (C) 2000--2006 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: 06 Feb 2000

;; Keywords: languages, faces

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; provides syntax highlighting support.

;;; Code:

                                        ; Requires and autoloads

(require 'font-lock)
(require 'paren)
(if (fboundp 'show-paren-mode) (show-paren-mode 1))

;;; Emacs 20.x notes:

;; font-lock faces are defined in /emacs/emacs-20.5/lisp/font-lock.el
;; The font-lock faces are applied to ESS buffers by
;; ess-mode.el ess-inf.el ess-trns.el ess-custom.el
;; The keywords for faces are defined in the ess[dl]*.el files.
;; All faces can be looked at, under Emacs 20.x, with
;;        [menu-bar] [Edit] [Text Properties] [Display Faces}

;;; For XEmacs

;; ... (tony needs to write something here).

(defun ess-font-lock-rmh ()
  "Set font-lock colors to Richard Heiberger's usual choice."
  (interactive)

  (if (featurep 'xemacs) nil
    (set-foreground-color "Black")
    (set-background-color "lightcyan"))

  (set-face-background 'mode-line "lightskyblue")
  (set-face-foreground 'mode-line "midnightblue")

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (set-face-foreground 'font-lock-constant-face "Brown")
  (set-face-foreground 'font-lock-string-face "VioletRed")
  (set-face-foreground 'font-lock-type-face "Sienna")
  (set-face-foreground 'font-lock-variable-name-face "Black"))

(defun ess-font-lock-blue ()
  "Set font-lock colors to Richard Heiberger's blue color scheme."
  (interactive)

  (if (featurep 'xemacs) nil
    (set-foreground-color "Black")
    (set-background-color "LightBlue"))

  (set-face-foreground 'mode-line "LightBlue")
  (set-face-background 'mode-line "DarkSlateBlue")

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (set-face-foreground 'font-lock-constant-face "Brown")
  (set-face-foreground 'font-lock-string-face "VioletRed")
  (set-face-foreground 'font-lock-type-face "Sienna")
  (set-face-foreground 'font-lock-variable-name-face "Black"))

(defun ess-font-lock-wheat ()
  "Set font-lock colors to Richard Heiberger's wheat color scheme."
  (interactive)

  (if (featurep 'xemacs) nil
    (set-foreground-color "Black")
    (set-background-color "Wheat"))

  (set-face-foreground 'mode-line "Wheat")
  (set-face-background 'mode-line "Sienna")

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (set-face-foreground 'font-lock-constant-face "Brown")
  (set-face-foreground 'font-lock-string-face "VioletRed")
  (set-face-foreground 'font-lock-type-face "Sienna")
  (set-face-foreground 'font-lock-variable-name-face "Black"))


(defun ess-font-lock-bw ()
  "Set font-lock colors to Richard Heiberger's black and white color scheme."
  (interactive)

  (if (featurep 'xemacs) nil
    (set-foreground-color "Black")
    (set-background-color "white"))

  (set-face-foreground 'mode-line "gray10")
  (set-face-background 'mode-line "gray90")

  ;; modify-face is an interactive compiled Lisp function in `faces'.
  ;; Sample usage:

  ;;(modify-face FACE                        FOREGROUND BACKGROUND STIPPLE BOLD-P ITALIC-P UNDERLINE-P &optional INVERSE-P FRAME)

  (modify-face 'mode-line                     "gray10"   "gray90"   nil     nil    t        nil       )
  (modify-face 'font-lock-comment-face       "black"    "white"    nil     nil    t        nil       )
  (modify-face 'font-lock-function-name-face "black"    "white"    nil     t      nil      nil       )
  (modify-face 'font-lock-keyword-face       "black"    "white"    nil     nil    nil      t         )
  (modify-face 'font-lock-constant-face      "black"    "white"    nil     t      nil      nil       )
  (modify-face 'font-lock-string-face        "black"    "white"    nil     nil    t        t         )
  (modify-face 'font-lock-type-face          "black"    "white"    nil     t      t        nil       )
  (modify-face 'font-lock-variable-name-face "black"    "white"    nil     nil    nil      nil       )
  (modify-face 'font-lock-builtin-face       "black"    "white"    nil     t      nil      nil       )
  (modify-face 'font-lock-warning-face       "black"    "white"    nil     t      nil      nil       )
  (modify-face 'show-paren-match-face        "gray20"   "gray80"   nil     t      nil      nil       )
  (modify-face 'show-paren-mismatch-face     "white"    "gray40"   nil     t      t        nil       ))

(defun ess-font-lock-db ()
  "Set font-lock colors (leave fore-/back-ground alone) courtesy David Brahm <David.Brahm@fmr.com>"
  (interactive)
  (set-face-foreground 'font-lock-comment-face       "Firebrick")  ; #...    %...
  (set-face-foreground 'font-lock-string-face        "SeaGreen")   ; "..."   "..."
  (set-face-foreground 'font-lock-keyword-face       "MediumBlue") ; if      \end
  (set-face-foreground 'font-lock-function-name-face "VioletRed")  ; talk<-  {center}
  (set-face-foreground 'font-lock-variable-name-face "Blue")       ; xv
  (set-face-foreground 'font-lock-type-face          "Goldenrod")  ; T,F       ?
  (set-face-foreground 'font-lock-constant-face      "Magenta")    ; <-      {eq1}
  )

(provide 'ess-font-lock)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment
;;; Local variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-font-lock.el ends here
