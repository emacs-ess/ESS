;;; ess-font-lock.el -- font-lock color options

;; Copyright (C) 1997--2000 Rodney Sparapani, A.J. Rossini,
;; Martin Maechler, Kurt Hornik, and Richard M. Heiberger.

;; Author: Richard M. Heiberger <rmh@temple.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 06 Feb 2000
;; Modified: $Date: 2000/03/02 18:27:40 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: ess-font-lock.el,v 1.3 2000/03/02 18:27:40 maechler Exp $

;; Keywords: ESS, font-lock

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.



;; Code:

; Requires and autoloads

(require 'font-lock)
(require 'paren)
(if (fboundp 'show-paren-mode) (show-paren-mode 1))

;; font-lock faces are defined in /emacs/emacs-20.5/lisp/font-lock.el

;; The font-lock faces are applied to ESS buffers by
;; ess-mode.el ess-inf.el ess-trns.el ess-vars.el
;; The keywords for faces are defined in the ess[dl]*.el files.

;;; All faces can be looked at with
;;        [menu-bar] [Edit] [Text Properties] [Display Faces}


;; Richard M. Heiberger <rmh@temple.edu>
(defun ess-font-lock-rmh ()
  "Set font-lock colors to Richard Heiberger's usual choice."
  (interactive)

  (set-foreground-color "Black")
  (set-background-color "lightcyan")
  (set-face-background 'modeline "lightskyblue")
  (set-face-foreground 'modeline "midnightblue")

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (if (eq font-lock-reference-face 'font-lock-constant-face )
      (set-face-foreground 'font-lock-constant-face  "Brown")
    (set-face-foreground   'font-lock-reference-face "Brown"))
  (set-face-foreground 'font-lock-string-face "VioletRed")
  (set-face-foreground 'font-lock-type-face "Sienna")
  (set-face-foreground 'font-lock-variable-name-face "Black")
)


;; Richard M. Heiberger <rmh@temple.edu>
(defun ess-font-lock-blue ()
  "Set font-lock colors to Richard Heiberger's blue color scheme."
  (interactive)

  (set-foreground-color "Black")
  (set-background-color "LightBlue")
  (set-face-foreground 'modeline "LightBlue")
  (set-face-background 'modeline "DarkSlateBlue")

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (if (eq font-lock-reference-face 'font-lock-constant-face )
      (set-face-foreground 'font-lock-constant-face  "Brown")
    (set-face-foreground   'font-lock-reference-face "Brown"))
  (set-face-foreground 'font-lock-string-face "VioletRed")
  (set-face-foreground 'font-lock-type-face "Sienna")
  (set-face-foreground 'font-lock-variable-name-face "Black")
)


;; Richard M. Heiberger <rmh@temple.edu>
(defun ess-font-lock-wheat ()
  "Set font-lock colors to Richard Heiberger's wheat color scheme."
  (interactive)

  (set-foreground-color "Black")
  (set-background-color "Wheat")
  (set-face-foreground 'modeline "Wheat")
  (set-face-background 'modeline "Sienna")

  (set-face-foreground 'font-lock-comment-face "Firebrick")
  (set-face-foreground 'font-lock-function-name-face "Blue")
  (set-face-foreground 'font-lock-keyword-face "Purple")
  (if (eq font-lock-reference-face 'font-lock-constant-face )
      (set-face-foreground 'font-lock-constant-face "Brown")
    (set-face-foreground 'font-lock-reference-face "Brown"))
  (set-face-foreground 'font-lock-string-face "VioletRed")
  (set-face-foreground 'font-lock-type-face "Sienna")
  (set-face-foreground 'font-lock-variable-name-face "Black")
)


;; Richard M. Heiberger <rmh@temple.edu>
(defun ess-font-lock-bw ()
  "Set font-lock colors to Richard Heiberger's black and white color scheme."
  (interactive)

  (set-foreground-color "Black")
  (set-background-color "white")
  (set-face-foreground 'modeline "gray10")
  (set-face-background 'modeline "gray90")

  ;;modify-face is an interactive compiled Lisp function in `faces'.
  ;;(modify-face FACE			     FOREGROUND BACKGROUND STIPPLE BOLD-P ITALIC-P UNDERLINE-P &optional INVERSE-P FRAME)

  (modify-face 'modeline		     "gray10"	"gray90"   nil	   nil	  t	   nil	     )
  (modify-face 'font-lock-comment-face	     "black"	"white"	   nil	   nil	  t	   nil	     )
  (modify-face 'font-lock-function-name-face "black"	"white"	   nil	   t	  nil	   nil	     )
  (modify-face 'font-lock-keyword-face	     "black"	"white"	   nil	   nil	  nil	   t	     )
  (if (eq font-lock-reference-face 'font-lock-constant-face )
      (modify-face 'font-lock-constant-face  "black"    "white"    nil     t      nil      nil       )
    (modify-face 'font-lock-reference-face   "black"	"white"	   nil	   t	  nil	   nil	     ))
  (modify-face 'font-lock-string-face	     "black"	"white"	   nil	   nil	  t	   t	     )
  (modify-face 'font-lock-type-face	     "black"	"white"	   nil	   t	  t	   nil	     )
  (modify-face 'font-lock-variable-name-face "black"	"white"	   nil	   nil	  nil	   nil	     )
  (modify-face 'font-lock-builtin-face	     "black"	"white"	   nil	   t	  nil	   nil	     )
  (modify-face 'font-lock-warning-face	     "black"	"white"	   nil	   t	  nil	   nil	     )
  (modify-face 'show-paren-match-face	     "gray20"	"gray80"   nil	   t	  nil	   nil	     )
  (modify-face 'show-paren-mismatch-face     "white"	"gray40"   nil	   t	  t	   nil	     )
)


;; David Brahm <David.Brahm@fmr.com>
(defun ess-font-lock-db ()
  "Set font-lock colors to David Brahm's usual choice (leave [fore/back]ground)."
  (interactive)
  (set-face-foreground 'font-lock-comment-face	     "Firebrick")  ; #...    %...
  (set-face-foreground 'font-lock-string-face	     "SeaGreen")   ; "..."   "..."
  (set-face-foreground 'font-lock-keyword-face	     "MediumBlue") ; if	     \end
  (set-face-foreground 'font-lock-function-name-face "VioletRed")  ; talk<-  {center}
  (set-face-foreground 'font-lock-variable-name-face "Blue")	   ; xv
  (set-face-foreground 'font-lock-type-face	     "Goldenrod")  ; T,F       ?
  (if (eq font-lock-reference-face 'font-lock-constant-face )
      (set-face-foreground 'font-lock-constant-face "Magenta")	   ; <-	     {eq1}
    (set-face-foreground 'font-lock-reference-face "Magenta"))
)


(provide 'ess-font-lock)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:	  ^L ;
;;; Sections:	 ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:	 defuns, defvars, defconsts
;;;		 Random code beginning with a ;;;;* comment
;;; Local variables:
;;; mode: emacs-lisp
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-font-lock.el ends here
