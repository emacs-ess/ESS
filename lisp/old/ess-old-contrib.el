;;; ess-old-contrib.el --- Various utils contributed over the years

;; Copyright (C) 1997--2012 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Created: 1 April 2016
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Code:

;; This is thanks to  Ed L Cashin <ecashin@uga.edu>, 03 Mar 2004 :
(defun ess-restore-asm-extns ()
  "Remove the S-Plus mode association for .s and .S files added by ESS.
Putting the following in ~/.emacs restores emacs' default association
between .s or .S files and assembly mode.

  (add-hook 'ess-mode-hook 'ess-restore-asm-extns)
  (add-hook 'inferior-ess-mode-hook 'ess-restore-asm-extns)"
  (interactive)
  (when (assoc "\\.[qsS]\\'" auto-mode-alist)
    (setq auto-mode-alist
          (remassoc "\\.[qsS]\\'" auto-mode-alist))
    ;; put .q extention back
    ;; (add-to-list is in xemacs and GNU emacs)
    ;; R-mode when in a R/ subdirectory, otherwise S-mode:
    (add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . R-mode))
    (add-to-list 'auto-mode-alist '("\\.q\\'" . S-mode))
    ))

(provide 'ess-old-contrib)

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

;;; ess-old-contrib.el ends here
