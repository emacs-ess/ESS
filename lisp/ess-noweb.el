;;; ess-noweb.el : support for Literate Data Analysis

;; Copyright (C) 1999, Mark Lunt and A.J. Rossini

;; Authors: Mark Lunt <mark.lunt@mrc-bsu.cam.ac.uk> 
;;          A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: April 18, 1999
;; Version: $Revision: 1.1 $
;; Keywords: statistical support
;; Summary: Noweb support for ESS
;; CVS: $Id: ess-noweb.el,v 1.1 1999/04/20 22:22:59 rossini Exp $

;; This file is part of ESS

 ; Requires and autoloads

(require 'noweb-mode)

;;;
;;; should be moved to ess-vars.
;;;

(defvar ess-noweb-use-font-lock t
  "Set to t if you want to use font-locking in ESS noweb buffers")

(if ess-noweb-use-font-lock
     (require 'noweb-font-lock-mode))

;;; 
;;; Code Chunk evaluation.
;;;

(defun ess-eval-chunk(vis)
  "Tangle the current chunk and send it to the inferior ESS process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (let (( temp-buffer (ess-create-temp-buffer "Tangle Buffer")))
    (noweb-tangle-chunk temp-buffer)
    (set-buffer temp-buffer)
    (ess-eval-region (point-min) (point-max) vis "Eval buffer")
    (kill-buffer temp-buffer)))

(defun ess-eval-chunk-and-go( vis )
"Tangle the current chunk, send to the ESS process and switch to its buffer.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-chunk vis)
  (ess-switch-to-ESS t))

(defun ess-eval-thread(vis)
  "Tangle all chunks in the current thread and send it to the ESS process.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (let (( temp-buffer (ess-create-temp-buffer "Tangle Buffer")))
    (noweb-tangle-current-thread temp-buffer)
    (set-buffer temp-buffer)
    (ess-eval-region (point-min) (point-max) vis "Eval buffer")
    (kill-buffer temp-buffer)))

(defun ess-eval-thread-and-go( vis )
"Tangle the all chunks in the current thread, send to ESS process and go there.
Arg has same meaning as for ess-eval-region."
  (interactive "P")
  (ess-eval-thread vis)
  (ess-switch-to-ESS t))

 ; Provide package

(provide 'ess-noweb)

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
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-inf.el ends here
