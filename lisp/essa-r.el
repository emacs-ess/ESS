;;; essa-r.el -- Possible local customizations for R with ESS.

;; Copyright (C) 1997--2000	A.J. Rossini, Martin Maechler,
;;				Kurt Hornik, and Richard M. Heiberger.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2003/07/24 10:37:39 $
;; Version: $Revision: 1.6 $
;; RCS: $Id: essa-r.el,v 1.6 2003/07/24 10:37:39 stephen Exp $

;; Keywords: editing and process modes.

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
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.

;;; Code:

;; you can invoke ESS/R from emacs by typing
;;      C-u M-x essr
;; with vsize set to (for example) 40M, and nsize set to 600000.
(defalias 'essr
  (read-kbd-macro
     "C-u M-x R RET - - vsize SPC 40M SPC - - nsize SPC 600000 2*RET"))
;; "SPC" must be "=" in future versions of R (works from 0.99 on)

(defun ess-r-do-region (start end &optional message)
  "Send the current region to R via AppleScript."
  (interactive "r\nP")
  (message "Starting evaluation...")
  (do-applescript (concat 
    "try\n"
	"tell application \"R\"\n"
		"activate\n"
		"with timeout of 0 seconds\n"
			"cmd \"" (buffer-substring start end)
			"\"\n"
		"end timeout\n"
	"end tell\n"
    "end try\n"))
  (message "Finished evaluation"))

(defun ess-r-do-line ()
  "Send the current line to R via AppleScript."
  (interactive) ;; "r\nP")
  (message "Starting evaluation...")
  (save-excursion
  (let ((end (point)))
  (move-to-column 0)
  (do-applescript (concat 
    "try\n"
	"tell application \"R\"\n"
		"activate\n"
		"with timeout of 0 seconds\n"
			"cmd \"" (buffer-substring (point) end)
			"\"\n"
		"end timeout\n"
	"end tell\n"
    "end try\n"))
  ))
  (message "Finished evaluation"))

(defun ess-r-var (beg end)
  "Load the current region of numbers into an R variable.  Prompts for
a variable name.  If none is given, it uses a default variable name,
e.  BEG and END denote the region in the current buffer to be sent."
  (interactive "r")
  (save-window-excursion
    (let ( (tmp-file (make-temp-file "ess-r-var"))
 	   cmd
	   var)
      (write-region beg end tmp-file)
      
      ;; Decide on the variable name to use in R; could use completion.
      (setq var (read-string "R Variable name (default e): "))
      (if (equal var "") 
	  (setq var "e"))
      
      ;; Command to send to the R process.  Get R to delete the file
      ;; rather than Emacs in case it takes R a long time to run the
      ;; scan command.
      (setq cmd (concat var " <- scan(\""  tmp-file "\"); "
			"unlink(\"" tmp-file "\")" ))
      
      ;; Put the output from the scan command into the process buffer so
      ;; the user has a record of it.
      (ess-execute cmd 'buffer))))

 ; Provide package

(provide 'essa-r)

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
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-site.el ends here
