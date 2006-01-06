;; Copyright (C) 2005 David Whiting, A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: David Whiting <david.whiting@ncl.ac.uk>
;; Created: 15 April 2005
;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

;; Keywords: Noweb, Literate Statistical Practice, Sweave

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;
;; Some simple functions for ESS and Sweave
;; david.whiting at ncl.ac.uk
;; Wed Sep 1 14:55:52 CEST 2004

;; I have written some very simple elisp functions that I have found
;; useful and thought others might like to see. I dabble with elisp
;; occasionally at best so there are probably better ways to do what I
;; have done, but so far this seems to work for me. There are several
;; things that are hard-coded, I use Linux and I think that this would
;; probably not work in Windows (not as it is now anyway).

;; With these functions and key bindings all I need to do is open a .Rnw
;; file and, assuming R is running, I press:

;; M-n s to Sweave the file, then
;; M-n l to run latex on the results of Sweave, then
;; M-n p to make and display a postscript file , or
;; M-n P to make and display a PDF version.

;; David  Whiting to Anthony Rossini,  Mar 30
;; On Wed, Mar 30, 2005 at 11:51:26AM +0200, A.J. Rossini wrote:
;; > I'm going to go ahead and add this stuff to the distribution tonight
;; > if you don't mind.  I'd forgotten about it!
;; It would make me very happy indeed.
;; > (however, need permission to do so).
;; Permission granted!

;; Dave
;; --
;; David Whiting
;; School of Clinical Medical Sciences, The Medical School
;; University of Newcastle upon Tyne, NE2 4HH, UK.


;;; TODO:
;;;
;;; 1. I want to be able to send ess-makeLatex a parameter to tell it
;;; the number of times to run LaTeX (to get references updated
;;; correctly).
;;;
;;; 2. Also need to add ess-makeBibtex.
;;;
;;; 3. Might be good to have a way to chain commands.
;;;

;;; Autoloads and Requires

(require 'ess-noweb)

(defun ess-makeSweave ()
   "Run Sweave on the current .Rnw file."
   (interactive)
   (save-excursion
     ;; Make sure tools is loaded.
     (let ((ess-command))
       (setq ess-command (format "library(tools)"))
       (ess-execute ess-command)
       (message "Sweaving %S" (buffer-file-name))
       (setq ess-command (format "Sweave(%S)" (buffer-file-name)))
       (ess-execute ess-command 'buffer nil nil))))


(defun ess-makeLatex ()
   "Run LaTeX on the product of Sweave()ing the current file."
   (interactive)
   (save-excursion
     (let* ((thisbuffer (buffer-name))
	    (namestem (substring (buffer-name) 0 (search ".Rnw" (buffer-name))))
	    (latex-filename (concat namestem ".tex")))
       (message "Running LaTeX ..." )
       (switch-to-buffer "*tex-output*")
       (call-process "latex" nil "*tex-output*" 1 latex-filename)
       (switch-to-buffer thisbuffer)
       (message "Finished running LaTeX" ))))


(defun ess-makePS ()
   "Create a postscript file from a dvi file (name based on the current
Sweave file buffer name) and display it with gv."
   (interactive)
   (let* ((namestem (substring (buffer-name) 0 (search ".Rnw" (buffer-name))))
	 (dvi-filename (concat namestem ".dvi")))
     (shell-command (concat "dvips -o temp.ps " dvi-filename))
     (shell-command "gv temp.ps & ")))


(defun ess-makePDF ()
   "Create a PDF file and display it with acroread."
   (interactive)
   (let* ((namestem (substring (buffer-name) 0 (search ".Rnw" (buffer-name))))
	 (tex-filename (concat namestem ".tex")))
     (shell-command (concat "pdflatex " tex-filename))
     (shell-command (concat "acroread " namestem ".pdf &"))))

(defun ess-insert-Sexpr ()
 "Insert Sexpr{} into the buffer at point."
 (interactive)
 (insert "\\Sexpr{}")
 (backward-char))


;;; Now bind some keys.
(define-key noweb-minor-mode-map "\M-ns" 'ess-makeSweave)
(define-key noweb-minor-mode-map "\M-nl" 'ess-makeLatex)
(define-key noweb-minor-mode-map "\M-np" 'ess-makePS)
(define-key noweb-minor-mode-map "\M-nP" 'ess-makePDF)
(define-key noweb-minor-mode-map "\M-nx" 'ess-insert-Sexpr)


 ; provides

(provide 'ess-swv)

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

;;; ess-swv.el ends here
