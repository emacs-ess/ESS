;;; ess-swv.el --- Some simple functions for ESS and Sweave

;; Copyright (C) 2005 David Whiting, A.J. Rossini, Rich M. Heiberger, Martin
;;     Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2006-2008 A.J. Rossini, Rich M. Heiberger, Martin Maechler,
;;     Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: David Whiting <david.whiting@ncl.ac.uk>
;; Created: 15 April 2005
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: statistics, tools

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
;;; 1. I want to be able to send ess-swv-latex a parameter to tell it
;;; the number of times to run LaTeX (to get references updated
;;; correctly).
;;;
;;; 2. Also need to add ess-swv-Bibtex.
;;;
;;; 3. Might be good to have a way to chain commands.
;;;
;;; 4. ADD to the ../doc/ess.texi !!


;;; Code:

;;; Autoloads and Requires

(eval-when-compile
  (require 'ess-custom)
  (require 'ess)
  )
(require 'noweb-mode)
(require 'ess-r-d); for Rnw-mode
(require 'easymenu)

(defun ess-swv-run-in-R (cmd &optional choose-process)
  "Run \\[cmd] on the current .Rnw file.  Utility function not called by user."
  (let* ((rnw-buf (current-buffer)))
    (if choose-process ;; previous behavior
        (ess-force-buffer-current "R process to load into: ")
      ;; else
      (update-ess-process-name-list)
      (cond ((= 0 (length ess-process-name-list))
             (message "no ESS processes running; starting R")
             (sit-for 1); so the user notices before the next msgs/prompt
             (R)
             (set-buffer rnw-buf)
             )
            ((not (string= "R" (ess-make-buffer-current))); e.g. Splus, need R
             (ess-force-buffer-current "R process to load into: "))
            ))

    (save-excursion
      (ess-execute (format "require(tools)")) ;; Make sure tools is loaded.
      (basic-save-buffer); do not Sweave/Stangle old version of file !
      (let* ((sprocess (get-ess-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (rnw-file (buffer-file-name))
             (Rnw-dir (file-name-directory rnw-file))
             (Sw-cmd
              (format
               "local({..od <- getwd(); setwd(%S); %s(%S); setwd(..od) })"
               Rnw-dir cmd rnw-file))
             )
        (message "%s()ing %S" cmd rnw-file)
        (ess-execute Sw-cmd 'buffer nil nil)
        (switch-to-buffer rnw-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))

(defun ess-swv-tangle ()
  "Run Stangle on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "Stangle"))

(defun ess-swv-weave ()
  "Run Sweave on the current .Rnw file."
  (interactive)
  (ess-swv-run-in-R "Sweave"))

(defun ess-swv-latex ()
  "Run LaTeX on the product of Sweave()ing the current file."
  (interactive)
  (save-excursion
    (let* ((namestem (file-name-sans-extension (buffer-file-name)))
           (latex-filename (concat namestem ".tex"))
           (tex-buf (get-buffer-create " *ESS-tex-output*")))
      (message "Running LaTeX on '%s' ..." latex-filename)
      (switch-to-buffer tex-buf)
      (call-process "latex" nil tex-buf 1 latex-filename)
      (switch-to-buffer (buffer-name))
      (display-buffer tex-buf)
      (message "Finished running LaTeX" ))))


(defun ess-swv-PS ()
  "Create a postscript file from a dvi file (name based on the current
Sweave file buffer name) and display it."
  (interactive)
  (let* ((buf (buffer-name))
         (namestem (file-name-sans-extension (buffer-file-name)))
         (dvi-filename (concat namestem ".dvi"))
         (psviewer (ess-get-ps-viewer)))
    (shell-command (concat "dvips -o temp.ps " dvi-filename))
    (shell-command (concat psviewer " temp.ps & "))
    (switch-to-buffer buf)
    ))

(defun ess-swv-PDF (&optional pdflatex-cmd)
  "From LaTeX file, create a PDF (via 'texi2pdf' or 'pdflatex', ...), by
default using the first entry of `ess-swv-pdflatex-commands' and display it."
  (interactive
   (list
    (let ((def (elt ess-swv-pdflatex-commands 0)))
      (ess-completing-read  "pdf latex command"
                            ess-swv-pdflatex-commands ; <- collection to choose from
                            nil 'confirm ; or 'confirm-after-completion
                            nil nil def))))
  (let* ((buf (buffer-name))
         (namestem (file-name-sans-extension (buffer-file-name)))
         (latex-filename (concat namestem ".tex"))
         (tex-buf (get-buffer-create " *ESS-tex-output*"))
         (pdfviewer (ess-get-pdf-viewer))
         (pdf-status)
         (cmdstr-win (format "start \"%s\" \"%s.pdf\""
                             pdfviewer namestem))
         (cmdstr (format "\"%s\" \"%s.pdf\" &" pdfviewer namestem)))
    ;;(shell-command (concat "pdflatex " latex-filename))
    (message "Running '%s' on '%s' ..." pdflatex-cmd latex-filename)
    (switch-to-buffer tex-buf)
    (setq pdf-status
          (call-process pdflatex-cmd nil tex-buf 1
                        (if (string= "texi2" (substring pdflatex-cmd 0 5))
                            ;; workaround (bug?): texi2pdf or texi2dvi *fail* to work with full path:
                            (file-name-nondirectory latex-filename)
                          latex-filename)))
    (if (not (= 0 pdf-status))
        (message "** OOPS: error in '%s' (%d)!" pdflatex-cmd pdf-status)
      ;; else: pdflatex probably ok
      (shell-command
       (concat (if (and ess-microsoft-p (w32-shell-dos-semantics))
                   cmdstr-win
                 cmdstr))))
    (switch-to-buffer buf)
    (display-buffer tex-buf)))


(defun ess-insert-Sexpr ()
  "Insert Sexpr{} into the buffer at point."
  (interactive)
  (insert "\\Sexpr{}")
  (backward-char))


;;; back-compatible wrappers:
(defun ess-makeSweave () "old *DEPRECATED* version of \\[ess-swv-weave]."
  (interactive) (ding)
  (message
   "** warning: ess-makeSweave is deprecated. Do use (ess-swv-weave) instead!")
  (ess-swv-weave))

(defun ess-makeLatex () "old *DEPRECATED* version of \\[ess-swv-latex]."
  (interactive) (ding)
  (message
   "** warning: ess-makeLatex is deprecated. Do use (ess-swv-latex) instead!")
  (ess-swv-latex))

(defun ess-makePS () "old *DEPRECATED* version of \\[ess-swv-PS]."
  (interactive) (ding)
  (message
   "** warning: ess-makePS is deprecated. Do use (ess-swv-PS) instead!")
  (ess-swv-PS))

(defun ess-makePDF () "old *DEPRECATED* version of \\[ess-swv-PDF]."
  (interactive) (ding)
  (message
   "** warning: ess-makePDF is deprecated. Do use (ess-swv-PDF) instead!")
  (ess-swv-PDF))


;; AUCTeX integration.  This is independent of this library, but it fits
;; here nonetheless since it's an alternative way of Sweave'ing without
;; starting iESS.

(defun ess-swv-add-TeX-commands ()
  "Add commands to AUCTeX's \\[TeX-command-list]."
  (unless (and (featurep 'tex-site) (featurep 'tex))
    (error "AUCTeX does not seem to be loaded"))
  (add-to-list 'TeX-command-list
               '("Sweave" "R CMD Sweave %t"
                 TeX-run-command nil (latex-mode) :help
                 "Run Sweave") t)
  (add-to-list 'TeX-command-list
               '("LaTeXSweave" "%l %(mode) %s"
                 TeX-run-TeX nil (latex-mode) :help
                 "Run LaTeX after Sweave") t)
  (setq TeX-command-default "Sweave")
  (mapc (lambda (suffix)
          (add-to-list 'TeX-file-extensions suffix))
        '("nw" "Snw" "Rnw")))

(defun ess-swv-remove-TeX-commands (x)
  "Helper function: check if car of X is one of the Sweave strings"
  (let ((swv-cmds '("Sweave" "LaTeXSweave")))
    (unless (member (car x) swv-cmds) x)))

(defun ess-swv-plug-into-AUCTeX ()
  "Add commands to AUCTeX's \\[TeX-command-list] to sweave the current noweb
file and latex the result."
  (if ess-swv-plug-into-AUCTeX-p
      (add-hook 'Rnw-mode-hook 'ess-swv-add-TeX-commands)
    (remove-hook 'Rnw-mode-hook 'ess-swv-add-TeX-commands)
    (setq TeX-command-list (mapcar 'ess-swv-remove-TeX-commands TeX-command-list)
          ;; this will remove the items, leaving nils, so remove them.
          TeX-command-list (delq nil TeX-command-list))))
;; as ess-swv-plug-into-AUCTeX-p is customizable ... :
(if ess-swv-plug-into-AUCTeX-p
    (eval-after-load "tex" '(ess-swv-plug-into-AUCTeX)))

(defun ess-swv-toggle-plug-into-AUCTeX ()
  "Toggle inclusion of commands to sweave noweb files and latex the results in
\\[TeX-command-list] on and off.  Commands are added via \\[Rnw-mode-hook]."
  (interactive)
  (unless (and (featurep 'tex-site) (featurep 'tex))
    (error "AUCTeX are not available"))
  (setq ess-swv-plug-into-AUCTeX-p (not ess-swv-plug-into-AUCTeX-p))
  (ess-swv-plug-into-AUCTeX)
  (TeX-normal-mode t)
  (if ess-swv-plug-into-AUCTeX-p
      (message "Sweave and LaTeXSweave are activated in AUCTeX.")
    (message "Sweave and LaTeXSweave are de-activated in AUCTeX.")))


;;; Now bind some keys.
(define-key noweb-minor-mode-map "\M-ns" 'ess-swv-weave)
(define-key noweb-minor-mode-map "\M-nT" 'ess-swv-tangle)
(define-key noweb-minor-mode-map "\M-nl" 'ess-swv-latex)
(define-key noweb-minor-mode-map "\M-np" 'ess-swv-PS)
(define-key noweb-minor-mode-map "\M-nP" 'ess-swv-PDF)

(define-key noweb-minor-mode-map "\M-nx" 'ess-insert-Sexpr)

;; AND add these to the noweb menu we have anyway ! :
(easy-menu-define ess-swv-menu
  noweb-minor-mode-menu
  "Submenu for use in `Rnw-mode'."

  '("Sweaving, Tangling, ..."
    ["Sweave" ess-swv-weave   t]
    ["Tangle" ess-swv-tangle  t]
    ["LaTeX"  ess-swv-latex   t]
    ["PDF(LaTeX)" ess-swv-PDF t]
    ["PS (dvips)" ess-swv-PS  t]
    ["Insert Sexpr" ess-insert-Sexpr t]
    ["AUCTeX Interface" ess-swv-toggle-plug-into-AUCTeX
     :style toggle :selected ess-swv-plug-into-AUCTeX-p]
    ))

(if (featurep 'xemacs)
    (add-hook 'Rnw-mode-hook
              (lambda ()
                ;; This adds to top menu:
                ;; (easy-menu-add ess-swv-menu noweb-minor-mode-map)
                ;; But that's using an unnecessary extra level -- FIXME
                (easy-menu-add-item noweb-minor-mode-menu
                                    '("Sweave");; 'nil' adds to top
                                    ess-swv-menu)))
  ;; normal GNU Emacs:
  (easy-menu-add-item noweb-minor-mode-menu
                      nil ;; <= path
                      ess-swv-menu))

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
