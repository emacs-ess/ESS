;;; ess-site.el --- user customization of ess-mode

;; Copyright (C) 1993 David M. Smith

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Nov 1993
;; Modified: $Date: 1997/11/07 23:17:54 $
;; Version: $Revision: 1.57 $
;; RCS: $Id: ess-site.el,v 1.57 1997/11/07 23:17:54 rossini Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ess-mode.

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

;;; This file defines all the site-specific customizations for ess-mode.
;;; It should be edited on a per-site basis.  Section 1 *must* be
;;; edited, and the correct pathname for the directory which contains
;;; this file must be supplied in ess-lisp-directory.  The editing of
;;; remaining sections is optional.  It should then be byte-compiled,
;;; and users who wish to use ess-mode should add the line:
;;;    (load "/PATH/TO/THIS/FILE/ess-site")
;;; (where /PATH/TO/THIS/FILE is the path to ess-site.elc: i.e. the
;;; value of ess-lisp-directory, below) to their .emacs file.
;;;
;;; Alternatively, if the file is already in a directory specified by
;;; the load-path variable, a simple:
;;;    (require 'ess-site)
;;; will work.

(provide 'ess-site)

;;; Code:

;;(setq-default debug-on-error t)

;;;; 1. Load path, autoloads, and major modes
;;;; ========================================
;;;
;;; (1.1) Change the value of ess-lisp-directory to the directory which
;;; is to contain the file ess-site.elc.  This is probably the current
;;; directory, or the value of LISPDIR if it was set in the Makefile.

(eval-and-compile

  ;; Not important in XEmacs, if unpacking from ../xemacs/site-lisp/
  ;; directory.

  ;; Suggested by RMH
  (defvar ess-lisp-directory
    (directory-file-name (file-name-directory (file-truename load-file-name))))
  ;;(defvar ess-lisp-directory
  ;;(directory-file-name "/p1/apps/X11R6.3/lib/xemacs/site-lisp/ESS"))
  ;;(directory-file-name "/usr/local/lib/xemacs/site-lisp/ess-mode"))
  ;;(directory-file-name "/usr/local/share/emacs/site-lisp/ess-mode"))
  ;;(directory-file-name "/stat2/faculty/rossini/S-mode"))
  
  (add-to-list 'load-path ess-lisp-directory))


;;; (1.2) Files ending in .q and .S are considered to be S source files
;;; Files ending in .St are considered to be S transcript files
;;; NB: in standard Emacs, files ending in .s are assembler files.  If you
;;; want to use assembler, comment the appropriate
;;; line below.

(autoload 'Rd-mode "essddr" "Major mode for editing R documentation." t)

;; This fails in Emacs.  How can it be done simply?  Should it be done?
;;    ;; get rid of assembler mode.
;;    (set auto-mode-alist (remassoc "\\.[sS]\\'" auto-mode-alist))


(if (assoc "\\.[rR]\\'" auto-mode-alist) nil
  (setq auto-mode-alist
	(append
	 '(("\\.sp\\'"    . S-mode) ;; re: Don MacQueen <macq@llnl.gov>
	   ("\\.[qsS]\\'" . S-mode) ;; q,s,S
	   ("\\.[rR]\\'"  . R-mode)
	   ("R.*/src/library/[A-Za-z]+/funs/[A-Za-z]" . R-mode); R <= 0.50
	   ("R.*/src/library/[A-Za-z]+/R/[A-Za-z]"    . R-mode); R >= 0.60
	   ("R.*/src/library/[A-Za-z]+/man/[A-Za-z]"  . Rman-mode);R <= 0.50
	   ("\\.lsp\\'"                               . XLS-mode)
	   ("\\.sas\\'"                               . SAS-mode)
	   ("\\.SAS\\'"                               . SAS-mode)
	   ("\\.lst\\'"                               . SAS-listing-mode);sasl
	   ;; Too many *.log files!
	   ;;("\\.log\\'"                               . SAS-log-mode);sasl
	   ("\\.[Ss]\\(ou\\)?t\\'"                    . S-transcript-mode)
	   ("\\.[Rr]\\(ou\\)?t\\'"                    . R-transcript-mode)
	   ("\\.Rd\\'"                                . Rd-mode);all R>=0.60
	   )
	 auto-mode-alist)))



;; (1.4) Customize the dialects for your setup.

;;;; Choices for *(), where * is from inferior-*-program....
;; Most sites will not need to use these customized program-names.  They are
;; provided for cases where the program is not on the standard default path.
;; If the program doesn't get located correctly by the default use of
;; M-x S+3 (for example), then put the path name for your system into the
;; the variable inferior-S+3-program-name.  If for any reason you want the
;; default use of M-x S to refer to a different program than S+3, then
;; redefine inferior-S-program-name.
;;
;;(setq-default inferior-S+3-program-name "Splus")
;;(setq-default inferior-R-program-name "/disk05/rmh/xemacs/R/R-0.49/bin/R")
;;(setq-default inferior-XLS-program-name "xlispstat")
;;(setq-default inferior-S4-program-name "/disk05/s4/S")
;;(setq-default inferior-S3-program-name "/disk05/s/S")
;;(setq-default inferior-SAS-program-name "sas")

;;;; Choice for S().
;;(setq-default inferior-S-program-name inferior-S+3-program-name)

;; (1.5) Require the needed dialects for your setup.

(require 'essd-s+3)
(require 'essd-r)
(require 'essd-xls)
(require 'essd-sas)
(require 'essd-s4)

;;TODO, for 5.0 :-), or rare.
;;(require 'essd-s3)  ; You might not have this
;;(require 'essd-vst) ; built on essd-xls.
;;(require 'essd-s+4) ; not for Unix, yet.


(require 'ess)

;;; 2. Site Specific setup
;;;; ===============================================

;;; Set this to the name of the program you use to run S or S-PLUS.  It
;;; can be an absolute pathname, if you wish.
;;(setq inferior-ess-program "Splus")
;;(setq inferior-ess-program (concat (getenv "SHOME") "/Splus"))

;;; You will need to change the following two variables if you use a
;;; non-standard S prompt.
;; (setq inferior-ess-primary-prompt "[a-zA-Z0-9() ]*> ?")
;; (setq inferior-ess-secondary-prompt "+ ?")


;;; 2.1 Backwards compatibility (roll your own!)
;;; What you want S to call...
(defun S ()
  "Basic, usual, call..."
  (interactive)
  (S+3))

;;; The basic mode.
(defun S-mode (&optional proc-name)
  "Major mode for editing S+3 source.  See ess-mode for more help."
  (interactive)
  (if proc-name (S+3-mode proc-name)
    (S+3-mode)))


(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files" t)

(defun s-transcript-mode ()
  "Does the right thing."
  (ess-transcript-mode S+3-customize-alist))



;;;;* Alias S-mode to s-mode
;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line. 
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*- S-*-" invokes s-mode and 
;;; not S-mode.
(fset 'S-transcript-mode 's-transcript-mode)
(fset 's-mode 'S-mode)


;; Alternative method (better?)
;;(fset 'S 'S+3)
;;(fset 'S-mode 'S+3-mode)





;;; 3. Customization (and commented out examples) for your site
;;;; ===============================================

;;; (3.1) Font-lock
;; The following two expressions automatically enable font-lock-mode
;; for ess-mode and inferior-ess-mode buffers.

;; XEmacs has font-lock for ttys, as well.
(if window-system
    (progn
      (add-hook 'ess-mode-hook 'turn-on-font-lock t)
      (add-hook 'ess-transcript-mode-hook 'turn-on-font-lock t)
      (add-hook 'inferior-ess-mode-hook 'turn-on-font-lock t)))

;; If nil, then don't font-lock the input
;; if t, font-lock (default).
(setq inferior-ess-font-lock-input t) ; from RMH

;;; (3.2) Framepop.  Windows produced by ess-execute-objects etc. are
;;; often unneccessarily large. The framepop package makes such
;;; windows appear in a separate, shrink-wrapped frame. This will
;;; also affect other "temporary" windows such as those produced by
;;; C-h k, etc.  To enable, uncomment both lines of code below).
;;;
;;; Works only with Emacs at this time.
;; (cond (window-system
;;       (require 'framepop)))

;;; (3.3) ess-keep-dump-files.
;;; Documentation:
;;; *Variable controlling whether to delete dump files after a successful load.
;;; If nil: always delete.  If `ask', confirm to delete.  If `check', confirm
;;; to delete, except for files created with ess-dump-object-into-edit-buffer.
;;; Anything else (for example `always'): always keep and never delete.
;;; This variable only affects the behaviour
;;; of ess-load-file.  Dump files are never deleted if an error occurs
;;; during the load. 
;;;
;;; RH sez: I find the default `always' keep to be imperative.  The previous
;;; default was to throw away
;;; files at the wrong time (I think it was something like, if you M-x
;;; ess-load a file twice, while you are working on it, the file is
;;; deleted).  I believe source is real and the ESS object is temporary.
;;; The previous default behavior is dangerous for people who believe this way.
;;; It made sense only for people who believe the object is real
;;; and the source file temporary.
(setq ess-keep-dump-files "always")

;;; (3.4) ess-ask-for-ess-directory 
;;; If t, will ask for the directory to use.  If nil, assumes the
;;; default (usually, the users home directory...).
(setq ess-ask-for-ess-directory t)

;;; (3.5) ess-directory default  (correlated with above)
;;; The default location for running the subprocess is configurable.
;;; By default, that is the default-directory (a lisp variable which
;;; initially contains the directory from which the inferior ESS
;;; statistical package/process  is started).
;;; For example, the following function (added to the pre-run-hook, by
;;; the line following it) will set the default directory to be your
;;; home directory:
;;;
;;(defun ajr:ess-set-directory () 
;;  "Set ess-directory to home."
;;  (setq-default ess-directory (file-name-as-directory (getenv "HOME"))))
;;(add-hook 'ess-pre-run-hook 'ajr:ess-set-directory)
;;;
;;; If you replace the setq-default line with:
;;;
;; (setq-default ess-directory (file-name-as-directory 
;;                                                  (concat (getenv "HOME") "/ess/"))) 
;;;
;;; then it will always start up in the directory "ess" in your home
;;; directory.
;;;
;;; The default is to have ess to start up in the current buffer's
;;; directory (the one in which you started the inferior ESS
;;; statistical package/process).  This is obtained
;;; by setting ess-directory to nil, i.e. 
;; (setq-default ess-directory nil) ; this is the default.


;;; 3.6 Example of formatting change (suggested by Terry Therneau).

;;;;The defaults in ess-vars.el are identical to current behavior:
;;;;added rmh 2Nov97 at request of Terry Therneau
;;(setq ess-close-brace-offset 0
;;  "*Extra indentation for closing braces")
;;(setq ess-fancy-comments t
;;  "*Non-nil means distiguish between #, ##, and ### for indentation")
;;
;;;;Terry will want to add the definitions:
;;(setq ess-fancy-comments nil
;;  "*Non-nil means distiguish between #, ##, and ### for indentation")
;;(setq ess-default-style 'CLB
;;  "*The default value of ess-style")


;;; 4.0 SAS configuration

;(defvar sas-require-confirmation t
;  "*Require confirmation when revisiting sas-output which has changed on disk.")
;;; added sas-program 4/29/94.  user can specify a different version of sas.
;;;(defvar sas-program "sas" "*Name of program which runs sas.")
;;(defvar sas-pre-run-hook nil
;;  "Hook to execute prior to running SAS vis submit-sas.")
;;(defvar sas-options-string ""
;;  "*Options to be passed to sas as if typed on the command line.")
;;(defvar sas-indent-width 4 "*Amount to indent sas statements")
;;(defvar sas-notify t "*Beep and display message when job is done?")  ;; added 4/7/94
;;(defvar sas-error-notify t
;;  "*If sas-notify is t, then indicate errors in log file upon completion")
;;;; added 5/2/94
;;(defvar sas-get-options nil "Options to be passed to SAS in sas-get-dataset")
;;(defvar sas-get-options-history nil "History list of Options passed to SAS in sas-get-dataset")
;;(defvar sas-page-number-max-line 3
;;  "*Number of lines from the page break in which to search for the page number")
;;(defvar sas-indent-ignore-comment "*"
;;  "*Comments with start with this string are ignored in indentation.")
;;(defvar sas-notify-popup nil
;;  "*If t (and sas-notify is also t), causes emacs to create a
;;popup window when the SAS job is finished.")
;;(defvar sas-tmp-libname "_tmp_" "*Libname to use for sas-get-dataset.")



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

;;; ess-site.el ends here
