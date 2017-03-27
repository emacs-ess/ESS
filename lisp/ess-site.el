;;; ess-site.el --- user customization of ESS

;; Copyright (C) 1993 David M. Smith
;; Copyright (C) 1997--2012 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Created: 12 Nov 1993
;; Maintainer: ESS-core <ESS-core@r-project.org>
;; Keywords: local

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


;;; Commentary:

;; This file defines all the site-specific customizations for ESS.  It should be
;; edited on a per-site basis.  Read the comments (1.1 in Section 1 to see if
;; ess-site.el must be edited.  The final directory location of this file must be
;; supplied in ess-lisp-directory.  The editing of remaining sections is
;; optional.  It should then be byte-compiled, and users who wish to use ESS
;; should add the line:
;;
;;    (load "/PATH/TO/THIS/FILE/ess-site")
;;
;; (where /PATH/TO/THIS/FILE is the path to ess-site.elc: i.e. the value of
;; `ess-lisp-directory', below) to their .emacs file.
;;
;; Alternatively, if the file is already in a directory specified by
;; the load-path variable:
;;
;;    (require 'ess-site)
;;
;; will work.
;;
;; with Emacs (and in general):
;;
;;      (add-to-list 'load-path "/path/to/ess/lisp-directory")

;;; Code:

;; Provide here; otherwise we'll get infinite loops of (require ..):
(provide 'ess-site)

;;;; Load path, autoloads, and major modes
;;;; ========================================
;;
;; For most users the variable ess-lisp-directory will automatically
;; be set correctly.  If you are working with an old emacs, one in
;; which file-truename is not defined, then you might need to change
;; the value of ess-lisp-directory to the directory which is to
;; contain the file ess-site.elc.  This is probably the current
;; directory, or the value of LISPDIR if it was set in the Makefile.

;; DEBUG: (setq ess-show-load-messages t); instead of nil above

;; This sets `ess-lisp-directory' either from the current directory
;; when the file is being `load'ed, or from the installed location
;; otherwise. This way, users can load ESS without having added ESS to
;; `load-path'.
(defvar ess-lisp-directory
  ;; A nice default
  (directory-file-name
   (file-name-directory
    (if (and (boundp 'load-file-name) load-file-name)
        (file-truename load-file-name)
      (locate-library "ess-site") )))
  "Directory containing ess-site.el(c) and other ESS lisp files.")

(add-to-list 'load-path (file-name-as-directory ess-lisp-directory))
(require 'ess-utils);; <<- _not_ in load-path typically for traditional setup

(ess-message (format "[ess-site:] ess-lisp-directory = '%s'" ess-lisp-directory))


(defun ess-require (feature &rest args)
  (let ((feature-name (symbol-name feature)))
    (ess-message (concat "[ess-site:] require '" feature-name))
    (apply 'require feature args)))

;; load code to figure out what version/strain of Emacs we are running
;; must come *AFTER* load-path is set !

(ess-require 'ess-compat)

;; If ess.info is not found, then ess-lisp-directory/../doc/info is added
;; resurrecting Stephen's version with a bug-fix
(unless (locate-file "ess.info" Info-default-directory-list)
  (add-to-list 'Info-default-directory-list (expand-file-name "../doc/info/" ess-lisp-directory)))



;; Loads ess-custom.el and more
(ess-require 'ess)


;;; Loading popular dialects (they should become optional in the future)

;; R and Julia
(ess-require 'ess-r-mode)
(ess-require 'ess-julia)

;; S-PLUS (MathSoft/StatSci/Insightful/TIBCO)
(ess-require 'ess-sp3-d)
(if ess-microsoft-p
    (ess-require 'ess-sp6w-d)
  (ess-require 'ess-sp6-d))

;; S-elsewhere, on another machine by telnet
(ess-require 'essd-els)

;; Stata, SAS and batch BUGS
(ess-require 'ess-stata-mode)
(ess-require 'ess-sas-d)
(ess-require 'ess-bugs-l)

(ess-write-to-dribble-buffer
 (format "[ess-site.el]: ess-customize-alist=%s \n"
         ess-customize-alist))

;;; Literate Data Analysis
(ess-require 'ess-noweb)
(ess-require 'ess-swv)

(ess-write-to-dribble-buffer
 (format "[ess-site.el _2_]: ess-customize-alist=%s \n"
         ess-customize-alist))

;;; Speedbar and mouse
(ess-require 'ess-mouse)

;;; Toolbar support
(ess-require 'ess-toolbar)

(ess-require 'ido nil t)


;;;  Site Specific setup
;;;; ===============================================

(autoload 'Rd-mode "ess-rd" "Major mode for editing R documentation." t)

;; Be careful when editing the following. MISTAKES WILL RESULT IN
;; *.sty BEING TREATED AS ESS[S], rather than LaTeX-mode!

(unless (assoc "\\.[rR]\\'" auto-mode-alist)
  (setq auto-mode-alist
        (append
         '(("\\.sp\\'"          . S-mode) ;; re: Don MacQueen <macq@llnl.gov>
           ("/R/.*\\.q\\'"      . R-mode) ;; R/*.q is R code (e.g., in package)
           ("\\.[qsS]\\'"       . S-mode) ;; s,S [see ess-restore-asm-extns above!]
           ("\\.ssc\\'"         . S-mode) ;; Splus (>= 4.x) script files.
           ("\\.SSC\\'"         . S-mode) ;; ditto for windoze
           ("\\.[rR]\\'"        . R-mode)
           ("\\.[rR]nw\\'"      . Rnw-mode)
           ("\\.[sS]nw\\'"      . Snw-mode); currently identical to Rnw-mode
           ("\\.[rR]profile\\'" . R-mode)
           ("NAMESPACE\\'"      . R-mode)
           ("CITATION\\'"       . R-mode)
           ("\\.omg\\'"         . omegahat-mode)
           ("\\.hat\\'"         . omegahat-mode) ;; Duncan's pref'd...
           ("\\.lsp\\'"         . XLS-mode)
           ("\\.do\\'"          . STA-mode)
           ("\\.ado\\'"         . STA-mode)
           ("\\.[Ss][Aa][Ss]\\'"        . SAS-mode)
           ;; Many .log/.lst files, not just SAS
           ;;("\\.log\\'"       . SAS-log-mode)
           ;;("\\.[Ll][Ss][Tt]\\'"      . SAS-listing-mode)
           ("\\.[Ss]t\\'"       . S-transcript-mode)
           ("\\.Sout"           . S-transcript-mode)
           ;;("\\.[Rr]t\\'"       . R-transcript-mode)
           ("\\.[Rr]out"        . R-transcript-mode)
           ("\\.Rd\\'"          . Rd-mode)
           ("\\.[Bb][Uu][Gg]\\'"         . ess-bugs-mode)
           ("\\.[Bb][Oo][Gg]\\'"         . ess-bugs-mode)
           ("\\.[Bb][Mm][Dd]\\'"         . ess-bugs-mode)
           ("\\.[Jj][Aa][Gg]\\'"         . ess-jags-mode)
           ("\\.[Jj][Oo][Gg]\\'"         . ess-jags-mode)
           ("\\.[Jj][Mm][Dd]\\'"         . ess-jags-mode)
           )
         auto-mode-alist)))

;; Rscript and littler interpreters recognized.
(add-to-list 'interpreter-mode-alist '("Rscript" . r-mode))
(add-to-list 'interpreter-mode-alist '("r" . r-mode))

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)
(autoload 'ess-transcript-clean-region "ess-trns" no-doc t)

(autoload 'ess-rdired "ess-rdired"
  "View *R* objects in a dired-like buffer." t)

(eval-after-load "ess-r-mode"
  '(progn
     (ess-message "[ess-site:] before creating ess-versions-* ...")
     (ess-r-s-versions-creation+menu)
     (ess-message "[ess-site:] after ess-versions-created ...")))

;; Check to see that inferior-ess-r-program-name points to a working version
;; of R; if not, try to find the newest version:
(ess-check-R-program-name) ;; -> (ess-find-newest-R) if needed, in ./ess-r-d.el
(ess-message "[ess-site:] after ess-check-R-prog... ...")

(ess-message "[ess-site:] *very* end ...")


 ; Local variables section

;; This file is automatically placed in Outline minor mode.
;; The file is structured as follows:
;; Chapters:     ^L ;
;; Sections:    ;;*;;
;; Subsections: ;;;*;;;
;; Components:  defuns, defvars, defconsts
;;              Random code beginning with a ;;;;* comment
;; Local variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;; End:

;;; ess-site.el ends here
