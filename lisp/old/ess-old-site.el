;;; ess-old-setup.el --- Old setup suggestions from ess-site.ely

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

;;; Commentary:

;; You may need to require ess-old-contrib.el to apply these
;; suggestions: (require 'ess-old-contrib)

;;; Code:


;;; The following require sets ess-local-custom-available to
;;; true if custom is provided at this point.
;;; If it is not provided, but we think it will be available when necessary,
;;; then we can use the following line (uncommented) to make sure that
;;; it will be used.  If you have to ask, then you don't need this.
;; (ess-require 'ess-compat)
;; (setq ess-local-custom-available t)


;; NON DEFAULTS:
;;(defvar ess-lisp-directory
;;(directory-file-name "/usr/local/lib/xemacs/site-lisp/ess-/lisp"))
;; >> or replace "ess-" above by "ESS" which would be a symbolic link..
;; >> This way, your .emacs (or default.el or site-start.el)
;; >> won't have to change with each version of ESS

;; example of "local" or personal use
;;(defvar ess-lisp-directory
;;(directory-file-name "/stat2/faculty/rossini/ESS/lisp"))

;;)


;; Files ending in .q and .S are considered to be S source files
;; Files ending in .St are considered to be S transcript files
;;
;; NB: in standard Emacs, files ending in .s are assembler files.  If you
;; want to use assembler.  If a user wants to
;; restore the default modes for assembly file extensions, the
;; following can go into ~/.emacs or ~/.xemacs/init.el
;;
;;  (add-hook 'ess-mode-hook 'ess-restore-asm-extns)
;;  (add-hook 'inferior-ess-mode-hook 'ess-restore-asm-extns)


;; (1.4) Customize the dialects for your setup.

;;; AS OF ESS 5.1.14, if you are using Emacs 20.x, x>3, or XEmacs
;;; 21.x, x>0, you can now use the "Customize" facility for
;;; customization.

;;;; Choices for *(), where * is from inferior-*-program....
;;; Most sites will not need to use these customized program-names.  They are
;;; provided for cases where the program is not on the standard default path.
;;; If the program doesn't get located correctly by the default use of
;;; M-x S+3 (for example), then put the path name for your system into the
;;; the variable inferior-S+3-program-name.  If for any reason you want the
;;; default use of M-x S to refer to a different program than S+3, then
;;; redefine inferior-S-program-name.

;;(setq-default inferior-S3-program-name "/disk05/s/S")
;;(setq-default inferior-S+3-program-name "Splus34")
;;(setq-default inferior-S4-program-name "/disk05/s4/S")
;;(setq-default inferior-S+4-program-name "Splus")
;;(setq-default inferior-S+5-program-name "Splus5")
;;(setq-default inferior-S+-program-name "Splus7") ; unix systems ; or
;;(setq-default inferior-S+-program-name "Splus") ; unix systems
;;
;; If you wish to call other versions of R on a Unix system, ESS
;; should auto-detect other versions of R, according to matches to the
;; variable `ess-r-versions' as described in its docstring.  Consider
;; changing that variable rather than changing inferior-ess-r-program-name
;; if your version of R is not already auto-detected.
;;(setq-default inferior-R-program-name "R")        ; unix systems
;;(setq-default inferior-R-program-name "Rterm")    ; MS Windows, see below for path as well
;;(setq-default inferior-R-program-name "C:\\Program Files\\R\\R-2.5.0\\bin\\Rterm.exe")
;;(setq-default inferior-XLS-program-name "xlispstat")
;;(setq-default inferior-ARC-program-name "arc")
;;(setq-default inferior-VST-program-name "vista")
;;(setq-default inferior-SAS-program-name "sas")
;;(setq-default inferior-OMG-program-name "/home/rossini/src/anoncvs/Omegahat/org/omegahat/bin/omegahat")
;;(setq-default inferior-OMG-program-name "omegahat")


;;; The line below is the ESS default and sends the commands window
;;; to emacs, giving the user the opportunity to
;;; (1) edit the output into a clean ess-transcript file before printing, or
;;; (2) print a region of the file.
;;(setq-default inferior-S+4-print-command "S_PRINT_COMMAND=emacsclientw.exe")

;;; The editor and pager output from S+4 and Sqpe+4 are sent by
;;; StatSci default to notepad, effectively using the definition:
;;(setq-default  inferior-S+4-editor-pager-command
;;   "options(editor='notepad', pager='notepad')")
;;;
;;; ESS sends the output from both commands to an emacs buffer using
;;; the definition:
;;(setq-default  inferior-S+4-editor-pager-command
;;   "options(editor='emacsclient.exe', pager='emacsclientw.exe')")

;;; These commands are for running the PC version of Sqpe of S+4 and
;;; S+6 in an emacs buffer, using the same technology as ESS uses for
;;; Unix S-Plus.  Interactive graphics with javagraph are available
;;; in this mode beginning with S-Plus 6.1.
;;; See ess-sp4-d.el or ess-sp6w-d.el

;;; -----> configuration now via custom, see ./ess-custom.el and look for
;;;        inferior-Sqpe+... e.g. inferior-Sqpe+6-program-name


;;; see essd-els.el

;;(setq-default inferior-S-elsewhere-program-name "sh")
;;(setq-default inferior-S-elsewhere-program-name "ssh")
;;; You might consider using ssh, if you can!  (and if you really do
;;; this, use ssh-agent, etc, for securing your sessions).


;;;; Choice for S().
;;(setq-default inferior-S-program-name inferior-S+3-program-name)

;; (ess-message "[ess-site:] require 'ess-s4-d ...")
;; (require 'ess-s4-d) ; has become VERY RARE ..

;;(ess-message "[ess-site:] require 'ess-s3-d ...")
;;(require 'ess-s3-d)  ; THIS IS RARE.  You probably do not have this.

;; Old S+ versions:

;; (if ess-microsoft-p
;;     (progn
;;       (ess-message "[ess-site:] require 'ess-sp4-d ...")
;;       (require 'ess-sp4-d))
;;   ;; else: decent OS
;;   (ess-message "[ess-site:] require 'ess-sp5-d ...")
;;   (require 'ess-sp5-d))


;;; Rarely used packages:

;; (ess-message "[ess-site:] require 'ess-xls-d ...")
;; (require 'ess-xls-d)  ;; XLispStat
;; (ess-message "[ess-site:] require 'ess-vst-d ...")
;; (require 'ess-vst-d)  ;; ViSta
;; (ess-message "[ess-site:] require 'ess-arc-d ...")
;; (require 'ess-arc-d)  ;; Arc
;; (ess-message "[ess-site:] require 'ess-omg-d ...")
;; (require 'ess-omg-d)  ;; for omegahat


;;; Set this to the name of the program you use to run S or S-PLUS.  It
;;; can be an absolute pathname, if you wish.
;;(setq inferior-ess-program "Splus")
;;(setq inferior-ess-program (concat (getenv "SHOME") "/Splus"))


;;; 2.1 Backwards compatibility (roll your own!)
;;; What you want S and R to call...



;;; (3.01) SOME PEOPLE (who will remain nameless) worry that novices
;;; won't like fancy buffer names for their first (and only :-)
;;; process.  To number all processes:
;;(setq ess-plain-first-buffername nil)

;;; (3.02) Some people have requested using the program name as part
;;; of the buffer.  Turned on for R.
;;(setq ess-use-inferior-program-name-in-buffer-name t)


;;; (3.2) Framepop.  Windows produced by ess-execute-objects etc. are
;;; often unnecessarily large. The framepop package makes such
;;; windows appear in a separate, shrink-wrapped frame. This will
;;; also affect other "temporary" windows such as those produced by
;;; C-h k, etc.  To enable:
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
;;; This variable only affects the behavior
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
;; (setq ess-keep-dump-files "always")

;;; (3.4) ess-ask-for-ess-directory
;;; If t, will ask for the directory to use.  If nil, assumes the
;;; default (usually, the users home directory...).
;;now rather in ./ess-custom.el : (setq ess-ask-for-ess-directory t)

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
;;                          (concat (getenv "HOME") "/ess/")))
;;;
;;; then it will always start up in the directory "ess" in your home
;;; directory.
;;;
;;; The default is to have ess to start up in the current buffer's
;;; directory (the one in which you started the inferior ESS
;;; statistical package/process).  This is obtained
;;; by setting ess-directory to nil, i.e.
;; (setq-default ess-directory nil) ; this is the default.



;;; 3.6 Example of formatting changes

;;; Formatting and indentation patterns are defined in ess-custom.el, please
;;; see ess-custom.el for exact definitions of these variable settings.
;;; To change them (eg, follow changes suggested by Terry Therneau),
;;; you need one or both of the following lines:
;;;
;;(setq ess-indent-with-fancy-comments nil)
;;(setq ess-default-style 'CLB)

;;; 4.0 SAS configuration

;;; Beginning with ESS 5.1.13, we have editing options in SAS-mode.
;;; The default behavior is as it was in prior releases.
;;;
;;; There are two sets of alternatives.
;;;   1. Editing SAS-mode files.
;;;   1a. Default: TAB is bound to sas-indent-line.
;;;       Current line is correctly indented as SAS code.  Equivalent to
;;;(setq ess-sas-edit-keys-toggle nil) ;; default TAB in sas-mode
;;;   1b. Optional: TAB is bound to tab-to-tab-stop and inserts up to 4
;;;       columns at a time.  C-TAB moves backwards and deletes characters
;;;       up to 4 columns at a time.
;;;       The following line is for the optional behavior.
;;;(setq ess-sas-edit-keys-toggle t)   ;; optional TAB and C-TAB in sas-mode
;;;   Use the function call (ess-sas-edit-keys-toggle)
;;;   to change the setting after the first SAS-mode buffer has been created.
;;;   1c. You can also define C-TAB in all modes by Option 2b (below).
;;;
;;;   2. Managing submitted SAS jobs with function keys.
;;;   2a. Default: To define the function keys in ESS[SAS] mode only,
;;;   you will need, at most, one of the following two lines.
;;;(setq ess-sas-local-unix-keys t)  ;; F2-F12 bound in ESS[SAS] mode
;;;(setq ess-sas-local-pc-keys t)    ;; F2-F12 bound in ESS[SAS] mode
;;;
;;;   2b. Options: To define the function keys in all modes,
;;;   you will need, at most, one of the following two lines.
;;;(setq ess-sas-global-unix-keys t) ;; F2-F12 bound in all modes
;;;(setq ess-sas-global-pc-keys t)   ;; F2-F12 bound in all modes
;;;
;;;   3. If it is more convenient to have "*Async Shell Command*"
;;;      in same-window-buffer-names, then:
;;;(ess-same-window-async)
;;;
;;;(defvar sas-program "sas" "*Name of program which runs sas.")
;;;
;;;(defvar sas-indent-width 4 "*Amount to indent sas statements")


;; To remove toolbar support under ESS, add "(setq ess-use-toolbar nil)"
;; to your ~/.emacs or ~/.xemacs/init.el before (require 'ess-site)


;;; Customization (and examples) for your site
;;;; ===============================================

;;; (3.1) Font-lock
;; The following two expressions automatically enable font-lock-mode
;; for ess-mode and inferior-ess-mode buffers.

;; ;; no longer requiring  (window-system)  here:
;; (when ess-font-lock-mode
;;   (add-hook 'ess-mode-hook 'turn-on-font-lock t)
;;   (add-hook 'ess-transcript-mode-hook 'turn-on-font-lock t)
;;   (add-hook 'Rd-mode-hook 'turn-on-font-lock t)
;;   (add-hook 'inferior-ess-mode-hook 'turn-on-font-lock t))



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

;;; ess-old-setup.el ends here
