;;; ess-sp6w-d.el --- S-PLUS 6.x  for Windows customization

;;; copied and edited from ess-s4-d.el - Richard M. Heiberger, April 2001

;; Copyright (C) 2001 Richard M. Heiberger <rmh@sbm.temple.edu>
;; Copyright (C) 2002--2005 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Richard M. Heiberger <rmh@sbm.temple.edu>
;; Created: April 2001
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; Keywords: start up, configuration.

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
;;;
;;; This file defines all the S-PLUS 6.x for Windows customizations
;;; for ess-mode with ddeclient.

;;; Requires and Autoloads:

(require 'ess-s-l)
;;NO: this is autoloaded from other places (require 'ess-dde)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

;;; Code:

(defcustom S+6-dialect-name "S"
  "Name of 'dialect' for S-PLUS 6.x and later.
Easily changeable in a user's `.emacs'."
  :group 'ess-SPLUS
  :type 'string)

(defvar inferior-S+6-start-args " "
  "Default is empty.  Can be used for license manager information, for example
`(setq inferior-S+6-start-args \" S_ELMHOST=\\\\\\\\@123.456.789.012  ELMTIMEOUT=60 \")'."
;; (setq inferior-S+6-start-args " S_ELMHOST=\\\\@123.456.789.012  ELMTIMEOUT=60 ")  ;; use this line as the model for your site-start.el
)

(defvar inferior-Sqpe-start-args " "
  "Default is empty.  Can be used for license manager information, for example
`(setq inferior-Sqpe-start-args \" S_ELMHOST=@123.456.789.012  ELMTIMEOUT=60 \")'."
;; (setq inferior-Sqpe-start-args " S_ELMHOST=@123.456.789.012  ELMTIMEOUT=60 ")  ;; use this line as the model for your site-start.el
)

(defvar inferior-S+6-multipleinstances "/MULTIPLEINSTANCES"
  "Default \"/MULTIPLEINSTANCES\" opens up a new instance of S+[678] in a
GUI window and connects it to the '(ddeESS [S+6])' window.  The
alternative nil uses an existing S+6 GUI (if there is one) and
connects it to the '(ddeESS [S+6])' window.")

(defvar S+6-customize-alist
  (append
  '((ess-local-customize-alist  . 'S+6-customize-alist)
    (ess-dialect                . S+6-dialect-name)
    (ess-loop-timeout		. ess-S-loop-timeout);fixme: dialect spec.
    (ess-object-name-db-file    . "ess-sp6-namedb.el" )
    (inferior-ess-program       . inferior-S+6-program-name)
    (inferior-ess-help-command  . "help(\"%s\")\n")
    (inferior-ess-help-filetype . "chm")
    (inferior-ess-search-list-command . "searchPaths()\n")
    (inferior-ess-start-file    . nil) ;"~/.ess-S+6")
    (inferior-ess-start-args    . (concat
				   inferior-S+6-multipleinstances
				   " "
				   inferior-S+6-start-args
				   " "
				   inferior-S+6-print-command
				   " S_PROJ="
				   (w32-short-file-name (directory-file-name default-directory))))
;;    (inferior-ess-ddeclient      . "ddeclient")
;;    (inferior-ess-client-name    . "S-PLUS")
;;    (inferior-ess-client-command . "SCommand")
    (ess-STERM  . "ddeESS")
    )
  S+common-cust-alist)
  "Variables to customize for S+6")

(defvar Sqpe+6-customize-alist
  (append
  '((ess-local-customize-alist  . 'Sqpe+6-customize-alist)
    (ess-dialect                . S+6-dialect-name)
    (ess-loop-timeout           . 500000 );fixme: dialect specific custom.var
    (ess-object-name-db-file    . "ess-sp6-namedb.el" )
    (inferior-ess-program       . inferior-Sqpe+6-program-name)
    (inferior-ess-help-command  . "help(\"%s\")\n")
    (inferior-ess-help-filetype . "chm")
    (inferior-ess-search-list-command . "searchPaths()\n")
    (inferior-ess-start-file    . nil) ;"~/.ess-S+6")
    (inferior-ess-start-args    . (concat
				   ;; workaround for bug in S-Plus 6 for Windows:
				   "ALWAYS_PROMPT=X"
				   " "
				   inferior-Sqpe-start-args ;; e.g. license manager
				   ))
    (ess-STERM  . "iESS")
    )
  S+common-cust-alist)
 "Variables to customize for Sqpe+6.")



;;; There are extra complications in S+6 and S+7 and S+8 (compared to S+3)
;;; because
;;;
;;; (1) The StatSci supplied Splus.exe doesn't work in an emacs
;;;     buffer.  It works as as a GUI window and we must send commands
;;;     to it through ddeclient.  Nonetheless, we need to give it a
;;;     process name and be sure that that there is a valid running
;;;     process in the '(ddeESS [S+6])' buffer.  Therefore we create an
;;;     ESS process in the buffer as a placeholder and start a shell
;;;     in the ESS buffer.  From the shell we start Splus.  Once Splus
;;;     finishes initializing and kills the original shell, we start
;;;     another shell.  We have a buffer-local variable
;;;     inferior-ess-ddeclient, initialized to nil.  When there is a
;;;     non-nil value of inferior-ess-ddeclient we send lines to
;;;     inferior-ess-ddeclient rather than to the Splus process.
;;; (2) There is no Splus process running in the '(ddeESS [S+6])'
;;;     buffer.  Therefore inferior-ess will never see a prompt,
;;;     unless we first change it to the null prompt "^".  Then once
;;;     the process has started, we change it back.
;;; (3) When M-x S+6 starts Splus by a shell command, then Splus is an
;;;     independent process and will be survive if the '(ddeESS [S+6])'
;;;     buffer is killed (or emacs is quit).  The '(ddeESS [S+6])' is
;;;     made read-only and a warning is placed in it saying that "You
;;;     can't type anything here."  Actually, if thestandalone Splus
;;;     is killed and the '(ddeESS [S+6])' is made writable (C-x C-q),
;;;     then '(ddeESS [S+6])' becomes a shell buffer.
;;;
(defun S+6 (&optional proc-name)
  "Verify that `inferior-S+6-program-name' points to S-Plus 6 or
S-Plus 7 or S-Plus 8.  Start normally for S-Plus 6.1 and later.
Inform the user to start S-Plus 6.0 from the icon and then
connect to it with `S+6-existing'.  Give an error message if
`inferior-S+6-program-name' doesn't point to S-Plus 6 or S-Plus 7
or S-Plus 8."
  (interactive)
  (save-excursion
    (set-buffer (find-file-noselect
		 (concat (executable-find inferior-S+6-program-name)
			 "/../../versions") t))
    (toggle-read-only 1)
    (forward-line)
    (if (not (search-backward-regexp "splus\t[678].[0-9]" (point-min) t))
	(error "The emacs variable `inferior-S+6-program-name' does
not point to S-Plus 6 or 7 or 8.  Please add `splus[678]?/cmd' (expand the
`[678]?' to match your setup) to your `exec-path' or specify the complete
path to `Splus.exe' in the variable `inferior-S+6-program-name' in your
`.emacs' file.")
      (forward-line)
      (if (search-backward "splus\t6.0" (point-min) t)
	  (error "S-Plus 6.0 for Microsoft Windows has a bug that
prevents it from being started by emacs.  Instead, you must start it
by double-clicking an icon.  Then you can connect to it with
`S+6-existing'.  You should consider upgrading to a newer
release of S-Plus."))))
  (S+6-initiate proc-name)) ;; normal start ;


(defun S+6-initiate (&optional proc-name)
  "Call 'S-PLUS [678].x for Windows', the 'GUI Thing' from StatSci.  Put
S-Plus in an independent MS-Window (Splus persists even if the
'(ddeESS [S+6])' window is killed in emacs).  Do this by creating a
comint process that calls sh.  Send a shell command in that sh buffer
to call Splus.  When it completes set up a shell as a placeholder in
the '(ddeESS [S+6])' buffer.  The S-Plus options are correctly set.
In particular, the S-Plus Commands window is opened if the
Options/General Settings/Startup menu says it should be.  There is a
startup delay of `ess-S+6-startup-delay' seconds during which the
screen will not be refreshed.  This delay is here to allow slow disks
to start the Splus program."
  (interactive)
  (save-excursion
    (setq ess-customize-alist S+6-customize-alist)
    (ess-write-to-dribble-buffer
     (format "\n(S+6): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist		; change inferior-ess-program
	  (append ess-customize-alist '((inferior-ess-program   . "sh"))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . "-i"))))
    (let ((s-proj (getenv "S_PROJ"))
	  (use-dialog-box (not (or ess-microsoft-p (eq system-type 'cygwin))))
	  )
      (cd (w32-short-file-name (directory-file-name default-directory)))
      (setenv "S_PROJ" (w32-short-file-name default-directory))
      (inferior-ess)
      (sleep-for 2) ; need to wait, else working too fast!  The Splus
		    ; command in '(ddeESS [S+6])' should follow the "$"
		    ; prompt.  If not, then increase the sleep-for time!
      (setenv "S_PROJ" s-proj))
    (setq ess-customize-alist S+6-customize-alist)
    (ess-setq-vars-local ess-customize-alist)
;;; the next three lines belong in customize-alist, but can't be there
;;; because of the broken ess-setq-vars-default usage in ess-inf.el
    (setq inferior-ess-ddeclient         "ddeclient")
    (setq inferior-ess-client-name       "S-PLUS")
    (setq inferior-ess-client-command    "SCommand")
;;; end of what belongs in customize-alist
    (setq comint-process-echoes nil)
    (setq comint-input-sender 'comint-simple-send)
    (goto-char (point-max))
    (insert (concat inferior-S+6-program-name " "
		    inferior-ess-start-args)) ; Note: there is no final "&".
    ;; Without the "&", the results of  !system.command  come to '(ddeESS [S+6])'
    ;; With the "&", the results of  !system.command  in S get lost.
    (inferior-ess-send-input)
    (sleep-for ess-S+6-startup-delay) ; Need to wait, else working too fast!
                   ; If the ess-current-process-name doesn't appear in the
       		   ; Splus Commands window increase the sleep-for time!
    (setq ess-local-process-name ess-current-process-name)
    (ess-eval-linewise (concat "#" ess-current-process-name))
    (goto-char (point-min))
    (insert
     "This is a placeholder buffer.  You can't type anything here.
Use `C-x b RET' to return to your file.\n
Anything sent to this process from an S-mode buffer goes
directly to the associated Splus Commands window.\n
The S-Plus Commands window must be visible.
You may need to open the S-Plus Commands window manually (by clicking on
Splus/Window/Commands Window).\n
Any results of the   !system.command   typed at the S prompt in the
Splus Commands window appear in this buffer.\n\n")
    (goto-char (point-max))		; comint-mode-map makes '(ddeESS [S+6])'
;;  (use-local-map comint-mode-map)     ;a shell buffer after Splus is finished.
    (set-buffer-process-coding-system 'raw-text-dos 'raw-text-unix)
    (toggle-read-only t)		; force buffer to be read-only
    (setq mode-name "ddeESS")
;;  (ess-eval-linewise inferior-S+6-editor-pager-command)
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start))
    ))




(defun S+6-existing (&optional proc-name)
  "Call 'S-PLUS [678].x for Windows', the 'GUI Thing' from StatSci.  Do so by
finding an existing S-Plus in an independent MS-Window (if there is one) and
set up a '(ddeESS [S+6])' buffer in emacs.  If there is no existing
S-Plus, then a new one will be opened in the default directory.  The default
is usually something like 'c:/Program Files/Insightful/splus70/users/yourname'
for S-Plus before 8.0.4.  Beginning with 8.0.4, S-Plus uses the default directory
'c:/Documents and Settings/yourname/My Documents/S-PLUS Projects/Project1'.
Beginning with TIBCO Spotfire S+ Version 8.1.1 for Microsft Windows, the default
directory is 'c:/DOCUME~1/yourname/MYDOCU~1/SPOTFI~1/Project1'.
If you have a HOME environment variable, it will open it there."
  (interactive)
  (let* ((inferior-S+6-multipleinstances " & # ") ; Note: there is a final "&".
	 (ess-S+6-startup-delay 0)) ;; No delay for existing S-Plus
    ;; Without the "&", there is a core dump.
    ;; With the "&", the results of  !system.command  in S get lost.
    ;; We are picking up an existing S-Plus process for sending to.
    ;; It doesn't know about us, so nothing comes back.
    (S+6-initiate proc-name))
  (save-excursion
    (set-buffer (car (buffer-list)))    ; get the ESS buffer just created
    (toggle-read-only nil)		; permit writing in ESS buffer
    (goto-char (point-max))
    (beginning-of-line)
    (forward-line -1)
    (insert
     "This is S+6-existing.
Results of the   !system.command   typed at the S prompt in the
Splus Commands window blink a DOS window and you won't see them.\n\n")
    (toggle-read-only t)		; restore ESS buffer to be read-only
    ))


;;; There are extra complications in Sqpe+6 (compared to S+3) because
;;; (1) The StatSci supplied Sqpe.exe won't work without SHOME as an
;;;     environment variable and Sqpe does not take command line
;;;     arguments and
;;; (2) Sqpe.exe comes up with options(interactive=F), which means it
;;;     doesn't provide prompts by default, and we must change it to T so
;;;     it will provide prompts.
;;;
(defun Sqpe+6 (&optional proc-name)
  "Call 'Sqpe' from 'S-PLUS [678].x for Windows', the 'Real Thing' from StatSci."
  (interactive)
  (setq ess-customize-alist Sqpe+6-customize-alist)
  (let* ((shome-nil-p (equal (getenv "SHOME") nil))
	 (use-dialog-box (not (or ess-microsoft-p (eq system-type 'cygwin))))
	 )
    (if shome-nil-p (setenv "SHOME" inferior-Sqpe+6-SHOME-name))
    (ess-write-to-dribble-buffer
     (format "\n(Sqpe+6): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (inferior-ess)
    (setq ess-customize-alist Sqpe+6-customize-alist) ; restore i-e-p-p in alist
    (ess-setq-vars-local ess-customize-alist)    ; restore i-e-p-p in buffer
    (setq inferior-ess-prompt                    ; define with correct i-e-p-p
	  ;; Do not anchor to bol with `^'       ; (copied from ess-inf.el)
	  (concat "\\("
		  inferior-ess-primary-prompt
		  "\\|"
		  inferior-ess-secondary-prompt
		  "\\)"))
    (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
                                                ; define with correct i-e-p-p
    (setq comint-input-sender 'inferior-ess-input-sender)
    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
    (goto-char (point-max))
    (insert "options(interactive=T)")
    (inferior-ess-send-input)
    (setq mode-name "iESS(Sqpe)")
;;  (ess-eval-linewise inferior-S+6-editor-pager-command)
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start))
    (if shome-nil-p (setenv "SHOME" nil))))



(defun S+6-mode (&optional proc-name)
  "Major mode for editing S+[678] source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+6-customize-alist)
  (ess-mode S+6-customize-alist proc-name)
  (if ess-imenu-use-S (ess-imenu-R)))


(defun S+6-transcript-mode ()
  "S-PLUS 6.x transcript mode."
  (interactive)
  (ess-transcript-mode S+6-customize-alist))


(defun S+6-msdos (&optional proc-name)
  "Verify that `inferior-S+6-program-name' points to S-Plus 6 or
S-Plus 7 or S-Plus 8.  Start normally for S-Plus 6.1 and later.
Inform the user to start S-Plus 6.0 from the icon and then
connect to it with `S+6-msdos-existing'.  Give an error message
if `inferior-S+6-program-name' doesn't point to S-Plus 6 or
S-Plus 7 or S-Plus 8."
  (interactive)
  (save-excursion
    (set-buffer (find-file-noselect
		 (concat (executable-find inferior-S+6-program-name)
			 "/../../versions") t))
    (toggle-read-only 1)
    (forward-line)
    (if (not (search-backward-regexp "splus\t[678].[0-9]" (point-min) t))
	(error "The emacs variable `inferior-S+6-program-name' does
not point to S-Plus 6 or 7 or 8.  Please add `splus[678]?/cmd'
(expand the `[678]?' to match your setup) to your `exec-path' or
specify the complete path to `Splus.exe' in the variable
`inferior-S+6-program-name' in your `.emacs' file.")  ;;; " This comment keeps emacs font-lock from getting out of phase.
      (progn
    (forward-line)
      (if (search-backward "splus\t6.0" (point-min) t)
	    (error "S-Plus 6.0 for Microsoft Windows has a bug that
prevents it from being started by emacs.  Instead, you must start it
by double-clicking an icon.  Then you can connect to it with
`S+6-msdos-existing'.  You should consider upgrading to a newer
release of S-Plus.")
  (S+6-msdos-initiate proc-name))) ;; normal start ;
      )))


(defun S+6-msdos-initiate (&optional proc-name)
  "Call 'S-PLUS [678].x for Windows', the 'GUI Thing' from StatSci.  Put
S-Plus in an independent MS-Window (Splus persists even if the
'(ddeESS [S+6])' window is killed in emacs).  Do this by creating a
comint process that calls sh.  Send a shell command in that sh buffer
to call Splus.  When it completes set up a shell as a placeholder in
the '(ddeESS [S+6])' buffer.  The S-Plus options are correctly set.
In particular, the S-Plus Commands window is opened if the
Options/General Settings/Startup menu says it should be.  There is a
startup delay of `ess-S+6-startup-delay' seconds during which the
screen will not be refreshed.  This delay is here to allow slow disks
to start the Splus program."
  (interactive)
  (save-excursion
    (setq ess-customize-alist S+6-customize-alist)
    (ess-write-to-dribble-buffer
     (format "\n(S+6): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist		; change inferior-ess-program
	  (append ess-customize-alist '((inferior-ess-program
					 . (getenv "COMSPEC")))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . ""))))
    (let ((s-proj (getenv "S_PROJ"))
	 (use-dialog-box (not (or ess-microsoft-p (eq system-type 'cygwin))))
	  )
      (cd (w32-short-file-name (directory-file-name default-directory)))
      (setenv "S_PROJ" (w32-short-file-name default-directory))
      (inferior-ess)
      (sleep-for 2) ; need to wait, else working too fast!  The Splus
		    ; command in '(ddeESS [S+6])' should follow the "$"
		    ; prompt.  If not, then increase the sleep-for time!
      (setenv "S_PROJ" s-proj))
    (setq ess-customize-alist S+6-customize-alist)
    (ess-setq-vars-local ess-customize-alist)
;;; the next three lines belong in customize-alist, but can't be there
;;; because of the broken ess-setq-vars-default usage in ess-inf.el
    (setq inferior-ess-ddeclient         "ddeclient")
    (setq inferior-ess-client-name       "S-PLUS")
    (setq inferior-ess-client-command    "SCommand")
;;; end of what belongs in customize-alist
    (setq comint-input-sender 'comint-simple-send)
    (setq comint-process-echoes nil)
    (set-buffer-process-coding-system 'raw-text-dos 'raw-text-dos)
    (goto-char (point-max))
    (insert (concat inferior-S+6-program-name " "
		    inferior-ess-start-args)) ; Note: there is no final "&".
    ;; Without the "&", the results of  !system.command  come to '(ddeESS [S+6])'
    ;; With the "&", the results of  !system.command  in S get lost.
    (inferior-ess-send-input)
    (sleep-for ess-S+6-startup-delay) ; Need to wait, else working too fast!
                   ; If the ess-current-process-name doesn't appear in the
       		   ; Splus Commands window increase the sleep-for time!
;;; from msdos-minor-mode
  (setq comint-process-echoes t)
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;;; end from msdos-minor-mode
    (setq ess-local-process-name ess-current-process-name)
    (ess-eval-linewise (concat "#" ess-current-process-name))
    (goto-char (point-min))
    (insert
     "This is a placeholder buffer.  You can't type anything here.
Use `C-x b RET' to return to your file.\n
Anything sent to this process from an S-mode buffer goes
directly to the associated Splus Commands window.\n
The S-Plus Commands window must be visible.
You may need to open the S-Plus Commands window manually
(by clicking on Splus/Window/Commands Window).\n
There is a `ess-S+6-startup-delay' second delay when this program starts
during which the emacs screen will be partially blank.\n
Remember to 'q()' from S-Plus and
then C-x C-q exit from the '(ddeESS [S+6])' buffer,
or take the risk of not being able to shut down your computer
and suffering through scandisk.\n
Any results of the   !system.command   typed at the S prompt in the
Splus Commands window (are supposed to) appear in this buffer.\n\n")
    (goto-char (point-max))	       ; comint-mode-map makes '(ddeESS [S+6])'
    (use-local-map comint-mode-map)    ; a shell buffer after Splus is finished.
    (toggle-read-only t)	       ; force buffer to be read-only
    (setq mode-name "ddeESS")
;;  (ess-eval-linewise inferior-S+6-editor-pager-command)
    (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start))
    ))

(defun S+6-msdos-existing (&optional proc-name)
  "Call 'S-PLUS [678].x for Windows', the 'GUI Thing' from StatSci.  Do so by
finding an existing S-Plus in an independent MS-Window (if there is one) and
set up a '(ddeESS [S+6])' buffer in emacs.  If there is no existing
S-Plus, then a new one will be opened in the default directory.  The default
is usually something like 'c:/Program Files/Insightful/splus70/users/yourname'
for S-Plus before 8.0.4.  Beginning with 8.0.4, S-Plus uses the default directory
'c:/Documents and Settings/yourname/My Documents/S-PLUS Projects/Project1'.
Beginning with TIBCO Spotfire S+ Version 8.1.1 for Microsft Windows, the default
directory is 'c:/DOCUME~1/yourname/MYDOCU~1/SPOTFI~1/Project1'.
If you have a HOME environment variable, it will open it there."
  (interactive)
  (let* ((inferior-S+6-multipleinstances "")
	 (ess-S+6-startup-delay 0)) ;; No delay for existing S-Plus
    (S+6-msdos-initiate proc-name))
  (save-excursion
    (set-buffer (car (buffer-list)))    ; get the ESS buffer just created
    (toggle-read-only nil)		; permit writing in ESS buffer
    (goto-char (point-max))
    (beginning-of-line)
    (forward-line -1)
    (insert
     "This is S+6-msdos-existing.
Results of the   !system.command   typed at the S prompt in the
Splus Commands window blink a DOS window and you won't see them.\n\n")
    (toggle-read-only t)		; restore ESS buffer to be read-only
    ))

(defun ess-sqpe-versions-create (ess-SHOME-versions &optional x64)
  "Generate the `M-x splusxy' functions for starting other versions of
Sqpe.  `ESS-SHOME-VERSIONS' is normally taken from
`ess-sqpe-versions', a variable that contains strings that determine which
functions are created.  This works by creating a temp buffer where the
template function `Sqpe+template' is edited by replacing the string
'Sqpe+template' by the version name.  The list of functions actually
created appears in the *ESS* buffer.  If `X64' is not nil, then
modify the function name to show \"-64bit\" in its name.

The result `ess-sqpe-versions-created' will store a list of the new
Sqpe defuns, if any, that were created.  The defuns will normally be
placed on the menubar upon ESS initialisation."
  (let ((beg)
	(versions)
	(version)
	(eval-buf (get-buffer-create "*ess-temp-sqpe-evals*"))
	(ess-sqpe-versions-created)
	)
    ;;
    (save-excursion
      (set-buffer eval-buf)
      ;; clear the buffer.
      (delete-region (point-min) (point-max))

      ;; Find which versions of Sqpe we want.
      (setq x64 (if x64 "-64bit"))
      (setq versions (ess-uniq-list ess-SHOME-versions))
      ;; Iterate over each string in VERSIONS, creating a new defun each time.
      (while versions
	(setq version (car versions)
	      versions (cdr versions))
	(if (file-executable-p version)
	    (progn
	      (setq beg (point))
	      (setq version-function-name (concat (file-name-nondirectory version) x64))
	      (prin1 (symbol-function 'Sqpe+template) eval-buf)
	      (insert "\n\n")
	      (goto-char beg)
	      (while (search-forward "lambda" nil t 1)
		(replace-match
		 (concat "defun " version-function-name)
		 t t))
	      (while (search-forward "ess-SHOME" nil t)
		(replace-match version t t))
	      (goto-char (point-max))
	      (setq ess-sqpe-versions-created
		    (cons version-function-name
			  ess-sqpe-versions-created))
	      (ess-write-to-dribble-buffer
	       (format
		"(Sqpe): ess-sqpe-versions-create making M-x defun %s for %s \n"
		 version-function-name version))
	      )))
      ;; buffer has now been created with defuns, so eval them!
      (eval-buffer)
      (kill-buffer eval-buf))
    ess-sqpe-versions-created))

;; template function used by ess-sqpe-versions-create
(defun Sqpe+template (&optional proc-name)
  "Call 'Sqpe' from 'S-PLUS for Windows ess-SHOME',
the 'Real Thing'  from StatSci.
This function was generated by `ess-sqpe-versions-create'."
  (interactive)
  (setq ess-customize-alist Sqpe+6-customize-alist)
  (let* ((use-dialog-box) ;; MS dialog box won't return a directory
	 (shome-old (getenv "SHOME"))
	 (inferior-Sqpe+6-SHOME-name "ess-SHOME")
	 (inferior-Sqpe+6-program-name (concat "ess-SHOME" "/cmd/sqpe.exe")))
    (setenv "SHOME" "ess-SHOME")
    (ess-write-to-dribble-buffer
     (format "\n(Sqpe+template): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist	 ; change inferior-ess-primary-prompt
	  (append ess-customize-alist
		  '((inferior-ess-primary-prompt   . "^"))))
    (inferior-ess)
    (setq ess-customize-alist Sqpe+6-customize-alist) ; restore i-e-p-p in alist
    (ess-setq-vars-local ess-customize-alist) ; restore i-e-p-p in buffer
    (setq inferior-ess-prompt		; define with correct i-e-p-p
	  ;; Do not anchor to bol with `^'       ; (copied from ess-inf.el)
	  (concat "\\("
		  inferior-ess-primary-prompt
		  "\\|"
		  inferior-ess-secondary-prompt
		  "\\)"))
    (setq comint-prompt-regexp (concat "^" inferior-ess-prompt))
					; define with correct i-e-p-p
    (setq comint-input-sender 'inferior-ess-input-sender)
    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
    (goto-char (point-max))
    (insert "options(interactive=T)")
    (inferior-ess-send-input)
    (setq mode-name "iESS(Sqpe)")
    ;;  (ess-eval-linewise inferior-S+6-editor-pager-command)
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start))
    (setenv "SHOME" shome-old)))


 ; Provide package

(provide 'ess-sp6w-d)

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

;;; ess-sp6w-d.el ends here
