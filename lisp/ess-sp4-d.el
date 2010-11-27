;;; ess-sp4-d.el --- S-PLUS 4.x customization

;; Copyright (C) 1998--2002 Richard M. Heiberger <rmh@temple.edu>
;; Copyright (C) 2003--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Richard M. Heiberger <rmh@temple.edu>
;; Created: December 1998
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; Keywords: start up, configuration.

;; This file is part of ESS.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.	If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; This file defines all the S-PLUS 4.x customizations for ess-mode
;;; with ddeclient.

;;; Requires and Autoloads:

(require 'ess-s-l)
;;NO: this is autoloaded from other places (require 'ess-dde)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode	"ess-mode" "Edit an ESS process.")

;;; Code:

(defvar S+4-dialect-name "S+4"
  "Name of 'dialect' for S-PLUS 4.x.");easily changeable in a user's .emacs

(defvar inferior-S+4-multipleinstances "/MULTIPLEINSTANCES"
  "Default \"/MULTIPLEINSTANCES\" opens up a new instance of S+4 in a
GUI window and connects it to the '(ddeESS [S+4])' window.  The
alternative nil uses an existing S+4 GUI (if there is one) and
connects it to the '(ddeESS [S+4])' window.")

(defvar S+4-customize-alist
  (append
   '((ess-local-customize-alist		. 'S+4-customize-alist)
     (ess-dialect			. S+4-dialect-name)
     (ess-loop-timeout			. ess-S-loop-timeout);fixme: dialect spec.
     (ess-object-name-db-file		. "ess-sp4-namedb.el" )
     (inferior-ess-program		. inferior-S+4-program-name)
     (inferior-ess-help-command		. "help(\"%s\")\n")
     (inferior-ess-help-filetype . "chm")

     (inferior-ess-start-file		. nil) ;"~/.ess-S+4")
     (inferior-ess-start-args		. (concat
					   inferior-S+4-multipleinstances " "
					   inferior-S+4-print-command
					   " S_PROJ="
					   (directory-file-name default-directory)))
     ;;	   (inferior-ess-ddeclient	. "ddeclient")
     ;;	   (inferior-ess-client-name	. "S-PLUS")
     ;;	   (inferior-ess-client-command	. "SCommand")
     (ess-STERM	 . "ddeESS")
     )
   S+common-cust-alist)

  "Variables to customize for S+4")

(defvar Sqpe+4-customize-alist
  (append
   '((ess-local-customize-alist		. 'Sqpe+4-customize-alist)
     (ess-dialect			. S+4-dialect-name)
     (ess-loop-timeout			. 500000 );fixme: dialect specific custom.v
     (ess-object-name-db-file		. "ess-sp4-namedb.el" )
     (inferior-ess-program		. inferior-Sqpe+4-program-name)
     (inferior-ess-help-command		. "help(\"%s\")\n")
     (inferior-ess-help-filetype . "chm")
     (inferior-ess-search-list-command	. "searchPaths()\n")
     (inferior-ess-start-file		. nil) ;"~/.ess-S+4")
     (inferior-ess-language-start . (concat
				     "options("
				     "STERM='" ess-STERM "'"
				     (if ess-editor
					 (concat ", editor='" ess-editor "'"))
				     (if ess-pager
					 (concat ", pager='"  ess-pager	 "'"))
				     ")"))
     (ess-STERM	 . "iESS")
     )
   S+common-cust-alist)

  "Variables to customize for Sqpe+4.")


;;; There are extra complications in S+4 (compared to S+3) because
;;;
;;; (1) The StatSci supplied Splus.exe doesn't work in an emacs
;;;	buffer.	 It works as as a GUI window and we must send commands
;;;	to it through ddeclient.  Nonetheless, we need to give it a
;;;	process name and be sure that that there is a valid running
;;;	process in the '(ddeESS [S+4])' buffer.	 Therefore we create an
;;;	ESS process in the buffer as a placeholder and start a shell
;;;	in the ESS buffer.  From the shell we start Splus.  Once Splus
;;;	finishes initializing and kills the original shell, we start
;;;	another shell.	We have a buffer-local variable
;;;	inferior-ess-ddeclient, initialized to nil.  When there is a
;;;	non-nil value of inferior-ess-ddeclient we send lines to
;;;	inferior-ess-ddeclient rather than to the Splus process.
;;; (2) There is no Splus process running in the '(ddeESS [S+4])'
;;;	buffer.	 Therefore inferior-ess will never see a prompt,
;;;	unless we first change it to the null prompt "^".  Then once
;;;	the process has started, we change it back.
;;; (3) When M-x S+4 starts Splus by a shell command, then Splus is an
;;;	independent process and will be survive if the '(ddeESS [S+4])'
;;;	buffer is killed (or emacs is quit).  The '(ddeESS [S+4])' is
;;;	made read-only and a warning is placed in it saying that "You
;;;	can't type anything here."  Actually, if the standalone Splus
;;;	is killed and the '(ddeESS [S+4])' is made writable (C-x C-q),
;;;	then '(ddeESS [S+4])' becomes a shell buffer.
;;;
(defun S+4 (&optional proc-name)
  "Call 'S-PLUS 4.x', the 'GUI Thing' from StatSci.  Put S-Plus in an
independent MS-Window (Splus persists even if the '(ddeESS [S+4])'
window is killed in emacs).  Do this by creating a comint process that
calls sh.  Send a shell command in that sh buffer to call Splus.  When
it completes set up a shell as a placeholder in the '(ddeESS [S+4])'
buffer.	 The S-Plus options are correctly set.	In particular, the
S-Plus Commands window is opened if the Options/General
Settings/Startup menu says it should be.  There is a 30 second delay
during startup in which the screen will not be refreshed.  This delay
is here to allow slow disks to start the Splus program."
  (interactive)
  (save-excursion
    (setq ess-customize-alist S+4-customize-alist)
    (ess-write-to-dribble-buffer
     (format "\n(S+4): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist		; change inferior-ess-program
	  (append ess-customize-alist '((inferior-ess-program	. "sh"))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . "-i"))))
    (let ((s-proj (getenv "S_PROJ"))
	  (manpath (getenv "MANPATH")))
      (cd (w32-short-file-name (directory-file-name default-directory)))
      (setenv "S_PROJ" default-directory)
      ;; I don't know why this PATH/MANPATH game is needed,
      ;; except that it doesn't work without it.
      (setenv "MANPATH" (getenv "PATH"))
      (inferior-ess)
      (sleep-for 2) ; need to wait, else working too fast!  The Splus
		    ; command in '(ddeESS [S+4])' should follow the "$"
		    ; prompt.  If not, then increase the sleep-for time!
      (setenv "MANPATH" manpath)
      (setenv "S_PROJ" s-proj))
    (setq ess-customize-alist S+4-customize-alist)
    (ess-setq-vars-local ess-customize-alist)
;;; the next three lines belong in customize-alist, but can't be there
;;; because of the broken ess-setq-vars-default usage in ess-inf.el
    (setq inferior-ess-ddeclient	 "ddeclient")
    (setq inferior-ess-client-name	 "S-PLUS")
    (setq inferior-ess-client-command	 "SCommand")
;;; end of what belongs in customize-alist
    (setq comint-process-echoes nil)
    (setq comint-input-sender 'comint-simple-send)
    (goto-char (point-max))
    (insert (concat inferior-S+4-program-name " "
		    inferior-ess-start-args)) ; Note: there is no final "&".
    ;; Without the "&", the results of	!system.command	 come to '(ddeESS [S+4])'
    ;; With the "&", the results of  !system.command  in S get lost.
    (inferior-ess-send-input)
    (sleep-for 30) ; Need to wait, else working too fast!
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
    (goto-char (point-max))		; comint-mode-map makes '(ddeESS [S+4])'
;;  (use-local-map comint-mode-map)	;a shell buffer after Splus is finished.
    (set-buffer-process-coding-system 'raw-text-dos 'raw-text-unix)
    (toggle-read-only t)		; force buffer to be read-only
    (setq mode-name "ddeESS")
;;  (ess-eval-linewise inferior-S+4-editor-pager-command)
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start))
    ))




(defun S+4-existing (&optional proc-name)
  "Call 'S-PLUS 4.x', the 'GUI Thing' from StatSci.  Do so by finding
an existing S-Plus in an independent MS-Window (if there is one) and
set up a '(ddeESS [S+4])' buffer in emacs.  If there is no existing
S-Plus, then a new one will be opened in the default directory,
usually something like c:/Program Files/spls45se/users/yourname.
If you have a HOME environment variable, it will open it there."
  (interactive)
  (let* ((inferior-S+4-multipleinstances " & # ")) ; Note: there is a final "&".
    ;; Without the "&", there is a core dump.
    ;; With the "&", the results of  !system.command  in S get lost.
    ;; We are picking up an existing S-Plus process for sending to.
    ;; It doesn't know about us, so nothing comes back.
    (S+4 proc-name))
  (save-excursion
    (set-buffer (car (buffer-list)))	; get the ESS buffer just created
    (toggle-read-only nil)		; permit writing in ESS buffer
    (goto-char (point-max))
    (beginning-of-line)
    (forward-line -1)
    (insert
     "This is S+4-existing.
Results of the	 !system.command   typed at the S prompt in the
Splus Commands window blink a DOS window and you won't see them.\n\n")
    (toggle-read-only t)		; restore ESS buffer to be read-only
    ))


;;; There are extra complications in Sqpe+4 (compared to S+3) because
;;; (1) The StatSci supplied Sqpe.exe won't work without SHOME as an
;;;	environment variable and Sqpe does not take command line
;;;	arguments and
;;; (2) Sqpe.exe comes up with options(interactive=F), which means it
;;;	doesn't provide prompts by default, and we must change it to T so
;;;	it will provide prompts.
;;;
(defun Sqpe+4 (&optional proc-name)
  "Call 'Sqpe' from 'S-PLUS 4.x', the 'Real Thing'  from StatSci."
  (interactive)
  (setq ess-customize-alist Sqpe+4-customize-alist)
  (let* ((shome-nil-p (equal (getenv "SHOME") nil)))
    (if shome-nil-p (setenv "SHOME" inferior-Sqpe+4-SHOME-name))
    (ess-write-to-dribble-buffer
     (format "\n(Sqpe+4): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (inferior-ess)
    (setq ess-customize-alist Sqpe+4-customize-alist) ; restore i-e-p-p in alist
    (ess-setq-vars-local ess-customize-alist)	 ; restore i-e-p-p in buffer
    (setq inferior-ess-prompt			 ; define with correct i-e-p-p
	  ;; Do not anchor to bol with `^'	 ; (copied from ess-inf.el)
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
;;  (ess-eval-linewise inferior-S+4-editor-pager-command)
    (if inferior-ess-language-start
	(ess-eval-linewise inferior-ess-language-start))
    (if shome-nil-p (setenv "SHOME" nil))))



(defun S+4-mode (&optional proc-name)
  "Major mode for editing S+4 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+4-customize-alist)
  (ess-mode S+4-customize-alist proc-name)
  (if ess-imenu-use-S (ess-imenu-R)))

(defun S+4-transcript-mode ()
  "S-PLUS 4.x transcript mode."
  (interactive)
  (ess-transcript-mode S+4-customize-alist))


(defun S+4-msdos (&optional proc-name)
  "Call 'S-PLUS 4.x', the 'GUI Thing' from StatSci.  Put S-Plus in an
independent MS-Window (Splus persists even if the '(ddeESS [S+4])'
window is killed in emacs).  Do this by creating a comint process that
calls sh.  Send a shell command in that sh buffer to call Splus.  When
it completes set up a shell as a placeholder in the '(ddeESS [S+4])'
buffer.	 The S-Plus options are correctly set.	In particular, the
S-Plus Commands window is opened if the Options/General
Settings/Startup menu says it should be.  There is a 30 second delay
during startup in which the screen will not be refreshed.  This delay
is here to allow slow disks to start the Splus program."
  (interactive)
  (save-excursion
    (setq ess-customize-alist S+4-customize-alist)
    (ess-write-to-dribble-buffer
     (format "\n(S+4): ess-dialect=%s, buf=%s\n" ess-dialect
	     (current-buffer)))
    (setq ess-customize-alist		; change inferior-ess-program
	  (append ess-customize-alist '((inferior-ess-program
					 . (getenv "COMSPEC")))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . ""))))
    (let ((s-proj (getenv "S_PROJ")))
      (cd (w32-short-file-name (directory-file-name default-directory)))
      (setenv "S_PROJ" default-directory)
      (inferior-ess)
      (sleep-for 2) ; need to wait, else working too fast!  The Splus
		    ; command in '(ddeESS [S+4])' should follow the "$"
		    ; prompt.  If not, then increase the sleep-for time!
      (setenv "S_PROJ" s-proj))
    (setq ess-customize-alist S+4-customize-alist)
    (ess-setq-vars-local ess-customize-alist)
;;; the next three lines belong in customize-alist, but can't be there
;;; because of the broken ess-setq-vars-default usage in ess-inf.el
    (setq inferior-ess-ddeclient	 "ddeclient")
    (setq inferior-ess-client-name	 "S-PLUS")
    (setq inferior-ess-client-command	 "SCommand")
;;; end of what belongs in customize-alist
    (setq comint-input-sender 'comint-simple-send)
    (setq comint-process-echoes nil)
    (set-buffer-process-coding-system 'raw-text-dos 'raw-text-dos)
    (goto-char (point-max))
    (insert (concat inferior-S+4-program-name " "
		    inferior-ess-start-args)) ; Note: there is no final "&".
; Without the "&", the results of  !system.command  come to '(ddeESS [S+4])'
; With the "&", the results of	!system.command	 in S get lost.
    (inferior-ess-send-input)
    (sleep-for 30) ; Need to wait, else working too fast!
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
Use 'C-x b RET' to return to your file.\n
Anything sent to this process from an S-mode buffer goes
directly to the associated Splus Commands window.\n
The S-Plus Commands window must be visible.
You may need to open the S-Plus Commands window manually
(by clicking on Splus/Window/Commands Window).\n
There is a 30 second delay when this program starts during which the
emacs screen will be partially blank.\n
Remember to
`q()' from S-Plus and
 then C-x C-q exit from the `'(ddeESS [S+4])'' buffer,
or take the risk of not being able to shut down your computer
and suffering through scandisk.\n
Any results of the   !system.command   typed at the S prompt in the
Splus Commands window (are supposed to) appear in this buffer.\n\n")
    (goto-char (point-max))	       ; comint-mode-map makes '(ddeESS [S+4])'
    (use-local-map comint-mode-map)    ; a shell buffer after Splus is finished.
    (toggle-read-only t)	       ; force buffer to be read-only
    (setq mode-name "ddeESS")
;;  (ess-eval-linewise inferior-S+4-editor-pager-command)
    (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start))
    ))

(defun S+4-msdos-existing (&optional proc-name)
  "Call 'S-PLUS 4.x', the 'GUI Thing' from StatSci.  Do so by finding
an existing S-Plus in an independent MS-Window (if there is one) and
set up a '(ddeESS [S+4])' buffer in emacs.  If there is no existing
S-Plus, then a new one will be opened in the default directory,
usually something like c:/Program Files/spls45se/users/yourname.
If you have a HOME environment variable, it will open it there."
  (interactive)
  (let* ((inferior-S+4-multipleinstances ""))
    (S+4-msdos proc-name))
  (save-excursion
    (set-buffer (car (buffer-list)))	; get the ESS buffer just created
    (toggle-read-only nil)		; permit writing in ESS buffer
    (goto-char (point-max))
    (beginning-of-line)
    (forward-line -1)
    (insert
     "This is S+4-msdos-existing.
Results of the	 !system.command   typed at the S prompt in the
Splus Commands window blink a DOS window and you won't see them.\n\n")
    (toggle-read-only t)		; restore ESS buffer to be read-only
    ))

 ; Provide package

(provide 'ess-sp4-d)

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

;;; ess-sp4-d.el ends here
