;;;; essnt204.el -- NTemacs functions introduced in NTemacs 20.4
;;;; that are needed for essa-sas.el to automatically select the correct
;;;; `ess-sas-submit-method'.

;; Modified: $Date: 2000/07/10 08:00:55 $
;; Version: $Revision: 5.2 $
;; RCS: $Id: essnt204.el,v 5.2 2000/07/10 08:00:55 maechler Exp $

;; These definitions are for users of Windows emacs versions < 20.4
;; These are taken verbatim from the file emacs-20.6/lisp/w32-fns.el
;;
;; Note: 20.3 and 19.x NTemacs users are strongly encouraged to
;; upgrade to the current release.
;;
;; NTemacs 20.2 users are warned that 20.2 is buggy and is not supported by ESS.

;; NTemacs 20.3 needs this
(defun w32-shell-dos-semantics ()
  "Return t if the interactive shell being used expects msdos shell semantics."
  (or (w32-system-shell-p (w32-shell-name))
      (and (member (downcase (file-name-nondirectory (w32-shell-name)))
		   '("cmdproxy" "cmdproxy.exe"))
	   (w32-system-shell-p (getenv "COMSPEC")))))

;; NTemacs 19.x also needs these
(if (< emacs-major-version 20)
    (progn
      (defun w32-system-shell-p (shell-name)
	(and shell-name
	     (member (downcase (file-name-nondirectory shell-name))
		     w32-system-shells)))

      (defun w32-shell-name ()
	"Return the name of the shell being used."
	(or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
	    (getenv "ESHELL")
	    (getenv "SHELL")
	    (and (w32-using-nt) "cmd.exe")
	    "command.com"))


      (defvar w32-system-shells '("cmd" "cmd.exe" "command" "command.com"
				  "4nt" "4nt.exe" "4dos" "4dos.exe"
				  "ndos" "ndos.exe")
	"List of strings recognized as Windows NT/9X system shells.")
))

;;; essnt204.el ends here
