;;;; essnt204.el -- NTemacs functions introduced in NTemacs 20.4
;;;; that are needed for essa-sas.el to automatically select the correct
;;;; `ess-sas-submit-method'.  Also, necessary for essl-bug.el.

;; Modified: $Date: 2001/04/26 16:38:46 $
;; Version: $Revision: 5.3 $
;; RCS: $Id: essnt204.el,v 5.3 2001/04/26 16:38:46 ess Exp $

;; These definitions are for Emacs versions < 20.4 or XEmacs
;; These are taken verbatim from the file emacs-20.6/lisp/w32-fns.el
;;
;; Note: 20.3 and 19.x NTemacs users are strongly encouraged to
;; upgrade to version 20.4 or higher.
;;
;; NTemacs 20.2 is not supported by ESS.

;; NTemacs 19.x needs these
(if (not (boundp 'w32-system-shells))
      (defvar w32-system-shells '("cmd" "cmd.exe" "command" "command.com"
				  "4nt" "4nt.exe" "4dos" "4dos.exe"
				  "ndos" "ndos.exe")
	"List of strings recognized as Windows NT/9X system shells.")
)

(if (not (fboundp 'w32-system-shell-p))
      (defun w32-system-shell-p (shell-name)
	(and shell-name
	     (member (downcase (file-name-nondirectory shell-name))
		     w32-system-shells)))
)

(if (not (fboundp 'w32-shell-name))
      (defun w32-shell-name ()
	"Return the name of the shell being used."
	(or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
	    (getenv "ESHELL")
	    (getenv "SHELL")
	    (and (w32-using-nt) "cmd.exe")
	    "command.com"))
)

;; NTemacs 20.3 needs this
(defun w32-shell-dos-semantics ()
  "Return t if the interactive shell being used expects msdos shell semantics."
  (or (w32-system-shell-p (w32-shell-name))
      (and (member (downcase (file-name-nondirectory (w32-shell-name)))
		   '("cmdproxy" "cmdproxy.exe"))
	   (w32-system-shell-p (getenv "COMSPEC")))))


;;; essnt204.el ends here
