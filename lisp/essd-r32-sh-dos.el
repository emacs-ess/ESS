;b. M-x R-sh-dos
;I normally use bash as the shell within NTemacs.  Therefore I wrote a
;major revision of essd-r.el that will permit Rterm.exe to run inside
;an emacs buffer with bash as the shell.  It does so by generating
;a bash call to command.com in 95/98 or to cmd.exe in NT.
;;;;     command.com /c c:\\Progra~1\\R\\rw0633\\bin\\Rterm.exe --ess  ;; 95/98
;;;;     cmd.exe /c c:\\Progra~1\\R\\rw0633\\bin\\Rterm.exe --ess      ;; NT
;I have tested it on 95, not on NT.
;


;;; Commentary:
;;; This file defines all the R customizations for ess-mode in Window 9x/NT
;;; (a) R         Run R in an emacs buffer when emacs has MS-DOS as the shell.
;;; (b) R-sh-dos  Run R in an emacs buffer when emacs has bash as the shell.
;;;
;;; Windows 95/98/NT    MS-DOS as the shell
;;; You take on the liability of not simultaneously running a *shell*
;;; window and an R process.  You are limited to one R process.
;;; You must quit R with "q()" or you take the risk of not being able
;;; to shut down the computer cleanly.
;;;
;;;
;;; Windows 95/98/NT    bash as the shell
;;; need to run command.com inside a bash buffer with argument
;;;     command.com /c c:\\Progra~1\\R\\rw0633\\bin\\Rterm.exe --ess  ;; 95/98
;;;     cmd.exe /c c:\\Progra~1\\R\\rw0633\\bin\\Rterm.exe --ess      ;; NT
;;; As a side-effect of running MS-DOS inside a bash shell, it is
;;; possible to have more than one R process running simultaneously.
;;; You must quit R with "q()" or you take the risk of not being able
;;; to shut down the computer cleanly.


(defun R-sh-dos (&optional start-args)
  "Call 'R', the GNU 'S clone' from Robert & Ross (Auckland, NZ),
from an NTemacs using bash as the shell."
  (interactive "P")
  (setq ess-customize-alist R-customize-alist)
  ;; for debugging only
  (ess-write-to-dribble-buffer
   (format 
    "\n(R-sh-dos): ess-dialect=%s, buf=%s, start-arg=%s\n\t current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args
    current-prefix-arg))
    (setq ess-customize-alist		; change inferior-ess-program
	  (append ess-customize-alist '((inferior-ess-program   . "sh"))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . "-i"))))
  (let* ((r-always-arg
	  (if (or (equal window-system 'w32) (equal window-system 'win32))
	      "--ess "  "--no-readline "))
	 (r-start-args 
	  (concat r-always-arg
		  (if start-args
		      (read-string
		       (concat "Starting Args? "))
		    nil)))
	 (R-command (concat (if (w32-using-nt) "cmd.exe /c" "command.com /c ")
			    (dos-name-4slash inferior-R-program-name)
			    " " r-start-args)))
    (inferior-ess)
    (sleep-for 2) ; need to wait, else working too fast!  The command.com
		  ; command in *R* should follow the "$"
		  ; prompt.  If not, then increase the sleep-for time!
    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
    (setq ess-customize-alist R-customize-alist) ; restore i-e-p-p in alist
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
    (goto-char (point-max))
    (ess-write-to-dribble-buffer
     (format 
      "\n(R-sh-dos): ess-dialect=%s, buf=%s, R-command=%s\n"
      ess-dialect (current-buffer) R-command))
    (insert R-command)
    (inferior-ess-send-input))
  (goto-char (point-min))
  (insert (concat
"R-mode in ESS version " ess-version
" for Windows 95/98/NT is early in its development.

You MUST exit R with \"q()\"!  Should you forget, you will be unable
to shut down Windows properly and will have to suffer through scandisk
on the next reboot of the system.
 
The R Graphics devices are created on screen.  You may need to reveal
them by resizing and moving both the emacs frame and the R Graphics window.
The graphics window gets focus only when locator() or identify() are executed.

The system() command does not work.\n\n\n"))
;    (comint-set-process-mark)
  (goto-char (point-max))
)



;;; this can sit in essd-r32 for now.  It really belongs to emacs/.../w32-fns.el
(defun dos-name-4slash (filename)
  "Return FILENAME in a 4-slash canonicalized form for use as an
argument to a dos function running inside a bash window."
  (let* ((name (untranslated-canonical-name filename))
	 (split-name (split-string name "/"))
	 (rev-split-name (reverse split-name))
	 (dirname (reverse (cdr rev-split-name)))
	 (basename (list (car rev-split-name)))
	 (dosdir (mapcar (lambda (str) (concat str "\\\\")) dirname))
	 (dosname (apply 'concat (append dosdir basename))))
    dosname))
