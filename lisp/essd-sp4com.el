;;; essd-sp4com.el
;;; Run S-Plus 4.x, the GUI, with the Commands window in the emacs buffer
;;;
;;; The advantages:
;;;  * S text output is in an emacs buffer and therefore searchable
;;;    using emacs commands.
;;;  * All GUI graphics and buttons are accessible.
;;;
;;; The characteristics of this version of the ESS interface:
;;;  * We can't type in the *S+4* buffer.
;;;  * Prompts do NOT appear in the *S+4* buffer.
;;;  * We can type in the S-Plus Commands window, but no results
;;;    appear there, all results appear in the *S+4* buffer.
;;;  * Echoed input is reformatted by S-Plus.
;;;
;;; 
;;; Uses explicit file S-transcript.st in starting directory.
;;;

;;; Work in progress.


(defun S+4-command ()
  "Run S-Plus 4.x, the GUI, with the commands window in the emacs buffer.\n
Enter `M-x S+4-command'.  S-Plus 4.x will begin.
You will be asked for permission to delete previous `S-transcript.st'.
You must say yes to continue!
Should you say no, then you must rename it manually
 and then reissue `M-x S+4-command'."
  (interactive)
  (S+4-command-startup)	  ; S+4-command-startup uses "&" on the command
  (S+4-tail)
)

(defun S+4-tail ()
  "Put the cursor in an *S+4* buffer and enter `M-x S+4-tail'."
  (interactive)
  (save-excursion
    (set-buffer (car (buffer-list))) ; get the ESS buffer just created
    (shell-command "touch -a S-transcript.st");access time doesn't really exist
    (if (ess-mark-pop-up
	 " *Deletions*" 'delete "S-transcript.st" dired-deletion-confirmer
	 (format "Delete %s " "S-transcript.st"))
	(delete-file "S-transcript.st")
      (error "Move the file `S-transcript.st' manually to somewhere else.
Then place the cursor in the *S+4* buffer and 
issue the command `M-x S+4-tail'"))
    (ess-eval-visibly "options(echo=T);sink(file='S-transcript.st')")
    (toggle-read-only nil)		; permit writing in ESS buffer
    (goto-char (point-max))
    (insert "tail -f S-transcript.st")
    (inferior-ess-send-input)
    (toggle-read-only t)		; restore ESS buffer to be read-only
    (set-variable 'comint-scroll-to-bottom-on-output t)
    (set-variable 'comint-scroll-show-maximum-output t)
    ))

;;; based on dired-mark-pop-up
(defun ess-mark-pop-up (bufname op-symbol files function &rest args)
  ;;"Args BUFNAME OP-SYMBOL FILES FUNCTION &rest ARGS.
  ;;Return FUNCTION's result on ARGS after popping up a window (in a buffer
  ;;named BUFNAME, nil gives \" *Marked Files*\") showing the marked
  ;;files.  Uses function `dired-pop-to-buffer' to do that.
  ;; FUNCTION should not manipulate files.
  ;; It should only read input (an argument or confirmation).
  ;;The window is not shown if there is just one file or
  ;; OP-SYMBOL is a member of the list in `dired-no-confirm'.
  ;;FILES is the list of marked files."
  (or bufname (setq bufname  " *Marked Files*"))
  (if (or (eq dired-no-confirm t)
	  (memq op-symbol dired-no-confirm)
	  (= (length files) 1))
      (apply function args)
    (save-excursion
      (set-buffer (get-buffer-create bufname))
      (erase-buffer)
      (insert files)  ;; replacement line
      (remove-text-properties (point-min) (point-max) '(mouse-face)))
    (save-window-excursion
      (dired-pop-to-buffer bufname)
      (apply function args))))



(defun S+4-command-startup (&optional proc-name)
  "Call 'S-PLUS 4.x', the 'GUI Thing' from StatSci.  Put S-Plus in an
independent MS-Window (Splus persists even if the *S+4 ddeclient*
window is killed in emacs).  Do this by creating a comint process that
calls sh.  Send a shell command in that sh buffer to call Splus.  When
it completes set up a shell as a placeholder in the *S+4 ddeclient*
buffer.  The S-Plus options are correctly set.  In particular, the
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
	  (append ess-customize-alist '((inferior-ess-program   . "sh"))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . "-i"))))
    (let ((s-proj (getenv "S_PROJ")))
      (setenv "S_PROJ" (directory-file-name default-directory))
      (inferior-ess)
      (sleep-for 2) ; need to wait, else working too fast!  The Splus
		    ; command in *S+4 ddeclient* should follow the "$"
		    ; prompt.  If not, then increase the sleep-for time!
      (setenv "S_PROJ" s-proj))
    (setq ess-customize-alist S+4-customize-alist)
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
    (insert (concat inferior-S+4-program-name " "
		    inferior-ess-start-args " &")) ; Note: there is a final "&".
    ;; The completion of the command with "&" permits the "tail -f" to run.
    ;; With the "&", the results of  !system.command  in S get lost.
    ;; Use dos() to get system results.
    (inferior-ess-send-input)
    (sleep-for 30) ; Need to wait, else working too fast!
                   ; If the ess-current-process-name doesn't appear in the
       		   ; Splus Commands window increase the sleep-for time!
    (setq ess-local-process-name ess-current-process-name)
    (ess-eval-visibly (concat "#" ess-current-process-name))
    (beginning-of-buffer)
    (insert
     "This is strictly a transcript buffer.  You can't type anything here.
Split the screen with 'C-x 2' and use 'C-x b RET' to return to your file
in the top half.\n
Anything sent to this process from an S-mode buffer goes
directly to the associated Splus Commands window which sends input and output
back to this buffer.
The S-Plus Commands window does not need to be visible.
Input, but no output, appears in the Commands window.
Any results of the   !system.command   typed at the S prompt in the
Splus Commands window get lost.  Use the dos() command.\n
On completion of your S-Plus session, you will need to kill the tail
process manually, either from a *shell* window or using the ALT-CTRL-DEL
task manager.\n\n")
    (goto-char (point-max))		; comint-mode-map makes *S+4 ddeclient*
;;  (use-local-map comint-mode-map)     ;a shell buffer after Splus is finished.
    (set-buffer-process-coding-system 'raw-text-dos 'raw-text-unix)
    (toggle-read-only t)		; force buffer to be read-only
    (setq mode-name "ddeESSst")))

