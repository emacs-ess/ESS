(defun S+4-msdos (&optional proc-name)
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
	  (append ess-customize-alist '((inferior-ess-program
					 . (getenv "COMSPEC")))))
    (setq ess-customize-alist		; change inferior-ess-primary-prompt
	  (append ess-customize-alist '((inferior-ess-primary-prompt   . "^"))))
    (setq ess-customize-alist		; change inferior-ess-start-args
	  (append ess-customize-alist '((inferior-ess-start-args   . ""))))
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
    (goto-char (point-max))
    (insert (concat inferior-S+4-program-name " "
		    inferior-ess-start-args)) ; Note: there is no final "&".
; Without the "&", the results of  !system.command  come to *S+4 ddeclient*
; With the "&", the results of  !system.command  in S get lost.
    (inferior-ess-send-input)
    (sleep-for 30) ; Need to wait, else working too fast!
                   ; If the ess-current-process-name doesn't appear in the
       		   ; Splus Commands window increase the sleep-for time!
;;; from msdos-minor-mode
  (setq comint-process-echoes t)
  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
;;; end from msdos-minor-mode
    (setq ess-local-process-name ess-current-process-name)
    (ess-eval-visibly (concat "#" ess-current-process-name))
    (beginning-of-buffer)
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
 then M-x C-q exit from the `*S+4 ddeclient* buffer,
or take the risk of not being able to shut down your computer
and suffering through scandisk.\n
Any results of the   !system.command   typed at the S prompt in the
Splus Commands window (are supposed to) appear in this buffer.\n\n")
    (goto-char (point-max))	       ; comint-mode-map makes *S+4 ddeclient*
    (use-local-map comint-mode-map)    ; a shell buffer after Splus is finished.
    (vc-toggle-read-only)	       ; force buffer to be read-only
    ))
