;; ESS-command games that mimic what is currently in ess-iw32.el
;; Today (03/19/01) this doesn't work on R 1.2.1 for Windows
;; It works correctly for S-Plus 4.5 SE and probably for S-Plus 6.
;; I haven't tested it on Unix yet.

(defun ess-command-ddeclient (com &optional buf)
  "ddeclient bypass of real ess-command"
  (ess-eval-linewise com))

(fset 'ess-command-original (symbol-function 'ess-command))

(defun ess-command (com &optional buf)
  "Send the ESS process command COM and delete the output
from the ESS process buffer.  If an optional second argument BUF exists
save the output in that buffer. BUF is erased before use.
COM should have a terminating newline.
Guarantees that the value of .Last.value will be preserved."
  (interactive)
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-command-original com buf)
    (ess-force-buffer-current "Process to load into: ")
    (ess-command-ddeclient com buf))
)


