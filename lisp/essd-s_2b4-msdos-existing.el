(defun S+4-msdos-existing (&optional proc-name)
  "Call 'S-PLUS 4.x', the 'GUI Thing' from StatSci.  Do so by finding
an existing S-Plus in an independent MS-Window (if there is one) and
set up a *S+4 ddeclient* buffer in emacs.  If there is no existing
S-Plus, then a new one will be opened in the default directory,
usually something like c:/Program Files/spls45se/users/yourname.
If you have a HOME environment variable, it will open it there."
  (interactive)
  (let* ((inferior-S+4-multipleinstances ""))
    (S+4-msdos proc-name)))
