;;; ess-rdired.el --- prototype object browser for R, looks like dired mode.

;; Copyright (C) 2002--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Stephen Eglen <stephen@anc.ed.ac.uk>
;; Created: Thu 24 Oct 2002
;; Maintainers: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is not part of GNU Emacs.

;; ess-rdired.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; ess-rdired.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This provides a dired-like buffer for R objects.  Instead of
;; operating on files, we operate on R objects in the current
;; environment.  Objects can be viewed, edited, deleted, plotted and
;; so on.

;; Installation and usage.
;;
;; Load in this library, e.g. with the command:
;; (autoload 'ess-rdired "ess-rdired" "View *R* objects in a dired-like buffer." t)
;;
;; After loading this file, do "M-x R" to start an R session, then
;; create a few variables:
;; s <- sin(seq(from=0, to=8*pi, length=100))
;; x <- c(1, 4, 9)
;; y <- rnorm(20)
;; z <- TRUE

;; Then in Emacs, do "M-x ess-rdired" and you should see the following in
;; the buffer *R dired*:
;;        mode length
;;   s numeric    100
;;   x numeric      3
;;   y numeric     20
;;   z logical      1

;; Type "?" in the buffer to see the documentation.  e.g. when the
;; cursor is on the line for `s', type 'p' to plot it, or `v' to view
;; its contents in a buffer.  Then type 'd' to mark it for deletion.

;; How it works.

;; Most of the hardwork is done by the R routine .rdired.objects(),
;; which, when called, produces the list of objects in a tidy format.
;; This function is stored within the lisp variable `ess-rdired-objects',
;; and can be altered to provide other information if you so need it.
;; (Martin Maechler suggested providing output from str() here.)

;; Tested on Emacs 21.2, 21.3 pretest and XEmacs 21.1.14, using R 1.6.

;; Todo - compare functionality with ess-mouse-me (ess-mous.el).

;; Todo - How to select alternative environments?  Currently only
;; shows objects in the .GlobalEnv?  See BrowseEnv() in 1.6.x for way
;; of browsing other environments.

;; Todo - problem with fix -- have to wait for fix() command to return
;; before *R* buffer can be used again.  This can get stuck, umm. not
;; sure what is going wrong here.  Maybe add a hook to the temp buffer
;; so that when buffer is killed, we send an instruction to R to
;; update the value of the variable to the contents of the buffer.
;; This way *R* doesn't have to wait.

;; Todo - small bug in .rdired.objects -- if we have a variable called
;; `my.x', its value is replaced by the value of my.x used in the
;; sapply() calls within .rdired.objects().


(defvar ess-rdired-objects ".rdired.objects <- function(objs) {
  if (length(objs)==0) {
    \"No objects to view!\"
  } else {
  mode <- sapply(objs, function(my.x) {
    eval( parse( text=sprintf('data.class(get(\"%s\"))', my.x))) })
  length <- sapply(objs, function(my.x) {
    eval( parse( text=sprintf('length(get(\"%s\"))', my.x))) })
  d <- data.frame(mode, length)
  
  var.names <- row.names(d)

  ## If any names contain spaces, we need to quote around them.
  quotes = rep('', length(var.names))
  spaces = grep(' ', var.names)
  if (any(spaces))
    quotes[spaces] <- '\"'
  var.names = paste(quotes, var.names, quotes, sep='')
  row.names(d) <- paste('  ', var.names, sep='')
  d
  }
}; .rdired.objects(ls())"
  "Function to call within R to print information on objects.  The last
line of this string should be the instruction to call the
function which prints the output for rdired.")

(defvar ess-rdired-buffer "*R dired*"
  "Name of buffer for displaying R objects.")

(defvar ess-rdired-mode-map nil
  "Keymap for the *R dired* buffer.")

(if ess-rdired-mode-map
    ()
  (setq ess-rdired-mode-map (make-sparse-keymap))

  (define-key ess-rdired-mode-map "?" 'ess-rdired-help)
  (define-key ess-rdired-mode-map "d" 'ess-rdired-delete)
  (define-key ess-rdired-mode-map "u" 'ess-rdired-undelete)
  (define-key ess-rdired-mode-map "x" 'ess-rdired-expunge)
  ;; editing requires a little more work.
  ;;(define-key ess-rdired-mode-map "e" 'ess-rdired-edit)
  (define-key ess-rdired-mode-map "v" 'ess-rdired-view)
  (define-key ess-rdired-mode-map "V" 'ess-rdired-View)
  (define-key ess-rdired-mode-map "p" 'ess-rdired-plot)
  (define-key ess-rdired-mode-map "s" 'ess-rdired-sort)
  (define-key ess-rdired-mode-map "q" 'ess-rdired-quit)
  (define-key ess-rdired-mode-map "y" 'ess-rdired-type)	;what type?
  (define-key ess-rdired-mode-map " "  'ess-rdired-next-line)
  (define-key ess-rdired-mode-map [backspace] 'ess-rdired-previous-line)
  (define-key ess-rdired-mode-map "\C-n" 'ess-rdired-next-line)
  (define-key ess-rdired-mode-map "\C-p" 'ess-rdired-previous-line)

  ;; R mode keybindings.
  (define-key ess-rdired-mode-map "\C-c\C-s" 'ess-rdired-switch-process)
  (define-key ess-rdired-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-rdired-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)

  (define-key ess-rdired-mode-map [down] 'ess-rdired-next-line)
  (define-key ess-rdired-mode-map [up] 'ess-rdired-previous-line)
  (define-key ess-rdired-mode-map "g" 'revert-buffer)
  (if (featurep 'xemacs)
      (define-key ess-rdired-mode-map [button2] 'ess-rdired-mouse-view)
    (define-key ess-rdired-mode-map [mouse-2] 'ess-rdired-mouse-view)
    ))

(defun ess-rdired-mode ()
  "Major mode for output from `ess-rdired'.
`ess-rdired' provides a dired-like mode for R objects.  It shows the
list of current objects in the current environment, one-per-line.  You
can then examine these objects, plot them, and so on.
\\{ess-rdired-mode-map}"
  (kill-all-local-variables)
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'ess-rdired-revert-buffer)
  (use-local-map ess-rdired-mode-map)
  (setq major-mode 'ess-rdired-mode)
  (setq mode-name (concat "RDired " ess-local-process-name)))

(defvar ess-rdired-sort-num nil)	;silence the compiler.
;; but see following defun -- maybe it should be buffer local.

(defun ess-rdired ()
  "Run dired-like mode on R objects.
This is the main function.  See documentation for `ess-rdired-mode' though
for more information!"
  (interactive)
  (if (get-buffer ess-rdired-buffer)
      (progn
	(set-buffer ess-rdired-buffer)
	(setq buffer-read-only nil)))

   (ess-execute ess-rdired-objects
		nil
		(substring ess-rdired-buffer 1 (- (length ess-rdired-buffer) 1))
		)

  (pop-to-buffer ess-rdired-buffer)
  ;; When definiting the function .rdired.objects(), a "+ " is printed
  ;; for every line of the function definition; these are deleted
  ;; here.
  (delete-char (* (1- (length (split-string ess-rdired-objects "\n"))) 2))

  ;; todo: not sure how to make ess-rdired-sort-num buffer local?
  ;;(set (make-local-variable 'ess-rdired-sort-num) 2)
  ;;(make-variable-buffer-local 'ess-rdired-sort-num)
  (setq ess-rdired-sort-num 1)
  (ess-rdired-insert-set-properties (save-excursion
				  (goto-char (point-min))
				  (forward-line 1)
				  (point))
				(point-max))
  (setq buffer-read-only t)
  (ess-rdired-mode)
  )

(defun ess-rdired-object ()
  "Return name of object on current line.
Handle special case when object contains spaces."
  (save-excursion
    (beginning-of-line)
    (forward-char 2)

    (cond ((looking-at " ")		; First line?
	   nil)
	  ((looking-at "\"")		; Object name contains spaces?
	   (let (beg)
	     (setq beg (point))
	     (forward-char 1)
	     (search-forward "\"")
	     (buffer-substring-no-properties beg (point))))
	   (t				;should be a regular object.
	    (let (beg)
	      (setq beg (point))
	      (search-forward " ") ;assume space follows object name.
	      (buffer-substring-no-properties beg (1- (point))))))))

(defun ess-rdired-edit ()
  "Edit (fix) the object at point."
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-command (concat "edit(" objname ")\n"))))

(defun ess-rdired-view ()
  "View the object at point."
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-execute (ess-rdired-get objname)
		 nil "R view" )))

(defun ess-rdired-get (name)
  "Generate R code to get the value of the variable name.
This is complicated because some variables might have spaces in their names.
Otherwise, we could just pass the variable name directly to *R*."
  (concat "get(" (ess-rdired-quote name) ")")
  )

(defun ess-rdired-quote (name)
  "Quote name if not already quoted."
  (if (equal (substring name 0 1) "\"")
      name
    (concat "\"" name "\"")))


(defun ess-rdired-View ()
  "View the object at point in its own buffer.
Like `ess-rdired-view', but the object gets its own buffer name."
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-execute
     (ess-rdired-get objname)
     nil (concat "R view " objname ))))

(defun ess-rdired-plot ()
  "Plot the object on current line."
  (interactive)
  (let ((objname (ess-rdired-object)))
    (ess-command (concat "plot(" (ess-rdired-get objname) ")\n"))))

(defun ess-rdired-type ()
  "Run the mode() on command at point.
Named type because of similarity with the dired command bound to
y key."
  (interactive)
  (let ((objname (ess-rdired-object))
	;; create a temp buffer, and then show output in echo area
	(tmpbuf (get-buffer-create "**ess-rdired-mode**")))
    (if objname
	(progn
	  (ess-command (concat "mode(" (ess-rdired-get objname) ")\n")
		       tmpbuf )
	  (set-buffer tmpbuf)
	  (message (concat
		    objname ": "
		    (buffer-substring (+ 4 (point-min)) (1- (point-max)))))
	  (kill-buffer tmpbuf)))))

(defun ess-rdired-delete (arg)
  "Mark the current (or next ARG) objects for deletion.
If point is on first line, all objects are marked for deletion."
  (interactive "p")
  (ess-rdired-mark "D" arg))

(defun ess-rdired-undelete (arg)
  "Unmark the current (or next ARG) objects.
If point is on first line, all objects will be unmarked."
  (interactive "p")
  (ess-rdired-mark " " arg))

(defun ess-rdired-mark (mark-char arg)
  "Mark the object, using MARK-CHAR,  on current line (or next ARG lines)."
  ;; If we are on first line, mark all lines.
  (let ((buffer-read-only nil)
	move)
    (if (eq (point-min)
	    (save-excursion (beginning-of-line) (point)))
	(progn
	  ;; we are on first line, so make a note of point, and count
	  ;; how many objects we want to delete.  Then at end of defun,
	  ;; restore point.
	  (setq move (point))
	  (forward-line 1)
	  (setq arg (count-lines (point) (point-max)))))
    (while (and (> arg 0) (not (eobp)))
      (setq arg (1- arg))
      (beginning-of-line)
      (progn
	(insert mark-char)
	(delete-char 1)
	(forward-line 1)))
    (if move
	(goto-char move))))


(defun ess-rdired-expunge ()
  "Delete the marked objects.
User is queried first to check that objects should really be deleted."
  (interactive)
  (let ((objs "rm(")
	(count 0))
    (save-excursion
      (goto-char (point-min)) (forward-line 1)
      (while (< (count-lines (point-min) (point))
		(count-lines (point-min) (point-max)))
	(beginning-of-line)
	(if (looking-at "^D ")
	    (setq count (1+ count)
		  objs (concat objs (ess-rdired-object) ", " )))
	(forward-line 1)
	))
    (if (> count 0)
	;; found objects to delete
	(progn
	  (setq objs (concat
		      (substring objs 0 (- (length objs) 2))
		      ")\n"))
	  (if (yes-or-no-p (format "Delete %d %s " count
				   (if (> count 1) "objects" "object")))
	      (progn
		(ess-command objs)
		(ess-rdired)
		)))
      ;; else nothing to delete
      (message "no objects set to delete")
      )))

;; Fancy delete method, based on dired.  Bit too much for our needs?
;; (defun ess-rdired-expunge ()
;;   "Delete the marked objects.
;; User is queried first to check that objects should really be deleted."
;;   (interactive)
;;   (let ((objs)
;;	(cmd "rm("))
;;     (save-excursion
;;       (goto-line 2)
;;       (while (< (count-lines (point-min) (point))
;;		(count-lines (point-min) (point-max)))
;;	(beginning-of-line)
;;	(if (looking-at "^D ")
;;	    (progn
;;	      (setq objs (cons (ess-rdired-object) objs ))
;;	      (setq cmd (concat cmd (ess-rdired-object) ", "))
;;	      ))
;;	(forward-line 1)
;;	))
;;     (if (> (length objs) 0)
;;	;; found objects to delete
;;	(if
;;	    (dired-mark-pop-up "*RDired deletions*" 'delete
;;			       objs dired-deletion-confirmer
;;			       (format "delete %s "
;;				       (dired-mark-prompt nil objs)))
;;	    ;; should delete the objects.
;;	    (progn
;;	      (setq cmd (concat (substring cmd 0 (- (length cmd) 2))
;;				")\n"))
;;	      (ess-command cmd)
;;	      (ess-rdired)))
;;       ;; else nothing to delete
;;       (message "no objects set to delete")
;;       )))

(defun ess-rdired-quit ()
  "Quit the R dired buffer."
  (interactive)
  (kill-buffer ess-rdired-buffer))

(defun ess-rdired-revert-buffer (ignore noconfirm)
  "Update the buffer list (in case object list has changed).
Arguments IGNORE and NOCONFIRM currently not used."
  (ess-rdired))

(defun ess-rdired-help ()
  "Show help for `ess-rdired-mode'."
  (interactive)
  (describe-function 'ess-rdired-mode))

(defun ess-rdired-sort ()
  "Sort the rdired output according to one of the columns.
Rotate between the alternative sorting methods."
  (interactive)
  (setq ess-rdired-sort-num (1+ ess-rdired-sort-num))
  (let ((buffer-read-only nil)
	(beg (save-excursion
	       (goto-char (point-min))
	       (forward-line 1)
	       (point)))
	(end (point-max)))
  (if (> ess-rdired-sort-num 3)
      (setq ess-rdired-sort-num 1))
  (cond ((eq ess-rdired-sort-num 1)
	 (sort-fields 1 beg end))
	((eq ess-rdired-sort-num 2)
	 (sort-fields 2 beg end))
	((eq ess-rdired-sort-num 3)
	 (sort-numeric-fields 3 beg end)))))

(defun ess-rdired-next-line (arg)
  "Move down lines then position at object.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line arg)
  (ess-rdired-move-to-object))

(defun ess-rdired-previous-line (arg)
  "Move up lines then position at object.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "p")
  (forward-line (- (or arg 1))) ; -1 if arg was nil
  (ess-rdired-move-to-object))

(defun ess-rdired-move-to-object ()
  "Put point at start of object."
  (beginning-of-line)
  (forward-char 2)
  )

(defun ess-rdired-mouse-view (event)
  "In rdired, visit the object on the line you click on."
  (interactive "e")
  (let (window pos)
    (save-excursion
      (if (featurep 'xemacs)
	  ;; XEmacs
	  (setq window (event-window event)
		pos (event-point event))
	;; Emacs
	(setq window (posn-window (event-end event))
	      pos (posn-point (event-end event))))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (ess-rdired-view))))

(defun ess-rdired-insert-set-properties (beg end)
  "Add mouse highlighting to each object name in the R dired buffer."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (ess-rdired-move-to-object)
      (add-text-properties
       (point)
       (save-excursion
	 (search-forward " ")
	 (1- (point)))
       '(mouse-face highlight
		    help-echo "mouse-2: view object in other window"))
      (forward-line 1))))

(defun ess-rdired-switch-process ()
  "Switch to examine different *R* process.
If you have multiple R processes running, e.g. *R*, *R:2*, *R:3*, you can
use this command to choose which R process you would like to examine.
After switching to a new process, the buffer is updated."
  (interactive)
  (ess-switch-process)
  (ess-rdired))

;;; ess-rdired.el ends here.
