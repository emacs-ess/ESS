;;; essd-iw32.el --- ESS customization for ddeclients under Windows 9x/NT

;; Copyright (C) 1998--1999 Richard M. Heiberger <rmh@fisher.stat.temple.edu>
;; Copyright (C) 2000--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Richard M. Heiberger  <rmh@fisher.stat.temple.edu>
;; Created: 9 Dec 1998
;; Maintainers: ESS-core <ESS-core@stat.math.ethz.ch>

;; This file is part of ESS

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

;; Code for dealing with running external processes on Windows 9x/NT
;; through ddeclient.


;;; Code:

 ; Requires and autoloads

(require 'ess-mode)
(require 'ess-inf)
(require 'ess-help)


;; C-c C-r
(defun ess-eval-region-ddeclient (start end toggle &optional message even-empty)
  "Loop through lines in region and send them to ESS via ddeclient.
The prefix argument is ignored when ddeclient is used"
  (setq inferior-ess-ddeclient
	(ess-get-process-variable
	 ess-current-process-name 'inferior-ess-ddeclient))
  (setq inferior-ess-client-name
	(ess-get-process-variable
	 ess-current-process-name 'inferior-ess-client-name))
  (setq inferior-ess-client-command
	(ess-get-process-variable
	 ess-current-process-name 'inferior-ess-client-command))
  (narrow-to-region start end)
  (beginning-of-buffer)
  (let ((beg))
    (while (or (< (point) (point-max))
	       (and (= 1 (point-max)) even-empty))
      (setq beg (point))
      (end-of-line)
      ;; call-process-region won't send over a 0-character line.
      ;; We go outside the loop to create a 1-character line " " in the
      ;; *ESS-temporary* buffer
      (if (= beg (point))  ;; do empty line outside loop
	    (ess-eval-linewise-ddeclient " " nil 'eob t)
	;;(call-process-region start end
	;;                     "ddeclient" nil nil nil "S-PLUS" "SCommand")
	(call-process-region
	 beg (point)
	 inferior-ess-ddeclient nil nil nil
	 inferior-ess-client-name inferior-ess-client-command))
      (forward-line 1))
    (widen)))

(fset 'ess-eval-region-original (symbol-function  'ess-eval-region))

(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior ESS process.  This is the
MS-Windows version of `ess-eval-region'.  When used with S-Plus 4.x or
S-Plus 2000 the prefix argument is ignored; see the documentation for
`ess-eval-region-ddeclient'.  When used with other ESS programs the
prefix argument will toggle meaning of `ess-eval-visibly-p'; see the
documentation for `ess-eval-region-original'."
  (interactive "r\nP")
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-eval-region-original start end toggle message)
    (ess-force-buffer-current "Process to load into: ")
    (ess-eval-region-ddeclient start end toggle message t))
)



;;; switch between Splus by ddeclient and Splus running in an emacs buffer
(defun ess-eval-linewise-ddeclient
  (text-withtabs &optional invisibly eob even-empty)
  (save-excursion
    (set-buffer (get-buffer-create "*ESS-temporary*"))
    (ess-setq-vars-local ess-customize-alist (current-buffer))
    (erase-buffer)
    (insert text-withtabs)
    (ess-eval-region-ddeclient (point-min) (point-max) t t even-empty)))

(fset 'ess-eval-linewise-original (symbol-function  'ess-eval-linewise))

(defun ess-eval-linewise (text-withtabs &optional invisibly eob even-empty)
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
             (default-value 'inferior-ess-ddeclient))
      (ess-eval-linewise-original text-withtabs invisibly eob even-empty)
      (ess-eval-linewise-ddeclient text-withtabs invisibly eob even-empty)))


;; C-c C-v
;;; this works for Sqpe+4 and S+4
(defun ess-display-help-on-object-ddeclient (object)
  "Display the ESS documentation for OBJECT in another window.
If prefix arg is given, forces a query of the ESS process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise (concat "help(" object ")")))


(fset 'ess-display-help-on-object-original
      (symbol-function  'ess-display-help-on-object))

(defun ess-display-help-on-object (object)
  (interactive "sHelp on: ")
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-display-help-on-object-original object)
    (ess-display-help-on-object-ddeclient object))
  (widen))



;;; Alternate version of ess-load-file, required with S+4.
;;; This version sends the S-Plus command
;;;         source("filename")
;;; to S.  This version does not guarantee to save .Last.value
;;; This version does not offer alternate buffers or editing capability.

;; C-c C-l
;;; this works for Sqpe+4 and S+4
(defun ess-load-file-ddeclient (filename)
  "Load an S source file into an inferior ESS process."
  ;; (require 'ess-inf) ; (rmh) not needed in function.  require is on the file.
  (ess-make-buffer-current)
  (let ((source-buffer (get-file-buffer filename)))
    (if (ess-check-source filename)
	(error "Buffer %s has not been saved" (buffer-name source-buffer))
      ;; Find the process to load into
      (if source-buffer
	  (save-excursion
	    (set-buffer source-buffer)
	    (ess-force-buffer-current "Process to load into: ")
	    ;; (ess-check-modifications) ;;; not possible with ddeclient
	    ;; it calls ess-command which requires two-way communication
	    ;; with the S-Plus process
	    )))
    (ess-eval-linewise (format inferior-ess-load-command filename))))

(fset 'ess-load-file-original
      (symbol-function  'ess-load-file))

(defun ess-load-file (filename)
"Alternate version of `ess-load-file', required with S+4.
This version sends the S-Plus command
     source(\"filename\")
to S.  This version does not guarantee to save .Last.value
This version does not offer alternate buffers or editing capability."
     (interactive (list
		   (or
		    (and (eq major-mode 'ess-mode)
			 (buffer-file-name))
		    (expand-file-name
		     (read-file-name "Load S file: " nil nil t)))))
     (if (equal (ess-get-process-variable
		 ess-current-process-name 'inferior-ess-ddeclient)
		(default-value 'inferior-ess-ddeclient))
	 (ess-load-file-original filename)
       (ess-load-file-ddeclient filename))
     (widen))

;; C-c C-d
(defun ess-dump-object-ddeclient (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise (concat "dump('" object "','" filename "')"))
  (sleep-for 5)
  (find-file filename))


(fset 'ess-dump-object-original
      (symbol-function  'ess-dump-object))

(defun ess-dump-object (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-dump-object-original object filename)
    (ess-dump-object-ddeclient object filename))
  (widen))




(fset 'ess-dump-object-into-edit-buffer-original
      (symbol-function  'ess-dump-object-into-edit-buffer))

(defun ess-dump-object-into-edit-buffer (object)
  "Dump the ESS object OBJECT into file FILENAME."
  (interactive
   (progn
     (ess-force-buffer-current "Process to dump from: ")
     (list (read-string "Object to edit: "))))
  (if (equal (ess-get-process-variable
	      ess-current-process-name 'inferior-ess-ddeclient)
	     (default-value 'inferior-ess-ddeclient))
      (ess-dump-object-into-edit-buffer-original object)
    (ess-dump-object-into-edit-buffer-ddeclient object))
  (widen))




(defun ess-dump-object-into-edit-buffer-ddeclient (object)
  "Edit an ESS object in its own buffer.

Without a prefix argument, this simply finds the file pointed to by
`ess-source-directory'. If this file does not exist, or if a
prefix argument is given, a dump() command is sent to the ESS process to
generate the source buffer."
  (interactive
   (progn
     (ess-force-buffer-current "Process to dump from: ")
     (ess-read-object-name "Object to edit: ")))
  (let* ((dirname (file-name-as-directory
		   (if (stringp ess-source-directory)
		       ess-source-directory
		     (save-excursion
		       (set-buffer
			(process-buffer (get-ess-process
					 ess-local-process-name)))
		       (ess-setq-vars-local ess-customize-alist)
		       (apply ess-source-directory nil)))))
	 (filename (concat dirname (format ess-dump-filename-template object)))
	 (old-buff (get-file-buffer filename)))

    ;; If the directory doesn't exist, offer to create it
    (if (file-exists-p (directory-file-name dirname)) nil
      (if (y-or-n-p	; Approved
	   (format "Directory %s does not exist. Create it? " dirname))
	  (make-directory (directory-file-name dirname))
	(error "Directory %s does not exist." dirname)))

    ;; Three options:
    ;;  (1) Pop to an existing buffer containing the file in question
    ;;  (2) Find an existing file
    ;;  (3) Create a new file by issuing a dump() command to S
    ;; Force option (3) if there is a prefix arg

    (if current-prefix-arg
	(ess-dump-object object filename)
      (if old-buff
	  (progn
	    (pop-to-buffer old-buff)
	    (message "Popped to edit buffer."))
	;; No current buffer containing desired file
	(if (file-exists-p filename)
	    (progn
	      (ess-find-dump-file-other-window filename)
	      (message "Read %s" filename))
	  ;; No buffer and no file
	  (ess-dump-object object filename))))))

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
  (if buf                ;;Mar 01 2002 rmh
      (save-excursion
	(set-buffer buf)
	(setq ess-local-process-name ess-current-process-name)))
  (if (not (ess-ddeclient-p))
      (ess-command-original com buf)
    (ess-force-buffer-current "Process to load into: ")
    (ess-command-ddeclient com buf)))

(provide 'ess-iw32)

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
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-iw32.el ends here
