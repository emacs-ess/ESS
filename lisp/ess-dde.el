;;; ess-dde.el --- ESS customization for ddeclients under Windows 9x/NT

;; Copyright (C) 1998--1999 Richard M. Heiberger <rmh@temple.edu>
;; Copyright (C) 2000--2006 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Richard M. Heiberger  <rmh@temple.edu>
;; Created: 9 Dec 1998
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/


;;; Commentary:

;; Code for dealing with running external processes on Windows 9x/NT
;; through ddeclient.

;;; Code:


;; *NO* Requires and autoloads

(defun ess-ddeclient-p ()
  "Returns t iff `ess-local-process-name' is associated with an
inferior-ess-ddeclient, and nil if the ess-process is running as an
ordinary inferior process.  Alway nil on Unix machines."
  (interactive)
  (when ess-microsoft-p
    ;; Debug: C-c C-l fails (to start R or give good message) in Windows
    (ess-write-to-dribble-buffer
     (format "*ddeclient-p: ess-loc-proc-name is '%s'" ess-local-process-name))
    (ess-force-buffer-current "Process to load into: ")
    (not (equal (ess-get-process-variable 'inferior-ess-ddeclient)
                (default-value 'inferior-ess-ddeclient)))))

;; C-c C-r
(defun ess-dde-send-region (proc start end &optional visibly message)
  "Loop through lines in region and send them to ESS via ddeclient.

PROC, VISIBLY and MESSAGE are ignored."
  (setq ;; set the following variables for the current ddeESS process.
   inferior-ess-ddeclient (ess-get-process-variable 'inferior-ess-ddeclient)
   inferior-ess-client-name (ess-get-process-variable 'inferior-ess-client-name)
   inferior-ess-client-command (ess-get-process-variable 'inferior-ess-client-command))
  (narrow-to-region start end)
  (goto-char (point-min))
  (let ((beg))
    (while (or (< (point) (point-max))
               (and (= 1 (point-max))
                    (bound-and-true-p ess-dde-even-empty)))
      (setq beg (point))
      (end-of-line)
      ;; call-process-region won't send over a 0-character line.
      ;; We go outside the loop to create a 1-character line " " in the
      ;; *ESS-temporary* buffer
      (if (= beg (point)) ;; do empty line outside loop
          (ess-eval-linewise-ddeclient " " nil 'eob t)
        ;;(call-process-region start end
        ;;                     "ddeclient" nil nil nil "S-PLUS" "SCommand")
        (call-process-region
         beg (point)
         inferior-ess-ddeclient nil nil nil
         inferior-ess-client-name inferior-ess-client-command))
      (forward-line 1))
    (widen)))

;; C-c C-n
(defun ess-dde-eval-linewise (text-withtabs &optional invisibly eob
                              even-empty wait-last-prompt &rest args)
  (with-current-buffer (get-buffer-create "*ESS-temporary*")
    (ess-setq-vars-local ess-customize-alist (current-buffer))
    (erase-buffer)
    (insert text-withtabs)
    (let ((ess-dde-even-empty even-empty))
      (ess-dde-send-region nil (point-min) (point-max))))
  (when wait-last-prompt
    ess-eval-ddeclient-sleep))          ; in addition to timeout-ms

;; C-c C-v
(defun ess-dde-display-help-on-object (object &rest args)
  "Display the ESS documentation for OBJECT in another window.
If prefix arg is given, forces a query of the ESS process for the help
file.  Otherwise just pops to an existing buffer if it exists."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise-ddeclient (concat "help(" object ")")))

(defun ess-dde-find-help-file (p-string)
  (read-string "Help on: "))

;; C-c C-l
(defun ess-dde-load-file (filename)
  "Load an S source file into an inferior ESS process; alternate behavior for
`ess-load-file', required with S-Plus GUI for Windows: Sends the S-Plus command
source(\"filename\") to S.  This version does not guarantee to save .Last.value,
nor offer alternate buffers or editing capability."
  (let ((source-buffer (get-file-buffer filename)))
    (if (ess-check-source filename)
        (error "Buffer %s has not been saved" (buffer-name source-buffer))
      ;; Find the process to load into
      (if source-buffer
          (with-current-buffer source-buffer
            (ess-force-buffer-current "Process to load into: ")
            ;; (ess-check-modifications) ;;; not possible with ddeclient
            ;; it calls ess-command which requires two-way communication
            ;; with the S-Plus process
            )))
    (ess-eval-linewise-ddeclient (format ess-load-command filename)))
  (widen))


;; C-c C-d
(defun ess-dde-dump-object (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise-ddeclient (concat "dump('" object "','" filename "')"))
  (sleep-for 5)
  (find-file filename)
  (widen))

(defun ess-dde-read-object-name (p-string)
  (read-string "Object to edit: "))

(defun ess-dput-expression-ddeclient (object filename)
  "Dump the ESS object found by evaluating OBJECT into file FILENAME."
  (ess-force-buffer-current "Process to load into: ")
  (ess-eval-linewise-ddeclient (concat "dput(" object ",'" filename "')"))
  (sleep-for 2)
  (find-file filename))

(defun ess-command-ddeclient-proposed (com &optional buf sleep)
  "ddeclient version of real `ess-command'.
Send the ESS process command COM and redirect its output to the
temporary file named BUF.  The temporary filename is constructed
in emacs, not in the ESS process.  The default name for the
temporary buffer is \"ess-temp.st\".  The function waits
SLEEP (which defaults to 1) seconds and then brings the temporary
file into an emacs buffer and displays it."
  (let (filename bufname)
    (if (not buf) (setq buf "ess-temp.st"))
    (if (not sleep) (setq sleep 1))
    (setq filename (concat (file-name-as-directory (getenv "TEMP")) buf))
    (ess-eval-linewise-ddeclient
     (concat ".old.Last.value <- .Last.value; sink('"
             filename
             "'); print("
             com
             "); sink(); .Last.value <- .old.Last.value"))
    (setq bufname (ess-get-file-or-buffer filename)) ;; must follow the eval
    (sleep-for sleep)
    (if (not bufname)
        (find-file filename)
      (switch-to-buffer bufname))
    (revert-buffer t t) ;; this allows the user to reuse the BUF name
    ))

;; previous version (ESS-5.2.12 and earlier)
(defun ess-dde-command (com &optional buf sleep)
  "ddeclient bypass of real ess-command"
  (ess-eval-linewise com))

(provide 'ess-dde)

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

;;; ess-dde.el ends here
