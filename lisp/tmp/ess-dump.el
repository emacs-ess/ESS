;;; ess-dump.el --- Getting objects into text files for editing

;; Copyright (C) 2000--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author:  A.J. Rossini <rossini@u.washington.edu>
;; Created: 3 Sept 2000
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

;; Code for sending statistical objects to text files for editing.
;;
;; THIS IS STRICTLY NOT USED, BUT IS FOR PLANNING.

;;; Code:

 ; Requires and autoloads

(require 'ess)

(defun ess-check-source (fname)
  "If file FNAME has an unsaved buffer, offer to save it.
Returns t if the buffer existed and was modified, but was not saved."
  (let ((buff (get-file-buffer fname)))
    ;; RMH: Corrections noted below are needed for C-c C-l to work
    ;; correctly when issued from *S* buffer.
    ;; The following barfs since
    ;; 1. `if' does not accept a buffer argument, `not' does.
    ;; 2. (buffer-file-name) is not necessarily defined for *S*
    ;;(if buff
    ;; (let ((deleted (not (file-exists-p (buffer-file-name)))))
    ;; Next 2 lines are RMH's solution:
    (if (not(not buff))
        (let ((deleted (not (file-exists-p fname))))
          (if (and deleted (not (buffer-modified-p buff)))
              ;; Buffer has been silently deleted, so silently save
              (save-excursion
                (set-buffer buff)
                (set-buffer-modified-p t)
                (save-buffer))
            (if (and (buffer-modified-p buff)
                     (or ess-mode-silently-save
                         (y-or-n-p
                          (format "Save buffer %s first? "
                                  (buffer-name buff)))))
                (save-excursion
                  (set-buffer buff)
                  (save-buffer))))
          (buffer-modified-p buff)))))


(defun ess-dump-object-into-edit-buffer (object)
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
      (if (y-or-n-p     ; Approved
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

(defun ess-dump-object (object filename)
  "Dump the ESS object OBJECT into file FILENAME."
  (let ((complete-dump-command (format inferior-ess-dump-command
                                       object filename)))
    (if (file-writable-p filename) nil
      (error "Can't dump %s as %f is not writeable." object filename))

    ;; Make sure we start fresh
    (if (get-file-buffer filename)
        (or (kill-buffer (get-file-buffer filename))
            (error "Aborted.")))

    (ess-command complete-dump-command)
    (message "Dumped in %s" filename)

    (ess-find-dump-file-other-window filename)

    ;; PD, 1Apr97
    ;;This ensures that the object gets indented according to ess-mode,
    ;;not as the R/S deparser does it. At the same time, it gets rid
    ;;of the mess generated by sending TAB characters to the readline
    ;;functions in R when you eval-buffer-*.
    (indent-region (point-min-marker) (point-max-marker) nil)
    (set-buffer-modified-p nil)

    ;; Don't make backups for temporary files; it only causes clutter.
    ;; The ESS object itself is a kind of backup, anyway.
    (if ess-keep-dump-files nil
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil))

    ;; Don't get confirmation to delete dumped files when loading
    (if (eq ess-keep-dump-files 'check)
        (setq ess-keep-dump-files nil))

    ;; Delete the file if necessary
    (if ess-delete-dump-files
        (delete-file (buffer-file-name)))))

(defun ess-find-dump-file-other-window (filename)
  "Find ESS source file FILENAME in another window."

  (if (file-exists-p filename) nil
    (ess-write-to-dribble-buffer
     (format "%s does not exist. Bad dump, starting fresh." filename)))

  ;; Generate a buffer with the dumped data
  (find-file-other-window filename)
  (ess-mode ess-customize-alist)

  (auto-save-mode 1)            ; Auto save in this buffer
  (setq ess-local-process-name ess-current-process-name)

  (if ess-function-template
      (progn
        (goto-char (point-max))
        (if (re-search-backward ess-dumped-missing-re nil t)
            (progn
              (replace-match ess-function-template t t)
              (set-buffer-modified-p nil) ; Don't offer to save if killed now
              (goto-char (point-min))
              (condition-case nil
                  ;; This may fail if there are no opens
                  (down-list 1)
                (error nil)))))))


;; AJR: XEmacs, makes sense to dump into "other frame".

(defun ess-dump-object-into-edit-buffer-other-frame (object)
  "Edit an ESS object in its own frame."
  (switch-to-buffer-other-frame (ess-dump-object-into-edit-buffer object)))





(provide 'ess-dump)

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
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-dump.el ends here
