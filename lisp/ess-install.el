;;; ess-install.el --- Automatic installation of ESS.
;; Auto-install procedure.  EXPERIMENTAL!

;; Copyright (C) 2006 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Stephen Eglen

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge more than a distribution fee for it, do distribute the
;; source with any binaries, remove this notice, or hold anyone liable
;; for its results.

;; Steps:
;;
;; Open this file within emacs and then type:
;; M-x eval-buffer RET
;; to install ESS in Emacs.


;; Installing ESS should be straightforward, but sometimes people get
;; confused about where to put the startup lines.  This should help!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Location where the new lisp files are stored.
(defvar ess-lisp-dir (file-name-directory 
		      (abbreviate-file-name buffer-file-name))
  "Location where the new lisp files are stored.")

(defvar ess-site-file (concat ess-lisp-dir "ess-site")
  "Full path to the new ess-site file.  
Do not include .el extension in case there is also a .elc around.")

(defvar ess-new-version nil
  "Version number of new ESS to be installed.")

(defvar ess-installed nil)

;; Check that ess-site-file is written using unix directory separators.
;; i.e. need to change c:\\some\\dir\\ess-site.el to 
;; c:/some/dir/ess-site.el
;; To do this, we have to load in ess-replace-in-string, from
;; the file ess-inf.el

(save-window-excursion
  (find-file (concat ess-lisp-dir "ess-inf.el"))
  (goto-char (point-min))
  (search-forward-regexp "^(defun ess-replace-in-string " nil t)
  (eval-defun nil)
  (setq ess-site-file 
	(ess-replace-in-string ess-site-file "\\\\" "/" t))
  )


;; Get the version number of the new software.  Open the file
;; ess-cust.el and then find the definition of the variable
;; ess-version.  
(save-window-excursion
  (let ((beg))
    (find-file (concat ess-lisp-dir "ess-cust.el"))
    ;; go back to start, just in case file was previously open.
    (goto-char (point-min))
    (search-forward "defvar ess-version \"")
    (setq beg (point))
    (search-forward "\"")
    (setq ess-new-version (buffer-substring beg (1- (point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highlighting (copied from reftex.el -- cheers Carsten!)
;; Only one highlight region is needed, whereas two are provided here,
;; so this code could be simplified.  But we may want it again later.

;; Highlighting uses overlays.  If this is for XEmacs, we need to load
;; the overlay library, available in version 19.15
(and (not (fboundp 'make-overlay))
     (condition-case nil
         (require 'overlay)
       ('error 
        (error "Fm needs overlay emulation (available in XEmacs 19.15)"))))

;; We keep a vector with several different overlays to do our highlighting.
(defvar ess-highlight-overlays [nil nil])

;; Initialize the overlays (here we provide two overlays)
(aset ess-highlight-overlays 0 (make-overlay 1 1))
(overlay-put (aref ess-highlight-overlays 0) 'face 'highlight)
(aset ess-highlight-overlays 1 (make-overlay 1 1))
(overlay-put (aref ess-highlight-overlays 1) 'face 'highlight)

;; Two functions for activating and deactivation highlight overlays
(defun ess-highlight (index begin end &optional buffer)
  "Highlight a region with overlay INDEX."
  (move-overlay (aref ess-highlight-overlays index)
                begin end (or buffer (current-buffer))))
(defun ess-unhighlight (index)
  "Detatch overlay INDEX."
  (delete-overlay (aref ess-highlight-overlays index)))

;;; End of highlighting code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  

;; Try to find the .emacs init file and edit it.
(save-window-excursion

  ;; Try to find the init file if one already exists, 
  ;; or create a new one if we can't find any.
  (if (stringp user-init-file)
      (find-file user-init-file)
    ;; else, let's guess that the init file should be called ".emacs"
    ;; and the tilde will be resolved okay.
    (find-file "~/.emacs"))
  (goto-char (point-min))

  (let ((ess-commands
	 (concat "\n;;; ESS setup for version " ess-new-version "\n"
		 "(load \"" ess-site-file "\")\n"))
	(new-install)
	(beg))
    (if (search-forward ";;; ESS setup for version " nil t)
	(progn
	  (message "You already have ESS installed.")
	  (setq ess-installed 
		(buffer-substring (point)
				  (save-excursion (end-of-line) (point))))
	  
	  (beginning-of-line)
	  (setq beg (point))
	  ;; We assume the next line contains a sexp that loads the
	  ;; the ess-site; this sexp can be multiple lines.
	  (forward-line 1) 
	  (forward-list 1)
	  (ess-highlight 0 beg (point))
	  
	  (setq new-install 
		(yes-or-no-p 
		 (concat "Replace ESS version " ess-installed 
			 " with version " 
		     ess-new-version "? ")))
	  (when new-install
	    (kill-region beg (point))
	    (insert ess-commands)
	    (save-buffer)
	    (message (concat "ESS updated to version "  ess-new-version))
	    ))
      ;; else, just insert commands at end.
      (goto-char (point-max))
      (insert ess-commands)
      (save-buffer)
      (message (concat "ESS version "ess-new-version" installed."))
      )))




