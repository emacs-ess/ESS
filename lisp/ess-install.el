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


;;; Commentary:

;; Although installing ESS is relatively simple, sometimes people get
;; confused as to what to add to their init files, or even where their
;; init files are located.  The following procedure should be a bit
;; simpler, as Emacs will add the necessary start-up lines itself.
;; (The instructions below assume you have downloaded ess as a zip
;; package, but it will work also for the .tar.gz version of ESS as
;; long as you know how to unpack a .tar.gz in step 3.)
;; 
;; Installing ESS for the first time.
;; 
;; 1. Create a folder (e.g C:/emacs) where you will store ESS.  We will
;;   assume that you are installing ESS into C:/emacs (unix users can use
;;   ~/emacs).
;; 
;; 2. Download ess-5.2.12.zip and store it in the folder you created.
;; 
;; 3. Unpack the files from the zip archive, e.g. by right-clicking on it
;;    within Windows explorer and selecting "Extract all".  On unix, use
;;    "unzip ess-5.2.12.zip".
;; 
;; 4. Start a new emacs (or xemacs).
;; 
;; 5. In the new emacs, you need to open the file "ess-install.el" which
;;    is part of ESS.  To do this, type:
;; 
;;    C-x C-f c:/emacs/ess-5.2.12/lisp/ess-install.el RET
;; 
;; You should now see a lisp file with the top line:
;;   ;;; ess-install.el --- Automatic installation of ESS.
;; 
;; 6. Type M-x eval-buffer RET
;; 
;; What does this do?  This will find your emacs initialisation file, and
;; it will add the following two lines to the end of the file:
;; 
;;   ;;; ESS setup for version 5.2.12
;;   (load "c:/emacs/ess-5.2.12/lisp/ess-site")
;; 
;; Do not edit those two lines!  They are useful if later you come to
;; upgrade ESS.
;; 
;; 7. Start a new Emacs and you should find then that ESS is loaded.  For
;;    example, create a new file called "foo.R" and check that it opens
;;    in R mode by looking at the mode line and menubar.
;; 
;; Upgrading your version of ESS.
;; 
;; If (and only if) you use the above instructions for installing ESS,
;; when a new version of ESS is released, you can use the same method to
;; install the new version.  Repeat steps 2-7 for the new release of ESS,
;; and this time in step 6, if emacs finds that you already have the
;; special line ";;; ESS setup for version 5.2.12", it will highlight
;; those lines, and query whether you want to replace those two lines
;; with the new setup code.
;; 
;; If you do upgrade ESS this way, bear in mind that your old version
;; will not be deleted from your filespace -- you will have to delete it
;; yourself.

;; TODO: possibly add a call to (byte-recompile-directory ess-lisp-dir
;; 0) so that lisp files are byte compiled.

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


(defun ess-install-byte-compile ()
  "Byte compile the ESS files.
This will probably generate warnings, but they can hopefully be
ignored."
  ;; To do byte compilation, XEmacs seems to want the files on its
  ;; load-path so that it can do the (require 'xyz) statements.
  (add-to-list 'load-path ess-lisp-dir)
  (byte-recompile-directory ess-lisp-dir 0))

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
;; ess-custom.el and then find the definition of the variable
;; ess-version.  
(save-window-excursion
  (let ((beg))
    (find-file (concat ess-lisp-dir "ess-custom.el"))
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
	    (ess-install-byte-compile)
	    (message (concat "ESS updated to version "  ess-new-version))
	    ))
      ;; else, just insert commands at end.
      (goto-char (point-max))
      (insert ess-commands)
      (save-buffer)
      (ess-install-byte-compile)
      (message (concat "ESS version "ess-new-version" installed."))
      )))
