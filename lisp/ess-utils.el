;;; ess-utils.el --- General Emacs utility functions used by ESS

;; Copyright (C) 1998--2000 D. Bates, K. Hornik, R.M. Heiberger,
;; M. Maechler, and A.J. Rossini.

;; Author: Martin Maechler <maechler@stat.math.ethz.ch>
;; Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
;; Created: 9 Sept 1998
;; Modified: $Date: 2002/07/16 14:16:33 $
;; Version: $Revision: 5.12 $
;; RCS: $Id: ess-utils.el,v 5.12 2002/07/16 14:16:33 rsparapa Exp $

;; This file is part of ESS (Emacs Speaks Statistics).

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

;;;-- Emacs Utilities --- Generally useful --- used by (but not requiring) ESS

(defun inside-string/comment-p (pos)
  "Return non-nil if POSition [defaults to (point) is inside string or comment
 (according to syntax). NOT OKAY for multi-line comments!!"
  ;;FIXME (defun S-calculate-indent ..) in ./essl-s.el can do that ...
  (interactive "d");point by default
  (let ((pps (save-excursion
	       (parse-partial-sexp
		(save-excursion (beginning-of-line) (point))
		pos))))
    (or (nth 3 pps) (nth 4 pps)))); 3: string,  4: comment

;; simple alternative to ess-read-object-name-default of ./ess-inf.el :
(defun ess-extract-word-name ()
  "Get the word you're on."
  (save-excursion
    (re-search-forward "\\<\\w+\\>" nil t)
    (buffer-substring (match-beginning 0) (match-end 0))))

(defun ess-rep-regexp (regexp to-string &optional fixedcase literal verbose)
  "Instead of (replace-regexp..) -- do NOT replace in strings or comments.
 If FIXEDCASE is non-nil, do *not* alter case of replacement text.
 If LITERAL   is non-nil, do *not* treat `\\' as special.
 If VERBOSE   is non-nil, (message ..) about replacements."
  (let ((case-fold-search (and case-fold-search
			       (not fixedcase))); t  <==> ignore case in search
	(pl) (p))
    (while (setq p (re-search-forward regexp nil t))
      (cond ((not (inside-string/comment-p (1- p)))
	     (if verbose
		 (let ((beg (match-beginning 0)))
		   (message "(beg,p)= (%d,%d) = %s"
			    beg p (buffer-substring beg p) )))
	     (replace-match to-string fixedcase literal)
	     ;;or (if verbose (setq pl (append pl (list p))))
	     )))
    ;;or (if (and verbose pl)
    ;;or  (message "s/%s/%s/ at %s" regexp to-string pl))
    ) )

(defun ess-revert-wisely ()
  "Revert from disk if file and buffer last modification times are different."
  (interactive)

; vc-revert-buffer acting strangely in Emacs 21.1; no longer used

; Long-winded Explanation

; Maybe I am being a little hard on 21.1, but it behaves differently.
; Basically, revert means roll-back.  But, for SAS purposes, you never
; really want to roll-back.  You want to refresh the buffer with the 
; disk file which is being modified in the background.  So, we only 
; roll-back when the date/time stamp of the file is newer than the buffer
; (technically, this is roll-ahead).

; However, I was supporting a version control system (RCS) when I originally 
; wrote this function.  I added functionality so that the roll-back was 
; performed by vc.  This worked fine until 21.1.  In 21.1 when you call this 
; function with vc/CVS, it actually rolls-back to the prior version of the 
; file rather than refreshing.  Apparently, it ignores the file on disk.  
; This change actually makes some sense, but it isn't what we want. 

  (if (not (verify-visited-file-modtime (current-buffer)))
      (revert-buffer t t)))

;;      (cond ((and (fboundp 'vc-backend-deduce)
;;		  (vc-backend-deduce (buffer-file-name))) (vc-revert-buffer))
;;	    ((and (fboundp 'vc-backend)
;;		  (vc-backend (buffer-file-name))) (vc-revert-buffer))
;;	    (t (revert-buffer t t)))))

(defun ess-space-around (word &optional from verbose)
  "Replace-regexp .. ensuring space around all occurences of WORD,
 starting from FROM {defaults to (point)}."
  (interactive "d\nP"); Defaults: point and prefix (C-u)
  (save-excursion
    (goto-char from)
    (ess-rep-regexp (concat "\\([^ \t\n]\\)\\(\\<" word "\\>\\)")
		    "\\1 \\2" nil nil verbose)
    (goto-char from)
    (ess-rep-regexp (concat "\\(\\<" word "\\>\\)\\([^ \t\n]\\)")
		    "\\1 \\2" nil nil verbose)
  )
)

(defun ess-time-string (&optional clock)
  "Returns a string for use as a timestamp. + hr:min if CLOCK is non-nil,
 like \"13 Mar 1992\".  Redefine to taste."
  (format-time-string (concat "%e %b %Y" (if clock ", %H:%M"))))


;;- From: friedman@gnu.ai.mit.edu (Noah Friedman)
;;- Date: 12 Feb 1995 21:30:56 -0500
;;- Newsgroups: gnu.emacs.sources
;;- Subject: nuke-trailing-whitespace
;;-
;;- This is too trivial to make into a big todo with comments and copyright
;;- notices whose length exceed the size of the actual code, so consider it
;;- public domain.  Its purpose is along similar lines to that of
;;- `require-final-newline', which is built in.  I hope the names make it
;;- obvious.

;; (add-hook 'write-file-hooks 'nuke-trailing-whitespace)
;;or at least
;; (add-hook 'ess-mode-hook
;; 	  '(lambda ()
;; 	     (add-hook 'local-write-file-hooks 'nuke-trailing-whitespace)))

(defvar nuke-trailing-whitespace-p nil;disabled by default  'ask
  "*[Dis]activates (nuke-trailing-whitespace).
 Disabled if `nil'; if `t', it works unconditionally, otherwise,
 the user is queried.
 Note that setting the default to `t' may not be a good idea when you edit
 binary files!")

(defun nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

If the variable `nuke-trailing-whitespace-p' is `nil', this function is
disabled.  If `t', unreservedly strip trailing whitespace.
If not `nil' and not `t', query for each instance."
  (interactive)
  (let ((bname (buffer-name)))
    (cond ((or
	    (string= major-mode "rmail-mode")
	    (string= bname "RMAIL")
	    nil)); do nothing..

	  (t
	   (and (not buffer-read-only)
		nuke-trailing-whitespace-p
		(save-match-data
		  (save-excursion
		    (save-restriction
		      (widen)
		      (goto-char (point-min))
		      (cond ((eq nuke-trailing-whitespace-p t)
			     (while (re-search-forward "[ \t]+$" (point-max) t)
			       (delete-region (match-beginning 0)
					      (match-end 0))))
			    (t
			     (query-replace-regexp "[ \t]+$" "")))))))))
    ;; always return nil, in case this is on write-file-hooks.
    nil))

(defun ess-kermit-get (&optional ess-file-arg)
"Get a file with Kermit.  WARNING:  Experimental!  From your *shell*
buffer, start kermit and then log in to the remote machine.  Open 
a file that starts with `ess-kermit-prefix'.  From that buffer, 
execute this command.  It will retrieve a file from the remote 
directory that you specify with the same name, but without the
`ess-kermit-prefix'."

    (interactive)

;;     (save-match-data 
       (let ((ess-temp-file (if ess-file-arg ess-file-arg (buffer-name)))
	     (ess-temp-file-remote-directory nil))
     
	(if (string-equal ess-kermit-prefix (substring ess-temp-file 0 1)) 
	  (progn
	    (setq ess-kermit-remote-directory (read-string "Remote directory to transfer file from: "
	  	ess-kermit-remote-directory))

	  (setq ess-temp-file-remote-directory ess-kermit-remote-directory)
;;	  (setq ess-temp-file (substring ess-temp-file (match-end 0)))
	  (shell)
	  (insert "cd $HOME; " ess-kermit-command " -s " ess-temp-file-remote-directory "/"
	    (substring ess-temp-file 1) " -a " ess-temp-file)
          (comint-send-input)	
;;          (insert (read-string "Press Return to connect to Kermit: " nil nil "\C-\\c"))
;;	  (comint-send-input)
;;	  (insert (read-string "Press Return when Kermit is ready to recieve: " nil nil 
;;		  (concat "receive ]" ess-sas-temp-file)))                
;;	  (comint-send-input)
;;	  (insert (read-string "Press Return when transfer is complete: " nil nil "c"))                
;;	  (comint-send-input)
          (insert (read-string "Press Return when shell is ready: "))
	  (comint-send-input)
	  (switch-to-buffer (find-buffer-visiting ess-temp-file))
	  (ess-revert-wisely)
))))

(defun ess-kermit-send ()
"Send a file with Kermit.  WARNING:  Experimental!  From 
a file that starts with `ess-kermit-prefix',
execute this command.  It will transfer this file to the remote 
directory with the same name, but without the `ess-kermit-prefix'."

    (interactive)

;;     (save-match-data 
       (let ((ess-temp-file (expand-file-name (buffer-name)))
	     (ess-temp-file-remote-directory nil))
     
	(if (string-equal ess-kermit-prefix (substring (file-name-nondirectory ess-temp-file) 0 1)) 
	  (progn
		
	  (setq ess-kermit-remote-directory (read-string "Remote directory to transfer file to: "
	    ess-kermit-remote-directory))
	  (setq ess-temp-file-remote-directory ess-kermit-remote-directory)
		
;;	  (setq ess-temp-file (substring ess-temp-file (match-end 0)))
	  (shell)
	  (insert "cd $HOME; " ess-kermit-command " -a " ess-temp-file-remote-directory "/"
	    (substring (file-name-nondirectory ess-temp-file) 1) " -g "  ess-temp-file)
          (comint-send-input)	
;;          (insert (read-string "Press Return to connect to Kermit: " nil nil "\C-\\c"))
;;	  (comint-send-input)
;;	  (insert (read-string "Press Return when Kermit is ready to recieve: " nil nil 
;;		  (concat "receive ]" ess-sas-temp-file)))                
;;	  (comint-send-input)
;;	  (insert (read-string "Press Return when transfer is complete: " nil nil "c"))                
;;	  (comint-send-input)
          (insert (read-string "Press Return when shell is ready: "))
	  (comint-send-input)
	  (switch-to-buffer (find-buffer-visiting ess-temp-file))
	  (ess-revert-wisely)
))))

(provide 'ess-utils)
