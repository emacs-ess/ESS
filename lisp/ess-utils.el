;;; ess-utils.el --- General Emacs utility functions used by ESS

;; Copyright (C) 1998--2000 D. Bates, K. Hornik, R.M. Heiberger,
;; M. Maechler, and A.J. Rossini.

;; Author: Martin Maechler <maechler@stat.math.ethz.ch>
;; Maintainer: Martin Maechler <maechler@stat.math.ethz.ch>
;; Created: 9 Sept 1998
;; Modified: $Date: 2000/10/26 09:11:39 $
;; Version: $Revision: 5.5 $
;; RCS: $Id: ess-utils.el,v 5.5 2000/10/26 09:11:39 maechler Exp $

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

(defun ess-time-string (&optional clock)
  "Returns a string for use as a timestamp. + hr:min if CLOCK is non-nil.
 Currently returns strings like \"13 Mar 1992\".  Redefine to taste."
  ;; RELIES on (current-time-string) : Must be  exactly
  ;; of this structure  [0..23], e.g. == "Mon Jan 27 17:30:45 1992"
  (let* ((time (current-time-string))
	 (mon (substring time 4 7))
	 (day (substring time 8 10))
	 (HM  (if clock (substring time 11 16)))
	 (year (substring time 20 24))); 4 digit year!
    (concat day " " mon " " year
	    (if clock (concat ", " HM)))))

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
;;-
;;- Keep in mind, making the default `t' may not be a good idea when you edit
;;- binary files.

;;-  (add-hook 'write-file-hooks 'nuke-trailing-whitespace)
;;-or at least

;; (add-hook 'ess-mode-hook
;; 	  '(lambda ()
;; 	     (add-hook 'local-write-file-hooks 'nuke-trailing-whitespace)))

(defvar nuke-trailing-whitespace-p nil;disabled by default  'ask
  "[Dis]activates (nuke-trailing-whitespace).
 Disabled if `nil'; if `t', it works unconditionally, otherwise,
 the user is queried.
 Note that making the default `t' may not be a good idea when you edit
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

(provide 'ess-utils)
