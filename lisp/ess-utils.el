;;; ess-utils.el --- General Emacs utility functions used by ESS

;; Copyright (C) 1998--2005 A.J. Rossini, Rich M. Heiberger, Martin
;;	Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Original Author: Martin Maechler <maechler@stat.math.ethz.ch>
;; Created: 9 Sept 1998
;; Maintainers: ESS-core <ESS-core@r-project.org>

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

(defun ess-inside-string-or-comment-p (pos)
  "Return non-nil if POSition [defaults to (point)] is inside string or comment
 (according to syntax). NOT OKAY for multi-line comments!!"
  ;;FIXME (defun S-calculate-indent ..) in ./ess-s-l.el can do that ...
  (interactive "d");point by default
  (let ((pps (save-excursion
	       (parse-partial-sexp
		(save-excursion (beginning-of-line) (point))
		pos))))
    (or (nth 3 pps) (nth 4 pps)))); 3: string,  4: comment

(defsubst ess-inside-string-p ()
  "Return non-nil if point is inside string (according to syntax)."
  (interactive)
  (save-excursion
    (nth 3 (parse-partial-sexp (ess-line-beginning-position) (point)))))

;; simple alternative to ess-read-object-name-default of ./ess-inf.el :
;; is "wrongly" returning   "p1"  for word "p1.part2" :
(defun ess-extract-word-name ()
  "Get the word you're on (cheap algorithm). Use `ess-read-object-name-default'
for a better but slower version."
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
      (cond ((not (ess-inside-string-or-comment-p (1- p)))
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

(defun ess-replace-regexp-dump-to-src
  (regexp to-string &optional dont-query verbose ensure-mode)
  "Depending on dont-query, call `ess-rep-regexp' or `query-replace-regexp'
from the beginning of the buffer."
  (save-excursion
    (if (and ensure-mode
	     (not (equal major-mode 'ess-mode)))
	(ess-mode))
    (goto-char (point-min))
    (if dont-query
	(ess-rep-regexp     regexp to-string nil nil verbose)
      (query-replace-regexp regexp to-string nil))))


(defun ess-revert-wisely ()
  "Revert from disk if file and buffer last modification times are different."
  (interactive)

; whether or not a revert is needed, force load local variables
; for example, suppose that you change the local variables and then
; save the file, a revert is unneeded, but a force load is
  (hack-local-variables)

  (if (not (verify-visited-file-modtime (current-buffer))) (progn
      (let ((ess-temp-store-point (point)))
	(revert-buffer t t)
	(goto-char ess-temp-store-point))
      t)
  nil))


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

(defvar ess-nuke-trailing-whitespace-p nil;disabled by default  'ask
  "*[Dis]activates (ess-nuke-trailing-whitespace).
 Disabled if `nil'; if `t', it works unconditionally, otherwise,
 the user is queried.
 Note that setting the default to `t' may not be a good idea when you edit
 binary files!")

;;; MM: Newer Emacsen now have  delete-trailing-whitespace
;;; --  but no customization like  nuke-trailing-whitespace-p ..
(defun ess-nuke-trailing-whitespace ()
  "Nuke all trailing whitespace in the buffer.
Whitespace in this case is just spaces or tabs.
This is a useful function to put on write-file-hooks.

If the variable `ess-nuke-trailing-whitespace-p' is `nil', this function is
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
		ess-nuke-trailing-whitespace-p
		(save-match-data
		  (save-excursion
		    (save-restriction
		      (widen)
		      (goto-char (point-min))
		      (cond ((eq ess-nuke-trailing-whitespace-p t)
			     (while (re-search-forward "[ \t]+$" (point-max) t)
			       (delete-region (match-beginning 0)
					      (match-end 0))))
			    (t
			     (query-replace-regexp "[ \t]+$" "")))))))))
    ;; always return nil, in case this is on write-file-hooks.
    nil))

(defun ess-kermit-get (&optional ess-file-arg ess-dir-arg)
"Get a file with Kermit.  WARNING:  Experimental!  From your *shell*
buffer, start kermit and then log in to the remote machine.  Open
a file that starts with `ess-kermit-prefix'.  From that buffer,
execute this command.  It will retrieve a file from the remote
directory that you specify with the same name, but without the
`ess-kermit-prefix'."

    (interactive)

;;     (save-match-data
       (let ((ess-temp-file (if ess-file-arg ess-file-arg (buffer-name)))
	     (ess-temp-file-remote-directory ess-dir-arg))

	(if (string-equal ess-kermit-prefix (substring ess-temp-file 0 1))
	  (progn
;; I think there is a bug in the buffer-local variable handling in GNU Emacs 21.3
;; Setting ess-kermit-remote-directory every time is somehow resetting it to the
;; default on the second pass.  So, here's a temporary work-around.  It will fail
;; if you change the default, so maybe this variable should not be customizable.
;; In any case, there is also trouble with local variables in XEmacs 21.4.9 and
;; 21.4.10.  XEmacs 21.4.8 is fine.
	    (if ess-temp-file-remote-directory
		(setq ess-kermit-remote-directory ess-temp-file-remote-directory)

		(if (string-equal "." ess-kermit-remote-directory)
		    (setq ess-kermit-remote-directory (read-string "Remote directory to transfer file from: "
		    ess-kermit-remote-directory))))

	  (setq ess-temp-file-remote-directory ess-kermit-remote-directory)
;;	  (setq ess-temp-file (substring ess-temp-file (match-end 0)))
	  (ess-sas-goto-shell)
	  (insert "cd " ess-temp-file-remote-directory "; " ess-kermit-command " -s "
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
;; I think there is a bug in the buffer-local variable handling in GNU Emacs 21.3
;; Setting ess-kermit-remote-directory every time is somehow resetting it to the
;; default on the second pass.  Here's a temporary work-around.  It will fail
;; if you change the default, so maybe this variable should not be customizable.
;; In any case, there is also trouble with local variables in XEmacs 21.4.9 and
;; 21.4.10.  XEmacs 21.4.8 is fine.
	    (if (string-equal "." ess-kermit-remote-directory)
	        (setq ess-kermit-remote-directory (read-string "Remote directory to transfer file to: "
		    ess-kermit-remote-directory)))

	  (setq ess-temp-file-remote-directory ess-kermit-remote-directory)

;;	  (setq ess-temp-file (substring ess-temp-file (match-end 0)))
	  (ess-sas-goto-shell)
	  (insert "cd " ess-temp-file-remote-directory "; " ess-kermit-command " -a "
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

(defun ess-search-except (regexp &optional except backward)
  "Search for a regexp, store as match 1, optionally ignore
strings that match exceptions."
  (interactive)

  (let ((continue t) (exit nil))

    (while continue
      (if (or (and backward (search-backward-regexp regexp nil t))
	      (and (not backward) (search-forward-regexp regexp nil t)))
	  (progn
	    (setq exit (match-string 1))
            (setq continue (and except (string-match except exit)))
	    (if continue (setq exit nil)))
	;;else
	(setq continue nil))
      )

    exit))

(defun ess-save-and-set-local-variables ()
  "If buffer was modified, save file and set Local Variables if defined.
Return t if buffer was modified, nil otherwise."
  (interactive)

  (let ((ess-temp-point (point))
	(ess-temp-return-value (buffer-modified-p)))
    ;; if buffer has changed, save buffer now (before potential revert)
    (if ess-temp-return-value (save-buffer))

    ;; If Local Variables are defined, update them now
    ;; since they may have changed since the last revert
    ;;  (save-excursion
    (beginning-of-line -1)
    (save-match-data
      (if (search-forward "End:" nil t) (revert-buffer t t)))
    ;; save-excursion doesn't save point in the presence of a revert
    ;; so you need to do it yourself
    (goto-char ess-temp-point)

    ess-temp-return-value))

(defun ess-get-file-or-buffer (file-or-buffer)
  "Return file-or-buffer if it is a buffer; otherwise return the buffer
associated with the file which must be qualified by it's path; if the
buffer does not exist, return nil."
  (interactive)

  (if file-or-buffer
      (if (bufferp file-or-buffer) file-or-buffer
	(find-buffer-visiting file-or-buffer))))

(defun ess-set-local-variables (alist &optional file-or-buffer)
  "Set local variables from ALIST in current buffer; if file-or-buffer
is specified, perform action in that buffer."
  (interactive)
  (if file-or-buffer (set-buffer (ess-get-file-or-buffer file-or-buffer)))

  (mapcar (lambda (pair)
	    (make-local-variable (car pair))
            (set (car pair) (eval (cdr pair))))
          alist))

(defun ess-clone-local-variables (from-file-or-buffer
				  &optional to-file-or-buffer)
  "Clone local variables from one buffer to another buffer."
  (interactive)
  (ess-set-local-variables
   (ess-sas-create-local-variables-alist from-file-or-buffer)
   to-file-or-buffer))

(defun ess-return-list (ess-arg)
  "Given an item, if it is a list return it, else return item in a list."
  (if (listp ess-arg) ess-arg (list ess-arg)))

(defun ess-find-exec (ess-root-arg ess-root-dir)
  "Given a root directory and the root of an executable file name,
find it's full name and path, if it exists, anywhere in the sub-tree."
  (let* ((ess-tmp-dirs (directory-files ess-root-dir t "^[^.]"))
	 (ess-tmp-return (ess-find-exec-completions ess-root-arg ess-root-dir))
	 (ess-tmp-dir nil))

    (while ess-tmp-dirs
      (setq ess-tmp-dir (car ess-tmp-dirs)
	    ess-tmp-dirs (cdr ess-tmp-dirs))
      (if (file-accessible-directory-p ess-tmp-dir)
	  (setq ess-tmp-return
		(nconc ess-tmp-return
		       (ess-find-exec ess-root-arg ess-tmp-dir)))))
    ess-tmp-return))

(defun ess-find-exec-completions (ess-root-arg &optional ess-exec-dir)
  "Given the root of an executable file name, find all possible completions.
Search for the executables in ESS-EXEC-DIR (which defaults to
`exec-path' if no value is given)."
  (let* ((ess-exec-path
	  (if ess-exec-dir (ess-return-list ess-exec-dir) exec-path))
	 (ess-tmp-exec nil)
	 (ess-tmp-path-count (length ess-exec-path))
	 (ess-tmp-dir nil)
	 (ess-tmp-files nil)
	 (ess-tmp-file nil))

    (while ess-exec-path
      (setq ess-tmp-dir (car ess-exec-path)
	    ess-exec-path (cdr ess-exec-path))
      (when
	  (and (> (length ess-tmp-dir) 0)
	       (file-accessible-directory-p ess-tmp-dir))
	;; the first test above excludes "" from exec-path, which can be
	;; problematic with Tramp.
	(setq ess-tmp-files
	      (file-name-all-completions ess-root-arg ess-tmp-dir))

	(while ess-tmp-files
	  (setq ess-tmp-file
		(concat (file-name-as-directory ess-tmp-dir)
			(car ess-tmp-files))
		ess-tmp-files (cdr ess-tmp-files))
	  (if (and (file-executable-p ess-tmp-file)
		   (not (file-directory-p ess-tmp-file)))
	      ;; we have found a possible executable, so keep it.
	      (setq ess-tmp-exec
		    (nconc ess-tmp-exec (list ess-tmp-file)))))))
    ess-tmp-exec))

;; Copyright (C) 1994 Simon Marshall.
;; Author: Simon Marshall <Simon.Marshall@mail.esrin.esa.it>
;; LCD Archive Entry:
;; unique|Simon Marshall|Simon.Marshall@mail.esrin.esa.it|
;; Functions and commands to uniquify lists or buffer text (cf. sort).
;; 23-Apr-1994|1.00|~/packages/unique.el.Z|
;;
;; MM: renamed from 'unique' to
(defun ess-unique (list predicate)
  "Uniquify LIST, stably, deleting elements using PREDICATE.
Return the list with subsequent duplicate items removed by side effects.
PREDICATE is called with an element of LIST and a list of elements from LIST,
and should return the list of elements with occurrences of the element removed.
This function will work even if LIST is unsorted.  See also `uniq'."
  (let ((list list))
    (while list
      (setq list (setcdr list (funcall predicate (car list) (cdr list))))))
  list)

(defun ess-uniq-list (items)
  "Delete all duplicate entries in ITEMS list, calling `ess-unique'."
  (ess-unique items 'delete))

(defun ess-drop-non-directories (file-strings)
  "Drop all entries that do not \"look like\" directories."
  (ess-flatten-list (mapcar 'file-name-directory file-strings)))


(defun ess-flatten-list (&rest list)
  "Take the arguments and flatten them into one long list.
Drops 'nil' entries."
  ;; Taken from lpr.el
  ;; `lpr-flatten-list' is defined here (copied from "message.el" and
  ;; enhanced to handle dotted pairs as well) until we can get some
  ;; sensible autoloads, or `flatten-list' gets put somewhere decent.

  ;; (ess-flatten-list '((a . b) c (d . e) (f g h) i . j))
  ;; => (a b c d e f g h i j)
  (ess-flatten-list-1 list))

(defun ess-flatten-list-1 (list)
  (cond
   ((null list) (list))
   ((consp list)
    (append (ess-flatten-list-1 (car list))
	    (ess-flatten-list-1 (cdr list))))
   (t (list list))))

(defun ess-delete-blank-lines ()
  "Convert 2 or more lines of white space into one."
    (interactive)
    (save-excursion
	(goto-char (point-min))
	(save-match-data
	    (while (search-forward-regexp "^[ \t]*\n[ \t]*\n" nil t)
              ;;(goto-char (match-beginning 0))
		    (delete-blank-lines)))))

(defun ess-do-auto-fill ()
  "This is the same as \\[do-auto-fill] in GNU emacs 21.3, with one major
difference: if we could not find a suitable place to break the line,
we simply do not break it (instead of breaking after the first word)."
  (let (fc justify bol give-up
	   (fill-prefix fill-prefix))
    (if (or (not (setq justify (current-justification)))
	    (null (setq fc (current-fill-column)))
	    (and (eq justify 'left)
		 (<= (current-column) fc))
	    (save-excursion (beginning-of-line)
			    (setq bol (point))
			    (and auto-fill-inhibit-regexp
				 (looking-at auto-fill-inhibit-regexp))))
	nil ;; Auto-filling not required
      (if (memq justify '(full center right))
	  (save-excursion (unjustify-current-line)))

      ;; Choose a fill-prefix automatically.
      (if (and adaptive-fill-mode
	       (or (null fill-prefix) (string= fill-prefix "")))
	  (let ((prefix
		 (fill-context-prefix
		  (save-excursion (backward-paragraph 1) (point))
		  (save-excursion (forward-paragraph 1) (point)))))
	    (and prefix (not (equal prefix ""))
		 (setq fill-prefix prefix))))

      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (after-prefix
	       (fill-point
		(let ((opoint (point))
		      bounce
		      (first t))
		  (save-excursion
		    (beginning-of-line)
		    (setq after-prefix (point))
		    (and fill-prefix
			 (looking-at (regexp-quote fill-prefix))
			 (setq after-prefix (match-end 0)))
		    (move-to-column (1+ fc))
		    ;; Move back to the point where we can break the line.
		    ;; We break the line between word or
		    ;; after/before the character which has character
		    ;; category `|'.  We search space, \c| followed by
		    ;; a character, or \c| following a character.  If
		    ;; not found, place the point at beginning of line.
		    (while (or first
			       ;; If this is after period and a single space,
			       ;; move back once more--we don't want to break
			       ;; the line there and make it look like a
			       ;; sentence end.
			       (and (not (bobp))
				    (not bounce)
				    sentence-end-double-space
				    (save-excursion (forward-char -1)
						    (and (looking-at "\\. ")
							 (not (looking-at "\\.  ")))))
			       (and (not (bobp))
				    (not bounce)
				    fill-nobreak-predicate
				    (funcall fill-nobreak-predicate)))
		      (setq first nil)
		      (re-search-backward "[ \t]\\|\\c|.\\|.\\c|\\|^")
		      ;; If we find nowhere on the line to break it,
		      ;; do not break it.  Set bounce to t
		      ;; so we will not keep going in this while loop.
		      (if (<= (point) after-prefix)
			  (setq bounce t)
			(if (looking-at "[ \t]")
			    ;; Break the line at word boundary.
			    (skip-chars-backward " \t")
			  ;; Break the line after/before \c|.
			  (forward-char 1))))
		    (if enable-multibyte-characters
			;; If we are going to break the line after or
			;; before a non-ascii character, we may have
			;; to run a special function for the charset
			;; of the character to find the correct break
			;; point.
			(if (not (and (eq (charset-after (1- (point))) 'ascii)
				      (eq (charset-after (point)) 'ascii)))
			    (fill-find-break-point after-prefix)))

		    ;; Let fill-point be set to the place where we end up.
		    ;; But move back before any whitespace here.
		    (skip-chars-backward " \t")
		    (point)))))

	  ;; See whether the place we found is any good.
	  (if (save-excursion
		(goto-char fill-point)
		(and (not (bolp))
		     ;; There is no use breaking at end of line.
		     (not (save-excursion (skip-chars-forward " ") (eolp)))
		     ;; It is futile to split at the end of the prefix
		     ;; since we would just insert the prefix again.
		     (not (and after-prefix (<= (point) after-prefix)))
		     ;; Don't split right after a comment starter
		     ;; since we would just make another comment starter.
		     (not (and comment-start-skip
			       (let ((limit (point)))
				 (beginning-of-line)
				 (and (re-search-forward comment-start-skip
							 limit t)
				      (eq (point) limit)))))))
	      ;; Ok, we have a useful place to break the line.  Do it.
	      (let ((prev-column (current-column)))
		;; If point is at the fill-point, do not `save-excursion'.
		;; Otherwise, if a comment prefix or fill-prefix is inserted,
		;; point will end up before it rather than after it.
		(if (save-excursion
		      (skip-chars-backward " \t")
		      (= (point) fill-point))
		    (funcall comment-line-break-function t)
		  (save-excursion
		    (goto-char fill-point)
		    (funcall comment-line-break-function t)))
		;; Now do justification, if required
		(if (not (eq justify 'left))
		    (save-excursion
		      (end-of-line 0)
		      (justify-current-line justify nil t)))
		;; If making the new line didn't reduce the hpos of
		;; the end of the line, then give up now;
		;; trying again will not help.
		(if (>= (current-column) prev-column)
		    (setq give-up t)))
	    ;; No good place to break => stop trying.
	    (setq give-up t))))
      ;; Justify last line.
      (justify-current-line justify t t)
      t)))

(defun ess-select-frame-set-input-focus (frame)
  "Select FRAME, raise it, and set input focus, if possible.
Copied almost verbatim from gnus-utils.el (but with test for mac added)."
  (cond ((featurep 'xemacs)
	 (raise-frame frame)
	 (select-frame frame)
	 (focus-frame frame))
	;; The function `select-frame-set-input-focus' won't set
	;; the input focus under Emacs 21.2 and X window system.
	;;((fboundp 'select-frame-set-input-focus)
	;; (defalias 'gnus-select-frame-set-input-focus
	;;   'select-frame-set-input-focus)
	;; (select-frame-set-input-focus frame))
	(t
	 (raise-frame frame)
	 (select-frame frame)
	 (cond ((and
		 (memq window-system '(x mac))
		 (fboundp 'x-focus-frame))
		(x-focus-frame frame))
	       ((eq window-system 'w32)
		(w32-focus-frame frame)))
	 (when focus-follows-mouse
	   (set-mouse-position frame (1- (frame-width frame)) 0)))))

(defun ess-sci-to-dec ()
    "For BUGS/S family: Express +/-0.000E+/-0 or +/-0.0e+/-00 as a decimal."
    (interactive)
    (setq buffer-read-only nil)
    (save-excursion (goto-char 0)
    (save-match-data (let ((ess-temp-replacement-string nil)
			    (ess-temp-replacement-9 0)
			    (ess-temp-replacement-diff 0))
     (while (search-forward-regexp "-?[0-9][.][0-9][0-9]?[0-9]?[Ee][+-][0-9][0-9]?" nil t)
	    (setq ess-temp-replacement-string
		  (int-to-string (string-to-number (match-string 0))))
	    (setq ess-temp-replacement-diff (- (match-end 0) (match-beginning 0)))
	    (save-match-data
	        (setq ess-temp-replacement-9
		    (string-match "99999999999$" ess-temp-replacement-string))

		(if (not ess-temp-replacement-9)
		    (setq ess-temp-replacement-9
			(string-match "000000000001$" ess-temp-replacement-string))))

	    (if ess-temp-replacement-9
		(setq ess-temp-replacement-string
		    (substring ess-temp-replacement-string 0 ess-temp-replacement-9)))

	    (setq ess-temp-replacement-diff
		(- ess-temp-replacement-diff (string-width ess-temp-replacement-string)))

	   (while (> ess-temp-replacement-diff 0)
		(setq ess-temp-replacement-string (concat ess-temp-replacement-string " "))
		(setq ess-temp-replacement-diff (- ess-temp-replacement-diff 1)))

           (replace-match ess-temp-replacement-string))))))

(defun ess-num-or-zero (arg)
"*If a number, then return that number, otherwise return 0."
(or (and (numberp arg) arg) 0))

(defun ess-change-directory (path)
  "Set the current working directory to PATH for both *R* and Emacs."
  (interactive "DDirectory to change to: ")

  (when (file-exists-p path)
    (ess-command (concat "setwd(\"" path "\")\n"))
    ;; use file-name-as-directory to ensure it has trailing /
    (setq default-directory (file-name-as-directory path))))

(provide 'ess-utils)
