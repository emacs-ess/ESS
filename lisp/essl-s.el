;;; essl-s.el --- Support for editing S source code

;; Copyright (C) 1989-2001 D. Bates, Kademan, Ritter, D.M. Smith, K. Hornik,
;; R.M. Heiberger, M. Maechler, and A.J. Rossini.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 26 Aug 1997
;; Modified: $Date: 2002/01/15 08:47:17 $
;; Version: $Revision: 5.27 $
;; RCS: $Id: essl-s.el,v 5.27 2002/01/15 08:47:17 maechler Exp $

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

;;; Commentary:

;; Code for general editing S source code (specializes to S, S+, R).

;;; Code:

 ; Requires and autoloads

(ess-message "[essl-s:] (def** ) only ...")

 ; Configuration variables

(defvar S-syntax-table nil "Syntax table for S code.")
(if S-syntax-table
    nil
  (setq S-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" S-syntax-table)
  (modify-syntax-entry ?+  "."  S-syntax-table)
  (modify-syntax-entry ?-  "."  S-syntax-table)
  (modify-syntax-entry ?=  "."  S-syntax-table)
  (modify-syntax-entry ?%  "."  S-syntax-table)
  (modify-syntax-entry ?<  "."  S-syntax-table)
  (modify-syntax-entry ?>  "."  S-syntax-table)
  (modify-syntax-entry ?&  "."  S-syntax-table)
  (modify-syntax-entry ?|  "."  S-syntax-table)
  (modify-syntax-entry ?\' "\"" S-syntax-table)
  (modify-syntax-entry ?#  "<"  S-syntax-table) ; open comment
  (modify-syntax-entry ?\n ">"  S-syntax-table) ; close comment
  ;;(modify-syntax-entry ?.  "w"  S-syntax-table) ; "." used in S obj names
  (modify-syntax-entry ?.  "_"  S-syntax-table) ; see above/below,
					; plus consider separation.
  (modify-syntax-entry ?$  "_"  S-syntax-table) ; foo.bar$hack is 1 symbol
  (modify-syntax-entry ?_  "."  S-syntax-table)
  (modify-syntax-entry ?*  "."  S-syntax-table)
  (modify-syntax-entry ?<  "."  S-syntax-table)
  (modify-syntax-entry ?>  "."  S-syntax-table)
  (modify-syntax-entry ?/  "."  S-syntax-table))

(defvar S-editing-alist
  '((paragraph-start              . (concat "^$\\|" page-delimiter))
    (paragraph-separate           . (concat "^$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline        . t)
    (comment-start                . "#")
    (comment-start-skip           . "#+ *")
    (comment-column               . 40)
    ;;(comment-indent-function  . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-indent-line                      . 'S-indent-line)
    ;;(ess-calculate-indent           . 'S-calculate-indent)
    (indent-line-function            . 'S-indent-line)
    (parse-sexp-ignore-comments   . t)
    (ess-set-style                . ess-default-style)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . S-syntax-table)
    ;; For Changelog add, require ' ' before <- : "attr<-" is a function name :
    (add-log-current-defun-header-regexp . "^\\(.+\\)\\s-+<-[ \t\n]*function")
    (font-lock-defaults           . '(ess-mode-font-lock-keywords
				      nil nil ((?\. . "w")))))
  "General options for editing S, S+, and R source files.")

;;; Changes from S to S-PLUS 3.x.  (standard S3 should be in essl-s!).

(defconst S+-help-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?O . "OPTIONAL ARGUMENTS:")
    (?R . "REQUIRED ARGUMENTS:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Alist of (key . string) pairs for use in section searching.")
;;; `key' indicates the keystroke to use to search for the section heading
;;; `string' in an S help file. `string' is used as part of a
;;; regexp-search, and so specials should be quoted.

;; S ver.3 (NOT S-Plus)
(defconst S3-help-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Help section keys for S ver.3.")

;; S ver.4 (NOT S-Plus)
(defconst S4-help-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Help section keys for S4.")

;; R
(defconst R-help-sec-keys-alist
  '((?a . "\\s *Arguments:")
    (?d . "\\s *Description:")
    (?D . "\\s *Details:")
    (?e . "\\s *Examples:")
    (?n . "\\s *Note:")
    (?r . "\\s *References:")
    (?s . "\\s *See Also:")
    (?u . "\\s *Usage:")
    (?v . "\\s *Value[s]?")	;
    )
  "Alist of (key . string) pairs for use in help section searching.")


(defconst ess-help-S+-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file.")

(defconst ess-help-R-sec-regex "^\\s *[A-Z[a-z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file.")

;;; S-mode extras of Martin Maechler, Statistik, ETH Zurich.
;;; See also ./ess-utils.el

(defvar ess-function-outline-file
  (concat ess-lisp-directory "/../etc/" "function-outline.S")
  "The file name of the ess-function outline that is to be inserted at point,
when \\<ess-mode-map>\\[ess-insert-function-outline] is used.
Placeholders (substituted `at runtime'): $A$ for `Author', $D$ for `Date'.")

;; Use the user's own ~/S/emacs-fun.outline  is (s)he has one : ---
(let ((outline-file (concat (getenv "HOME") "/S/function-outline.S")))
  (if (file-exists-p outline-file)
      (setq ess-function-outline-file outline-file)))

 ; Function Definitions

(defun S-comment-indent ()
  "Indentation for S comments."

  (if (looking-at "###")
      (current-column)
    (if (looking-at "##")
	(let ((tem (S-calculate-indent)))
	  (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column))))

(defun S-indent-line ()
  "Indent current line as S code.
Return the amount the indentation changed by."
  (let ((indent (S-calculate-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
	   (setq indent (current-indentation)))
	  (t
	   (skip-chars-forward " \t")
	   (if (and ess-fancy-comments (looking-at "###"))
	       (setq indent 0))
	   (if (and ess-fancy-comments
		    (looking-at "#")
		    (not (looking-at "##")))
	       (setq indent comment-column)
	     (if (eq indent t) (setq indent 0))
	     (if (listp indent) (setq indent (car indent)))
	     (cond ((and (looking-at "else\\b")
			 (not (looking-at "else\\s_")))
		    (setq indent (save-excursion
				   (ess-backward-to-start-of-if)
				   (+ ess-else-offset (current-indentation)))))
		   ((= (following-char) ?})
		    (setq indent
			  (+ indent
			     (- ess-close-brace-offset ess-indent-level))))
		   ((= (following-char) ?{)
		    (setq indent (+ indent ess-brace-offset)))))))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.
      ;; Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun S-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as S code.
In usual case returns an integer: the column to indent to.
Returns nil if line starts inside a string, t if in a comment."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (case-fold-search nil)
	  state
	  containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      (while (< (point) indent-point)
	(setq parse-start (point))
	(setq state (parse-partial-sexp (point) indent-point 0))
	(setq containing-sexp (car (cdr state))))
      (cond ((or (nth 3 state) (nth 4 state))
	     ;; return nil or t if should not change this line
	     (nth 4 state))
	    ((null containing-sexp)
	     ;; Line is at top level.  May be data or function definition,
	     (beginning-of-line)
	     (if (and (/= (following-char) ?\{)
		      (save-excursion
			(ess-backward-to-noncomment (point-min))
			(ess-continued-statement-p)))
		 ess-continued-statement-offset
	       0))   ; Unless it starts a function body
	    ((/= (char-after containing-sexp) ?{)
	     ;; line is expression, not statement:
	     ;; indent to just after the surrounding open.
	     (goto-char containing-sexp)
	     (let ((bol (save-excursion (beginning-of-line) (point))))

	       ;; modified by shiba@isac 7.3.1992
	       (cond ((and (numberp ess-expression-offset)
			   (re-search-backward "[ \t]*expression[ \t]*" bol t))
		      ;; This regexp match every "expression".
		      ;; modified by shiba
		      ;;(forward-sexp -1)
		      (beginning-of-line)
		      (skip-chars-forward " \t")
		      ;; End
		      (+ (current-column) ess-expression-offset))
		     ((and (numberp ess-arg-function-offset)
			   (re-search-backward
			    "=[ \t]*\\s\"*\\(\\w\\|\\s_\\)+\\s\"*[ \t]*"
			    bol
			    t))
		      (forward-sexp -1)
		      (+ (current-column) ess-arg-function-offset))
		     ;; "expression" is searched before "=".
		     ;; End

		     (t
		      (progn (goto-char (1+ containing-sexp))
			     (current-column))))))
	    (t
	     ;; Statement level.  Is it a continuation or a new statement?
	     ;; Find previous non-comment character.
	     (goto-char indent-point)
	     (ess-backward-to-noncomment containing-sexp)
	     ;; Back up over label lines, since they don't
	     ;; affect whether our line is a continuation.
	     (while (eq (preceding-char) ?\,)
	       (ess-backward-to-start-of-continued-exp containing-sexp)
	       (beginning-of-line)
	       (ess-backward-to-noncomment containing-sexp))
	     ;; Now we get the answer.
	     (if (ess-continued-statement-p)
		 ;; This line is continuation of preceding line's statement;
		 ;; indent  ess-continued-statement-offset  more than the
		 ;; previous line of the statement.
		 (progn
		   (ess-backward-to-start-of-continued-exp containing-sexp)
		   (+ ess-continued-statement-offset (current-column)
		      (if (save-excursion (goto-char indent-point)
					  (skip-chars-forward " \t")
					  (eq (following-char) ?{))
			  ess-continued-brace-offset 0)))
	       ;; This line starts a new statement.
	       ;; Position following last unclosed open.
	       (goto-char containing-sexp)
	       ;; Is line first statement after an open-brace?
	       (or
		 ;; If no, find that first statement and indent like it.
		 (save-excursion
		   (forward-char 1)
		   (while (progn (skip-chars-forward " \t\n")
				 (looking-at "#"))
		     ;; Skip over comments following openbrace.
		     (forward-line 1))
		   ;; The first following code counts
		   ;; if it is before the line we want to indent.
		   (and (< (point) indent-point)
			(current-column)))
		 ;; If no previous statement,
		 ;; indent it relative to line brace is on.
		 ;; For open brace in column zero, don't let statement
		 ;; start there too.  If ess-indent-level is zero, use
		 ;; ess-brace-offset + ess-continued-statement-offset
		 ;; instead.
		 ;; For open-braces not the first thing in a line,
		 ;; add in ess-brace-imaginary-offset.
		 (+ (if (and (bolp) (zerop ess-indent-level))
			(+ ess-brace-offset ess-continued-statement-offset)
		      ess-indent-level)
		    ;; Move back over whitespace before the openbrace.
		    ;; If openbrace is not first nonwhite thing on the line,
		    ;; add the ess-brace-imaginary-offset.
		    (progn (skip-chars-backward " \t")
			   (if (bolp) 0 ess-brace-imaginary-offset))
		    ;; If the openbrace is preceded by a parenthesized exp,
		    ;; move to the beginning of that;
		    ;; possibly a different line
		    (progn
		      (if (eq (preceding-char) ?\))
			  (forward-sexp -1))
		      ;; Get initial indentation of the line we are on.
		      (current-indentation))))))))))

(defun ess-insert-function-outline ()
  "Insert an S function definition `outline' at point.
Uses the file given by the variable `ess-function-outline-file'."
  (interactive)
  (let ((oldpos (point)))
    (save-excursion
      (insert-file-contents ess-function-outline-file)
      (if (search-forward "$A$" nil t)
	  (replace-match (user-full-name) 'not-upcase 'literal))
      (goto-char oldpos)
      (if (search-forward "$D$" nil t)
	  (replace-match (ess-time-string 'clock) 'not-upcase 'literal)))
    (goto-char (1+ oldpos))))

;;*;; S/R  Pretty-Editing

(defun ess-fix-comments (&optional dont-query verbose)
  "Fix ess-mode buffer so that single-line comments start with at least `##'."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))
    (let ((rgxp "^\\([ \t]*#\\)\\([^#]\\)")
	  (to   "\\1#\\2"))
      (if dont-query
	  (ess-rep-regexp     rgxp to nil nil verbose)
	(query-replace-regexp rgxp to nil)))))

(defun ess-dump-to-src (&optional dont-query verbose)
  "Make the changes in an S - dump() file to improve human readability."
  (interactive "P")
  (save-excursion
    (if (not (equal major-mode 'ess-mode))
	(ess-mode))
    (goto-char (point-min))
    (let ((rgxp "^\"\\([a-z.][a-z.0-9]*\\)\"<-\n")
	  (to   "\n\\1 <- "))
      (if dont-query
	  (ess-rep-regexp     rgxp to nil nil verbose)
	(query-replace-regexp rgxp to nil)))))

(defun ess-num-var-round (&optional dont-query verbose)
  "Is VERY useful for dump(.)'ed numeric variables; ROUND some of them by
  replacing  endings of 000000*.. and 999999*.  Martin Maechler"
  (interactive "P")
  (save-excursion
    (goto-char (point-min))

    (let ((num 0)
	  (str "")
	  (rgxp "000000+[1-9]?[1-9]?\\>")
	  (to   ""))
      (if dont-query
	  (ess-rep-regexp     rgxp to nil nil verbose)
	(query-replace-regexp rgxp to nil))

      (while (< num 9)
	(setq str (concat (int-to-string num) "999999+[0-8]*"))
	(if (and (numberp verbose) (> verbose 1))
	    (message (format "\nregexp: '%s'" str)))
	(goto-char (point-min))
	(ess-rep-regexp str (int-to-string (1+ num))
			'fixedcase 'literal verbose)
	(setq num (1+ num))))))

;;; All of the above three :
(defun ess-MM-fix-src (&optional dont-query verbose)
  "Clean up ess-source code which has been produced by  dump(..).
 Produces more readable code, and one that is well formatted in emacs ess-mode."
  (interactive "P")
  ;; each of the following does a save-excursion:
  (ess-dump-to-src dont-query)
  (ess-fix-comments dont-query)
  (ess-num-var-round dont-query verbose))

(defun ess-fix-miscellaneous (&optional from verbose)
  "Fix Miscellaneous S/R `ill-formation's from current \\[point].
 Particularly use \"<-\"and put spaces around operators."
  (interactive "d\nP"); Defaults: point and prefix (C-u)
  (save-excursion

    (if (string= ess-dialect "R")
	(progn
	 (require 'essd-r)
	 (R-fix-T-F from (not verbose))))

    (goto-char from) (ess-rep-regexp " *_ *" " <- " nil 'literal verbose)

    ;; ensure space around  "<-"  ---- but only replace if necessary:
    (goto-char from)
    (ess-rep-regexp "\\([^< \t\n]\\)\\(<<?-\\)" "\\1 \\2" nil nil verbose)
    (goto-char from)(ess-rep-regexp "<-\\([^ \t\n]\\)" "<- \\1" nil nil verbose)
    ;; ensure space around  "<" (not in "<-","<=","<<-")  and ">" (not ">=") :
    (goto-char from);; --> " <", care with "->":
    (ess-rep-regexp "\\([^-< \t\n]\\)\\([<>]\\)" "\\1 \\2" nil nil verbose)
    ;; ">" -> "> " , for "<", don't split "<-" nor "<<-":
    (goto-char from)
    (ess-rep-regexp "\\(>=?\\)\\([^= \t\n]\\)" "\\1 \\2" nil nil verbose)
    (goto-char from)
    (ess-rep-regexp "\\(<=?\\)\\([^-<= \t\n]\\)" "\\1 \\2" nil nil t)

    ;; -- ensure space around "=", "==", "!=" :
    (goto-char from) ;; --> " ="
    (ess-rep-regexp "\\([^=!<> ]\\)\\([=!]?\\)=" "\\1 \\2=" nil nil verbose)
    (goto-char from) (ess-rep-regexp "=\\([^= ]\\)" "= \\1" nil nil verbose)

    (goto-char from) ;; add a space between "{" and surrounding ..char:
    (ess-rep-regexp "{\\([.A-Za-z()]\\)" "{ \\1" 'fix nil verbose)
    (ess-rep-regexp "\\([()]\\){" "\\1 {" 'fix nil verbose)
    (goto-char from) ;; add a space between "}" and a preceding wordchar:
    (ess-rep-regexp "\\([A-Za-z0-9()]\\)}" "\\1 }" 'fix nil verbose)
    (ess-space-around "else" from verbose)

    ;; add a newline and indent before a "}"
    ;; --- IFF there's NO "{" or "#" AND some NON-white text on the same line:
    ;;D (if verbose (message "\t R-fix-misc..: Hard.. '}'"))
    (goto-char from)
    (ess-rep-regexp "^\\([^#{\n]*[^#{ \t\n]+[ \t]*\\)}[ \t]*$"
		     "\\1\n}" 'fix nil verbose)
    ))

(defun ess-smart-underscore ()
  "Electrical \"_\" key: insert `ess-S-assign' after removing spaces, unless
 in string/comment.  `ess-S-assign', typically \" <- \", can be customized."
  (interactive)
  ;;(insert (if (inside-string/comment-p (point)) "_" ess-S-assign))
  (if (inside-string/comment-p (point))
      (insert "_")
    ;; else
    (delete-horizontal-space)
    (insert ess-S-assign)))

(defun ess-toggle-underscore (force)
  "Set the \"_\" (underscore) key to \\[ess-smart-underscore] or back to \"_\".
 Toggle the current definition, unless FORCE is non-nil, where
 \\[ess-smart-underscore] is set unconditionally.

 Using \"C-q _\" will always just insert the underscore character."
  (interactive "P")
  (require 'ess-mode)
  (require 'ess-inf)
  (let ((uscore (lookup-key ess-mode-map "_")))
    (if (and uscore
	     ;; (stringp uscore) (string= uscore ess-S-assign)
	     (not force))
	(progn
	 (define-key ess-mode-map          "_" nil); 'self-insert-command
	 (define-key inferior-ess-mode-map "_" nil))
      ;; else : "force" or uscore is "nil", i.e. default
      (define-key ess-mode-map          "_" 'ess-smart-underscore)
      (define-key inferior-ess-mode-map "_" 'ess-smart-underscore))))

;; NOTA BENE: "_" is smart *by default* :
;; -----  The user can always customize `ess-S-assign' ...
(ess-toggle-underscore 'force-to-S-assign)

(defun ess-add-MM-keys ()
  "Define MM's user keys, currently only \\<ess-mode-map>\\[ess-insert-function-outline]."
  (interactive)
  (require 'ess-mode)
  (define-key ess-mode-map "\C-cf" 'ess-insert-function-outline))

(defun ess-dump-args-and-go (Sfunc) ; &optional buff)
  "Dump the function name, with arguments, to a buffer for editing.

Currently, this needs to:
   1. set the buffer to the right mode, with the right settings
   2. format the statement,
   3. c/function/Sfunc/
and I need to relearn emacs lisp (but I had to, anyway."

  (interactive "sFunction ? ")
  (let* ((buffname "ess-complete.R")
	 (buf (ess-execute (format "args(%s)" Sfunc)
					  t
					  buffname)))
    (pop-to-buffer buf)
    (message "here yet?")
    (replace-string "function" Sfunc)
    (ess-setq-vars-local ess-customize-alist (current-buffer))
    (setq major-mode 'ess-mode)
    (use-local-map ess-mode-map)
    (set-syntax-table ess-mode-syntax-table)
    ))


(provide 'essl-s)

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

;;; essl-s.el ends here

