;;; essl-s.el --- Support for editing S source code

;; Copyright (C) 1989-1997 Bates, Kademan, Ritter, Smith, Hornik,
;; Heiberger, Maechler, and Rossini.

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossinI@stat.sc.edu>
;; Created: 26 Aug 1997
;; Modified: $Date: 1998/04/27 07:53:02 $
;; Version: $Revision: 5.3 $
;; RCS: $Id: essl-s.el,v 5.3 1998/04/27 07:53:02 maechler Exp $

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



 ; Configuration variables




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
			   (re-search-backward "=[ \t]*\\s\"*\\(\\w\\|\\s_\\)+\\s\"*[ \t]*" bol t))
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
		 ;; start there too.  If ess-indent-level is zero,
		 ;; use ess-brace-offset + ess-continued-statement-offset instead.
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
    (font-lock-defaults           . '(ess-mode-font-lock-keywords)))
  "General options for editing S, S+, and R source files.")


;;; Changes from S to S-PLUS 3.x.  (standard S3 should be in essl-s!).

(defconst S+3-help-sec-keys-alist
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


(defconst S3-help-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DESCRIPTION:")
    (?D . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
;;    (?o . "OPTIONAL ARGUMENTS:")
;;    (?r . "REQUIRED ARGUMENTS:")
    (?r . "REFERENCES:")
    (?s . "SEE ALSO:")
    (?S . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Help section keys for display.")


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

(defconst R-help-sec-keys-alist
  '((?a . "\\s *Arguments:")
    (?d . "\\s *Description:")
    (?e . "\\s *Examples:")
    (?n . "\\s *Note:")
    (?r . "\\s *References:")
    (?s . "\\s *See Also:")
    (?v . "\\s *Value[s]?")	;
    )) ;; "Alist of (key . string) pairs for use in section searching."

(defconst ess-help-S+3-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file")

(defconst ess-help-R-sec-regex "^\\s *[A-Z[a-z. ---]+:$")




;;; S4 stuff.

;; Based on files from:
;;     Copyright (C) 1996, John M. Chambers.
;; and
;;;    S-mode extras of SfS (Seminar für Statistik)


(if (not (fboundp 'ease:time-string))
    (defun ease:time-string (&optional clock)
      "Returns a string for use as a timestamp. + hr:min if CLOCK is non-nil.
	Currently returns strings like \"13 Mar 92\".  Redefine to taste."
      ;; RELIES on (current-time-string) : Must be  exactly
      ;; of this structure  [0..23], e.g. == "Mon Jan 27 17:30:45 1992"
      (let* ((time (current-time-string))
	     (mon (substring time 4 7))
	     (day (substring time 8 10))
	     (HM  (if clock (substring time 11 16)))
	     (year (substring time 22 24)))
	(concat day " " mon " " year
		(if clock (concat ", " HM))))))

(defvar ess-function-outline-file
  (concat ess-lisp-directory "/../etc/" "function-outline.S")
  "The file name of the ess-function outline that is to be inserted at point,
when \\<ess-mode-map>\\[ess-insert-function-outline] is used.
Placeholders (substituted `at runtime'): $A$ for `Author', $D$ for `Date'.")

;; Use the user's own ~/S/emacs-fun.outline  is (s)he has one : ---
(let ((outline-file (concat (getenv "HOME") "/S/function-outline.S")))
  (if (file-exists-p outline-file)
      (setq ess-function-outline-file outline-file)))

(defun ess-insert-function-outline ()
  "Insert an S function definition `outline' at point.
Uses the file given by the variable ess-function-outline-file;
M.Maechler,ess-extra"
;;---- FIXME: This should be done such that ONE 'undo' undoes all ! ---
  (interactive)
  (let ((oldpos (point)))
    (insert-file-contents ess-function-outline-file)
    (if (search-forward "$A$" nil t)
	(replace-match (user-full-name) 'not-upcase 'literal))
    (goto-char oldpos)
    (if (search-forward "$D$" nil t)
	(replace-match (ease:time-string 'clock) 'not-upcase 'literal))
    (goto-char (1+ oldpos))))

(defun ess-fix-comments ()
 "Fix ess-mode buffer so that single-line comments start with '##'."
 (interactive)
 (let ((curr (point)))
   (goto-char (point-min))
   (query-replace-regexp "^\\([ \\t]*#\\)\\([^#]\\)" "\\1#\\2" nil)
   (goto-char curr)))

(defun ess-dump-to-src ()
  "Make the changes in an S - dump() file to improve human readability"
  (interactive)
  (ess-mode)
  (query-replace-regexp "^\"\\([a-z.][a-z.0-9]*\\)\"<-\n"  "\n\\1 <- " nil))

(defun ess-num-var-round ()
 "Is VERY useful for dump(.)'ed numeric variables; ROUND some of them by
  replacing  endings of 000000*.. and 999999*.  Martin Maechler"
 (interactive)
  (let ((num 0)
	(str ""))
    (goto-char (point-min))
    (query-replace-regexp "000000+[1-9]?[1-9]?\\>" "" nil)
    (while (< num 9)
      (setq str (concat (int-to-string num) "999999+[0-8]*"))
      (princ (format "\nregexp: '%s'" str))
      (goto-char (point-min))
      (replace-regexp str (int-to-string (1+ num)))
      (setq num (1+ num)))))

(defun ess-MM-fix-src ()
  "Clean up ess-source code which has been produced by  dump(..).
 Produces more readable code, and one that is well formatted in emacs
 ess-mode. Martin Maechler, ETH Zurich."
  (interactive)
  (let ((pm (point-min)))
    (save-excursion
      (goto-char pm)
      (ess-dump-to-src)
      (goto-char pm)
      (ess-fix-comments)
      (goto-char pm)
      (ess-num-var-round))))

;;;--------- see earlier (RCS) versions of this file for older 'hacks..'

(defun ess-add-MM-keys ()
  (require 'ess-mode)
  (define-key ess-mode-map "\C-cf" 'ess-insert-function-outline))



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

