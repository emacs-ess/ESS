;;; essl-s.el --- Support for editing S source code

;; Copyright (C) 1989-1997 Bates, Kademan, Ritter, Smith, Hornik,
;; Heiberger, Maechler, and Rossini.

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossinI@stat.sc.edu>
;; Created: 26 Aug 1997
;; Modified: $Date: 1997/10/20 20:04:51 $
;; Version: $Revision: 1.15 $
;; RCS: $Id: essl-s.el,v 1.15 1997/10/20 20:04:51 rossini Exp $

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

;; Code for general editing S source code (specializes to S, S+, R). 

;;; Code:

 ; Requires and autoloads


(autoload 'S-transcript-mode
  "ess-trns" "ESS source eval mode" t)
(autoload 'R-transcript-mode
  "ess-trns" "ESS source eval mode" t)

;;;;* Alias ess-mode to s-mode
;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line. 
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*- S-*-" invokes s-mode and 
;;; not S-mode.
(fset 's-mode 'S-mode)
(fset 'r-mode 'R-mode)


 ; ess-transcript-mode

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files" t)

(defun s-transcript-mode ()
  "Does the right thing."
  (ess-transcript-mode S+3-customize-alist))


(fset 'S-transcript-mode 's-transcript-mode)

(defun r-transcript-mode ()
  "Does the right thing."
  (ess-transcript-mode R-customize-alist))

(fset 'R-transcript-mode 'r-transcript-mode)


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
  ;;(modify-syntax-entry ?.  "w"  S-syntax-table) ; used in S obj names
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
    (indent-line-function         . 'ess-indent-line)
    (require-final-newline        . t)
    (comment-start                . "#")
    (comment-start-skip           . "#+ *")
    (comment-column               . 40)
    (comment-indent-function      . 'ess-comment-indent)
    (parse-sexp-ignore-comments   . t)
    (ess-set-style                . ess-default-style)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . S-syntax-table)
    (font-lock-defaults           . '(ess-mode-font-lock-keywords)))
  "General options for editing S, S+, and R source files.")


;;; Changes from S to S-PLUS 3.x.  (standard S3 should be in essl-s!).

(defconst ess-help-S+3-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?D . "DESCRIPTION:")
    (?d . "DETAILS:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
    (?o . "OPTIONAL ARGUMENTS:")
    (?R . "REFERENCES:") 
    (?r . "REQUIRED ARGUMENTS:")
    (?S . "SEE ALSO:")
    (?s . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Alist of (key . string) pairs for use in section searching.")
;;; `key' indicates the keystroke to use to search for the section heading
;;; `string' in an S help file. `string' is used as part of a
;;; regexp-search, and so specials should be quoted.


(defvar S3-help-sec-keys-alist
  '((?a . "ARGUMENTS:")
    (?b . "BACKGROUND:")
    (?B . "BUGS:")
    (?d . "DETAILS:")
    (?D . "DESCRIPTION:")
    (?e . "EXAMPLES:")
    (?n . "NOTE:")
;;    (?o . "OPTIONAL ARGUMENTS:")
;;    (?r . "REQUIRED ARGUMENTS:")
    (?R . "REFERENCES:")
    (?S . "SEE ALSO:")
    (?s . "SIDE EFFECTS:")
    (?u . "USAGE:")
    (?v . "VALUE:"))
  "Help section keys for display.")


(defvar S4-help-sec-keys-alist
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

(defconst ess-help-R-sec-keys-alist 
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

(defvar ess-function-outline-file "/u/sfs/S/emacs-fun.outline"
  "The file name of the ess-function outline that is to be inserted at point,
when \\<ess-mode-map>\\[ess-insert-function-outline] is used.
Placeholders (substituted `at runtime'): $A$ for `Author', $D$ for `Date'.")
;;---------------------- currently :
;;- f <- function()
;;- {
;;-   ## Purpose:
;;-   ## ----------------------------------------------------------------------
;;-   ## Arguments:
;;-   ## ----------------------------------------------------------------------
;;-   ## Author: $A$, Date: $D$
;;- }



;; Use the user's own ~/S/emacs-fun.outline  is (s)he has one : ---
(let ((outline-file (concat (getenv "HOME") "/S/emacs-fun.outline")))
  (if (file-exists-p outline-file)
      (setq ess-function-outline-file outline-file)))

(defun ess-insert-function-outline ()
  "Insert an S function definition `outline' at point.
Uses the file given by the variable ess-function-outline-file;
M.Maechler,ess-extra" 
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
    (query-replace-regexp "000000+[1-9]" "" nil)
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
  ;; Martin's original code
  ;; (let ((curr (point))
  ;;   (pm   (point-min)))
  ;; (goto-char pm)   (ess-dump-to-src)
  ;; (goto-char pm)   (ess-fix-comments)
  ;; (goto-char pm)   (ess-num-var-round)
  ;; (goto-char curr)))
  ;; Kurt's suggestion
  (let ((pm (point-min)))
    (save-excursion
      (goto-char pm)
      (S-dump-to-src)
      (goto-char pm)
      (S-fix-comments)
      (goto-char pm)
      (S-num-var-round))))

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

