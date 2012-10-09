;;; ess-omg-l.el --- Support for editing Omega source code

;; Copyright (C) 1999--2001 A.J. Rossini.
;; Copyright (C) 2002--2004 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@u.washington.edu>
;; Created: 15 Aug 1999
;; Maintainer: ESS-core <ESS-core@r-project.org>

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

;; Code for general editing Omega source code.  This is initially
;; based upon the similarities between Omega and S, but will need to
;; diverge to incorporate the use of Java-style coding.

;;; Code:

 ; Requires and autoloads


 ; Specialized functions

(defun OMG-comment-indent ()
  "Indentation for Omega comments."

  (if (looking-at "////")
      (current-column)
    (if (looking-at "///")
        (let ((tem (ess-calculate-indent)))
          (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

(defun OMG-indent-line ()
  "Indent current line as Omega code.
Return the amount the indentation changed by."
  (let ((indent (ess-calculate-indent nil))
        beg shift-amt
        (case-fold-search nil)
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (cond ((eq indent nil)
           (setq indent (current-indentation)))
          (t
           (skip-chars-forward " \t")
           (if (and ess-fancy-comments (looking-at "////"))
               (setq indent 0))
           (if (and ess-fancy-comments
                    (looking-at "//")
                    (not (looking-at "///")))
               (setq indent comment-column)
             (if (eq indent t) (setq indent 0))
             (if (listp indent) (setq indent (car indent)))
             (cond ((and (looking-at "else\\b")
                         (not (looking-at "else\\s_")))
                    (setq indent (save-excursion
                                   (ess-backward-to-start-of-if)
                                   (+ ess-else-offset
                                      (current-indentation)))))
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


(defun OMG-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Omega code.
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
                                (looking-at "//"))
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




(defvar OMG-syntax-table nil "Syntax table for Omegahat code.")
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
  ;;FIXME: This fails (warning in compilation):
  ;;F "//" are 2 characters; ?// is invalid
  ;;F NEXT LINE IS BOGUS IN XEMACS, AJR
  ;;F (modify-syntax-entry ?//  "<"  S-syntax-table) ; open comment
  ;;F (modify-syntax-entry ?\n ">"  S-syntax-table) ; close comment
  ;;(modify-syntax-entry ?.  "w"  S-syntax-table) ; "." used in S obj names
  (modify-syntax-entry ?.  "_"  S-syntax-table) ; see above/below,
                                        ; plus consider separation.
  (modify-syntax-entry ?$  "_"  S-syntax-table) ; foo.bar$hack is 1 symbol
  (modify-syntax-entry ?_  "."  S-syntax-table)
  (modify-syntax-entry ?*  "."  S-syntax-table)
  (modify-syntax-entry ?<  "."  S-syntax-table)
  (modify-syntax-entry ?>  "."  S-syntax-table)
  (modify-syntax-entry ?/  "."  S-syntax-table))


(defvar OMG-editing-alist
  '((paragraph-start              . (concat "^$\\|" page-delimiter))
    (paragraph-separate           . (concat "^$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline        . t)
    (comment-start                . "//")
    (comment-start-skip           . "//+ *")
    (comment-column               . 40)
    ;;(comment-indent-function  . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-indent-line                      . 'S-indent-line)
    ;;(ess-calculate-indent           . 'ess-calculate-indent)
    (indent-line-function            . 'ess-indent-line)
    (parse-sexp-ignore-comments   . t)
    (ess-style                . ess-default-style)
    (ess-local-process-name       . nil)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . S-syntax-table)
    (font-lock-defaults           . '(ess-OMG-font-lock-defaults
                                      nil nil ((?\. . "w")))))
  "General options for Omegahat source files.")

(defvar ess-OMG-font-lock-defaults
  (append (list
           (cons "\\b[0-9]+\\b" 'font-lock-type-face) ; numbers
           (cons (concat "\\<" (regexp-opt ess-S-keywords 'enc-paren) "\\>")
                 'font-lock-keyword-face))
          (list
           (cons (regexp-opt ess-S-assign-ops)
                 'font-lock-constant-face)     ; assign
           (cons (concat "\\<" (regexp-opt ess-S-constants 'enc-paren) "\\>")
                 'font-lock-type-face)          ; constants
           (cons (concat "\\<" (regexp-opt ess-S-modifyiers 'enc-paren) "\\>")
                 'font-lock-constant-face)     ; modify search list or source

           (cons ess-S-function-name-regexp
                 '(1 font-lock-function-name-face keep))
                                        ; function name
           (cons "\\s.\\|\\s(\\|\\s)" 'font-lock-function-name-face)
                                        ;punctuation and parents  (same as function not to cause vidual disturbance)
           ))        ; keywords
  "Font-lock patterns used in `OMG' buffers.")


;;; Changes from S to S-PLUS 3.x.  (standard S3 should be in ess-s-l.el !).

(defconst OMG-help-sec-keys-alist
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

(defconst ess-help-OMG-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file")

;;;    S-mode extras of Martin Maechler, Statistik, ETH Zurich.

;;>> Moved things into --> ./ess-utils.el

(provide 'ess-omg-l)

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

;;; ess-omg-l.el ends here
