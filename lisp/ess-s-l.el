;;; ess-s-l.el --- Support for editing S source code

;; Copyright (C) 1989-1997 D. Bates, Kademan, Ritter, D.M. Smith, K. Hornik,
;;      R.M. Heiberger, M. Maechler, and A.J. Rossini.
;; Copyright (C) 1998-2015 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 26 Aug 1997
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

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; Code for general editing S source code (specializes to S, S+, R).

;;; Code:

 ; Requires and autoloads

(ess-message "[ess-s-l:] (def** ) only ...")
(require 'ess-utils)

 ; Configuration variables

(defvar S-syntax-table
  (let ((S-syntax-table (make-syntax-table)))
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
    (modify-syntax-entry ?\" "\"" S-syntax-table)
    (modify-syntax-entry ?#  "<"  S-syntax-table) ; open comment
    (modify-syntax-entry ?\n ">"  S-syntax-table) ; close comment
    ;;(modify-syntax-entry ?.  "w"  S-syntax-table) ; "." used in S obj names
    (modify-syntax-entry ?.  "_"  S-syntax-table) ; see above/below,
                                        ; plus consider separation.
    (modify-syntax-entry ?$  "_"  S-syntax-table); foo$comp = 1 symbol(completion)
    (modify-syntax-entry ?@  "_"  S-syntax-table); foo@slot = 1 symbol(completion)
    (modify-syntax-entry ?_  "_"  S-syntax-table)
    (modify-syntax-entry ?:  "_"  S-syntax-table)
    (modify-syntax-entry ?*  "."  S-syntax-table)
    (modify-syntax-entry ?<  "."  S-syntax-table)
    (modify-syntax-entry ?>  "."  S-syntax-table)
    (modify-syntax-entry ?/  "."  S-syntax-table)
    S-syntax-table)
  "Syntax table for S code."
  )

(defvar S-editing-alist
  '((paragraph-start              . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-separate           . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline        . mode-require-final-newline)
    ;;(comment-indent-function  . 'S-comment-indent)
    ;;(ess-comment-indent           . 'S-comment-indent)
    ;;(ess-indent-line                      . 'S-indent-line)
    ;;(ess-calculate-indent           . 'ess-calculate-indent)
    (indent-line-function         . 'ess-indent-line)
    (parse-sexp-ignore-comments   . t)
    (ess-style                    . ess-default-style)
    ;;(ess-keep-dump-files          . 'ask)
    (ess-mode-syntax-table        . S-syntax-table)
    ;; For Changelog add, require ' ' before <- : "attr<-" is a function name :
    (add-log-current-defun-header-regexp . "^\\(.+\\)\\s-+<-[ \t\n]*function")
    (ess-font-lock-keywords       . 'ess-S-font-lock-keywords)
    (ess-font-lock-defaults       . (ess--extract-default-fl-keywords ess-S-font-lock-keywords))
    (font-lock-defaults           . '(ess-font-lock-defaults
                                      nil nil ((?\. . "w") (?\_ . "w"))))
    )
  "General options for S and S+ source files.")

(defvar inferior-S-language-start
  '(concat "options("
           "STERM='"    ess-STERM  "'"
           ", str.dendrogram.last=\"'\""
           (if ess-editor (concat ", editor='" ess-editor "'"))
           (if ess-pager  (concat ", pager='"  ess-pager  "', help.pager='"  ess-pager  "'"))
           ", show.error.locations=TRUE"
           ")")
  "S language expression for startup -- default for all S dialects.")

(defconst S-common-cust-alist
  '((ess-language                  . "S")
    (inferior-ess-exit-command     . "q()\n")
    (inferior-ess-language-start   . (eval inferior-S-language-start))
    (comint-use-prompt-regexp      . t)  ;;use fields if nil
    (comint-process-echoes	   . t)
    ;; these prompt are the same for all S-languages As long as custom prompt
    ;; ends in inferior-ess-primary-prompt everything should work as expected.
    (inferior-ess-primary-prompt   . "> ")
    ;; (inferior-ess-secondary-prompt . "[+:] ") ;; catch Selection: and alike
    (inferior-ess-secondary-prompt . "+ ") ;; catch Selection: and alike
    (comment-start                . "#")
    (ess-imenu-generic-expression  . ess-imenu-S-generic-expression)
    (comment-add                  . 1)
    (comment-start-skip           . "#+ *")
    (comment-use-syntax           . t)  ; see log for bug report 2013-06-07
    (comment-column               . 40)
    (ess-no-skip-regexp           . (concat "^ *@\\|" (default-value 'ess-no-skip-regexp)))
    ;; inferior-ess-prompt is used by comint for navigation, only if
    ;; comint-use-prompt-regexp is t; (transcript-mode also relies on this regexp)
    (inferior-ess-prompt           . inferior-S-prompt) ;customizable
    (ess-get-help-topics-function  . 'ess-get-S-help-topics-function)
    (ess-getwd-command          . "getwd()\n")
    (ess-setwd-command          . "setwd('%s')\n")
    (ess-funargs-command        . ".ess_funargs(\"%s\")\n")
    (fill-nobreak-predicate     . 'ess-inside-string-p)
    (normal-auto-fill-function  . 'ess-do-auto-fill)
    (ess-execute-screen-options-command . "options(width=%d, length=99999)\n")
    )
  "S-language common settings for all <dialect>-customize-alist s")

(defconst S+common-cust-alist
  (append
   '((ess-suffix                . "S")
     (ess-mode-syntax-table     . S-syntax-table)
     (ess-help-sec-regex        . ess-help-S+-sec-regex)
     (ess-help-sec-keys-alist   . ess-help-S+sec-keys-alist)
     (ess-change-sp-regexp      . ess-S+-change-sp-regexp)
     (ess-function-pattern      . ess-s-function-pattern)
     (ess-function-template     . " <- \n#\nfunction()\n{\n\n}\n")
     (ess-dump-filename-template . (ess-replace-regexp-in-string
                                    "S$" ess-suffix ; in the one from custom:
                                    ess-dump-filename-template-proto))
     (ess-traceback-command     . "traceback()\n")
     (ess-mode-editing-alist    . S-editing-alist)

     (ess-dumped-missing-re
      . "\\(\\(<-\\|=\\)\nDumped\n\\'\\)\\|\\(\\(<-\\|=\\)\\(\\s \\|\n\\)*\\'\\)")
     (ess-syntax-error-re
      . "\\(Syntax error: .*\\) at line \\([0-9]*\\), file \\(.*\\)$")
     (inferior-ess-objects-command  . inferior-Splus-objects-command)
     (ess-describe-object-at-point-commands . 'ess-S-describe-object-at-point-commands)
     (inferior-ess-font-lock-keywords . 'inferior-S-font-lock-keywords)
     (ess-editor . S-editor)
     (ess-pager  . S-pager)
     )
   S-common-cust-alist)
  "Common settings for all S+<*>-customize-alist s"
  )

;;; Changes from S to S-PLUS 3.x.  (standard S3 should be in ess-s-l!).

(defconst ess-help-S+sec-keys-alist
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
(defconst ess-help-S3-sec-keys-alist
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
(defconst ess-help-S4-sec-keys-alist
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


(defconst ess-help-S+-sec-regex "^[A-Z. ---]+:$"
  "Reg(ular) Ex(pression) of section headers in help file.")

;;; S-mode extras of Martin Maechler, Statistik, ETH Zurich.
;;; See also ./ess-utils.el

(defvar ess-function-outline-file
  (concat ess-etc-directory  "/function-outline.S")
  "The file name of the ess-function outline that is to be inserted at point,
when \\[ess-insert-function-outline] is used.
Placeholders (substituted `at runtime'): $A$ for `Author', $D$ for `Date'.")

;; Use the user's own ~/S/emacs-fun.outline  if (s)he has one : ---
(let ((outline-file (concat (getenv "HOME") "/S/function-outline.S")))
  (if (file-exists-p outline-file)
      (setq ess-function-outline-file outline-file)))

;; Seth's idea; see ess-toggle-S-assign-key below
(defvar ess-S-assign-key [?\C-=] ;; = "\C-c=" ; old-default:  "_"
  "This key is mapped to insert `ess-S-assign' (by default '<-'),
when \\[ess-toggle-S-assign-key] is called.")

(defvar ess-S-assign-key-last nil
  "This caches the previous value (binding) of `ess-S-assign-key'.  It allows
 \\[ess-toggle-S-assign-key] to toggle back to the previous definition.")

 ; Function Definitions

(defun S-comment-indent ()
  "Indentation for S comments."
  (if (or (looking-at "###")
          (and (looking-at "#!") (= 1 (line-number-at-pos))))
      (current-column)
    (if (looking-at "##")
        (let ((tem (ess-calculate-indent)))
          (if (listp tem) (car tem) tem))
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
           comment-column))))

;; VS: these are ess-indent-line and ess-calculate-indent from 2004 already,so
;; commented out to avoid confusion:

;; (defun S-indent-line ()
;;   "Indent current line as S code.
;; Return the amount the indentation changed by."
;;   (let ((indent (S-calculate-indent nil))
;;      beg shift-amt
;;      (case-fold-search nil)
;;      (pos (- (point-max) (point))))
;;     (beginning-of-line)
;;     (setq beg (point))
;;     (cond ((eq indent nil)
;;         (setq indent (current-indentation)))
;;        (t
;;         (skip-chars-forward " \t")
;;         (cond ((and ess-indent-with-fancy-comments ;; ### or #!
;;                     (or (looking-at "###")
;;                         (and (looking-at "#!") (= 1 (line-number-at-pos)))))
;;                (setq indent 0))
;;               ;; Single # comment
;;               ((and ess-indent-with-fancy-comments
;;                     (looking-at "#") (not (looking-at "##")))
;;                (setq indent comment-column))
;;               (t
;;                (if (eq indent t) (setq indent 0))
;;                (if (listp indent) (setq indent (car indent)))
;;                (cond ((and (looking-at "else\\b")
;;                            (not (looking-at "else\\s_")))
;;                       (setq indent (save-excursion
;;                                      (ess-backward-to-start-of-if)
;;                                      (+ ess-else-offset (current-indentation)))))
;;                      ((= (following-char) ?})
;;                       (setq indent
;;                             (+ indent
;;                                (- ess-close-brace-offset ess-indent-offset))))
;;                      ((= (following-char) ?{)
;;                       (setq indent (+ indent ess-brace-offset))))))))
;;     (skip-chars-forward " \t")
;;     (setq shift-amt (- indent (current-column)))
;;     (if (zerop shift-amt)
;;      (if (> (- (point-max) pos) (point))
;;          (goto-char (- (point-max) pos)))
;;       (delete-region beg (point))
;;       (indent-to indent)
;;       ;; If initial point was within line's indentation,
;;       ;; position after the indentation.
;;       ;; Else stay at same point in text.
;;       (if (> (- (point-max) pos) (point))
;;        (goto-char (- (point-max) pos))))
;;     shift-amt))

;; (defun S-calculate-indent (&optional parse-start)
;;   "Return appropriate indentation for current line as S code.
;; In usual case returns an integer: the column to indent to.
;; Returns nil if line starts inside a string, t if in a comment."
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((indent-point (point))
;;        (beginning-of-defun-function nil) ;; don't call ess-beginning-of-function
;;        (case-fold-search nil)
;;        state
;;        containing-sexp)
;;       (if parse-start
;;        (goto-char parse-start)
;;      (beginning-of-defun))
;;       (while (< (point) indent-point)
;;      (setq parse-start (point))
;;      (setq state (parse-partial-sexp (point) indent-point 0))
;;      (setq containing-sexp (car (cdr state))))
;;       (cond ((or (nth 3 state) (nth 4 state))
;;           ;; return nil or t if should not change this line
;;           (nth 4 state))
;;          ((null containing-sexp)
;;           ;; Line is at top level.  May be data or function definition,
;;           (beginning-of-line)
;;           (if (and (/= (following-char) ?\{)
;;                    (save-excursion
;;                      (ess-backward-to-noncomment (point-min))
;;                      (ess-continued-statement-p)))
;;               ess-continued-statement-offset
;;             0))   ; Unless it starts a function body
;;          ((/= (char-after containing-sexp) ?{)
;;           ;; line is expression, not statement:
;;           ;; indent to just after the surrounding open.
;;           (goto-char containing-sexp)
;;           (let ((bol (save-excursion (beginning-of-line) (point))))

;;             ;; modified by shiba@isac 7.3.1992
;;             (cond ((and (numberp ess-expression-offset)
;;                         (re-search-backward "[ \t]*expression[ \t]*" bol t))
;;                    ;; This regexp match every "expression".
;;                    ;; modified by shiba
;;                    ;;(forward-sexp -1)
;;                    (beginning-of-line)
;;                    (skip-chars-forward " \t")
;;                    ;; End
;;                    (+ (current-column) ess-expression-offset))
;;                   ((and (numberp ess-arg-function-offset)
;;                         (re-search-backward
;;                          "=[ \t]*\\s\"?\\(\\w\\|\\s_\\)+\\s\"?[ \t]*"
;;                          bol
;;                          t))
;;                    (forward-sexp -1)
;;                    (+ (current-column) ess-arg-function-offset))
;;                   ;; "expression" is searched before "=".
;;                   ;; End

;;                   (t
;;                    (progn (goto-char (1+ containing-sexp))
;;                           (current-column))))))
;;          (t
;;           ;; Statement level.  Is it a continuation or a new statement?
;;           ;; Find previous non-comment character.
;;           (goto-char indent-point)
;;           (ess-backward-to-noncomment containing-sexp)
;;           ;; Back up over label lines, since they don't
;;           ;; affect whether our line is a continuation.
;;           (while (eq (preceding-char) ?\,)
;;             (ess-backward-to-start-of-continued-exp containing-sexp)
;;             (beginning-of-line)
;;             (ess-backward-to-noncomment containing-sexp))
;;           ;; Now we get the answer.
;;           (if (ess-continued-statement-p)
;;               ;; This line is continuation of preceding line's statement;
;;               ;; indent  ess-continued-statement-offset  more than the
;;               ;; previous line of the statement.
;;               (progn
;;                 (ess-backward-to-start-of-continued-exp containing-sexp)
;;                 (+ ess-continued-statement-offset (current-column)
;;                    (if (save-excursion (goto-char indent-point)
;;                                        (skip-chars-forward " \t")
;;                                        (eq (following-char) ?{))
;;                        ess-continued-brace-offset 0)))
;;             ;; This line starts a new statement.
;;             ;; Position following last unclosed open.
;;             (goto-char containing-sexp)
;;             ;; Is line first statement after an open-brace?
;;             (or
;;               ;; If no, find that first statement and indent like it.
;;               (save-excursion
;;                 (forward-char 1)
;;                 (while (progn (skip-chars-forward " \t\n")
;;                               (looking-at "#"))
;;                   ;; Skip over comments following openbrace.
;;                   (forward-line 1))
;;                 ;; The first following code counts
;;                 ;; if it is before the line we want to indent.
;;                 (and (< (point) indent-point)
;;                      (current-column)))
;;               ;; If no previous statement,
;;               ;; indent it relative to line brace is on.
;;               ;; For open brace in column zero, don't let statement
;;               ;; start there too.  If ess-indent-offset is zero, use
;;               ;; ess-brace-offset + ess-continued-statement-offset
;;               ;; instead.
;;               ;; For open-braces not the first thing in a line,
;;               ;; add in ess-brace-imaginary-offset.
;;               (+ (if (and (bolp) (zerop ess-indent-offset))
;;                      (+ ess-brace-offset ess-continued-statement-offset)
;;                    ess-indent-offset)
;;                  ;; Move back over whitespace before the openbrace.
;;                  ;; If openbrace is not first nonwhite thing on the line,
;;                  ;; add the ess-brace-imaginary-offset.
;;                  (progn (skip-chars-backward " \t")
;;                         (if (bolp) 0 ess-brace-imaginary-offset))
;;                  ;; If the openbrace is preceded by a parenthesized exp,
;;                  ;; move to the beginning of that;
;;                  ;; possibly a different line
;;                  (progn
;;                    (if (eq (preceding-char) ?\))
;;                        (forward-sexp -1))
;;                    ;; Get initial indentation of the line we are on.
;;                    (current-indentation))))))))))

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


;; typically bound to M-Enter
(defun ess-use-this-dir (&optional no-force-current)
  "Synchronise the current directory of the S or R process to the one of the current
buffer. If that buffer has no associated *R* process, use \\[ess-force-buffer-current],
unless prefix argument NO-FORCE-CURRENT is non-nil."
  (interactive "P")
  (unless no-force-current (ess-force-buffer-current "R process to use: "))
  (if ess-local-process-name
      (let ((cmd (format "setwd('%s')\n" default-directory))
            )
        (unless (string= ess-language "S")
          ;; FIXME: generalize this for Stata, SAS, Xlispstat... -- then move to ess-mode.el
          (error
           "ESS setting working directory in *%s* not yet implemented for language %s"
           ess-local-process-name ess-language))
        (ess-command cmd)
        (message "Directory of *%s* process set to %s"
                 ess-local-process-name default-directory))
    ;; no local process
    (message "No *%s* process associated with this buffer." ess-dialect)))


;;*;; S/R  Pretty-Editing

(defun ess-fix-comments (&optional dont-query verbose)
  "Fix ess-mode buffer so that single-line comments start with at least '##',
and ensure space before subsequent text."
  (interactive "P")
  (ess-replace-regexp-dump-to-src "#\\([A-Za-z0-9]\\)" "# \\1" nil verbose)
  (ess-replace-regexp-dump-to-src "^\\([ \t]*#\\)\\([^#]\\)"
                                  "\\1#\\2" dont-query verbose))

(defun ess-dump-to-src (&optional dont-query verbose)
  "Make the changes in an S - dump() file to improve human readability."
  (interactive "P")
  (ess-replace-regexp-dump-to-src  "^\"\\([a-z.][a-z.0-9]*\\)\" *<-\n"
                                   "\n\\1 <- "
                                   dont-query verbose 'ensure-ess))

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

(defun ess-fix-dot (before-chars &optional dont-query verbose)
  "Remove trailing decimal '.' (\"dot\"), before BEFORE; typically from S-plus"
  ;; typically, before-chars =  "]:" or more
  (ess-replace-regexp-dump-to-src
   (concat "\\([0-9]\\)\\.\\( *[" before-chars "]\\)")
   ;;           111      ^
   "\\1\\2" dont-query verbose))

(defun ess-fix-dot-1 (&optional do-query verbose)
  "Remove trailing decimal '.' (\"dot\"), before ':' or ']', i.e.,
in cases where it's ugly and nonsense.  DO-QUERY(prefix) asks before replacing."
  (interactive "P")
  (ess-fix-dot "]:" (not do-query) verbose))

(defun ess-fix-dot-more (&optional dont-query verbose)
  "Remove trailing decimal '.' (\"dot\", typically from S+) in more cases
 than `ess-fix-dot-1'."
  (interactive "P")
  (ess-fix-dot-1 nil verbose)
  (ess-fix-dot ",)" dont-query verbose))

(defun ess-fix-EQ-assign (&optional dont-query verbose not-all)
  "Replace \"=\" by \"<-\" in places where it 'might make sense', e.g.,
for function assignments and lines not ending in \",\".
Be *careful* for list()s of functions and when argument not-all is
nil (as by default) !"
  ;;TODO: "in the few places we can be very sure.."
  ;;---- is hard in general: local functions: ok; but functions in
  ;;  list(a = function(x) abs(x), b= function(y) bound(y))  *NOT* ok!
  (interactive "P")
  (ess-replace-regexp-dump-to-src
   "^\\( *[a-z.][_a-z.0-9]*\\) *= *\\(function *(\\)"
   "\\1 <- \\2" dont-query verbose)

  (unless not-all
    ;; "too" aggressive {proposing to replace function argument specs}:
    (ess-replace-regexp-dump-to-src ;; all those *not* ending in ","
     ;; including  Mat[ i, ] = ...,
     ;; but not `names(x) = "..."' for that is "confused" with plot(x=x,..)
     "^\\( *[a-z.][][, \"_a-z.0-9]*\\) *= *\\([a-z.0-9({]\\(.*[^,]\\)? *$\\)"
     "\\1 <- \\2" nil ;; always query - often has many "false positives"
     verbose)
    ))

;;; All of the above three :
(defun ess-MM-fix-src (&optional dont-query verbose)
  "Clean up ess-source code which has been produced by dump(..), and other
code typically produced by other tools.  Produces more readable code,
and one that is well formatted in emacs ess-mode."
  (interactive "P")
  ;; each of the following does a save-excursion:
  (ess-dump-to-src dont-query)
  (ess-fix-comments dont-query)
  (ess-num-var-round dont-query verbose)
  (ess-fix-dot-more dont-query verbose)
  (ess-fix-EQ-assign dont-query verbose 'not-all)
  )

(defun ess-fix-miscellaneous (&optional from verbose)
  "Fix Miscellaneous S/R `ill-formation's from current \\[point].
 Particularly use \"<-\"and put spaces around operators."
  (interactive "d\nP"); Defaults: point and prefix (C-u)
  ;; activate by (setq ess-verbose t)
  (ess-if-verbose-write
   (format "ess-fix-misc begin (from = %s, verbose = %s)\n" from verbose))
  (save-excursion

    (if (string= ess-dialect "R")
        (progn
          (require 'ess-r-d)
          (R-fix-T-F from (not verbose))))

    ;; activate by (setq ess-verbose t)
    (ess-if-verbose-write "ess-fix-misc: after fix-T-F\n");___D___

    ;; former C and matlab programmers leave trailing  ";" :
    ;; (goto-char from) (ess-rep-regexp "; *$" "" nil 'literal verbose)
    ;; (ess-if-verbose-write "ess-fix-misc: after trailing ';'\n");___D___
    (goto-char from) (ess-rep-regexp ";\\( *\\)#" "\\1#" nil nil verbose)
    (ess-if-verbose-write "ess-fix-misc: after ';' before #\n");___D___

    ;;from R 1.9.x "_" is valid in names; here assume no initial / trailing '_'
    ;; BUG: The following changes "beta_ " or " _abc"
    ;; (goto-char from) (ess-rep-regexp " +_ *" " <- " nil 'literal verbose)
    ;; (goto-char from) (ess-rep-regexp   "_ +" " <- " nil 'literal verbose)

    (ess-if-verbose-write "ess-fix-misc: before 'around \"<-\"' :\n");___D___
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

    (ess-if-verbose-write "ess-fix-misc: before \"=\" \"==\" .. :\n");___D___
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

    (ess-if-verbose-write "ess-fix-misc: after \"{ ... }\" :\n");___D___
    (goto-char from) ;; add a space inside "){"
    (ess-rep-regexp "){" ") {" 'fix nil verbose)

    ;; add a newline and indent before a "}"
    ;; --- IFF there's NO "{" or "#" AND some NON-white text on the same line:
    ;;D (if verbose (message "\t R-fix-misc..: Hard.. '}'"))
    (goto-char from)
    (ess-rep-regexp "^\\([^#{\n]*[^#{ \t\n]+[ \t]*\\)}[ \t]*$"
                    "\\1\n}" 'fix nil verbose)
    (ess-if-verbose-write "ess-fix-misc __end__\n");___D___
    ))

;; This is by Seth Falcon, modeled after ess-toggle-underscore (see below).
(defun ess-toggle-S-assign-key (force)
  "Possibly bind the key in `ess-S-assign-key' to inserting `ess-S-assign'.
If `ess-S-assign-key' is \"_\", simply use \\[ess-toggle-underscore].
Otherwise, unless the prefix argument FORCE is set,
toggle between the new and the previous assignment."
  (interactive "P")
  (require 'ess-mode)
  (require 'ess-inf)
  (let ((current-action (lookup-key ess-mode-map ess-S-assign-key))
        (insert-S-assign (lambda() (interactive)
                           (delete-horizontal-space) (insert ess-S-assign))))
    (if (and (stringp ess-S-assign-key)
             (string= ess-S-assign-key "_"))
        (ess-toggle-underscore force)
      ;; else "do things here"
      (let* ((current-is-S-assign (eq current-action insert-S-assign))
             (new-action (if force insert-S-assign
                           ;; else "not force" (default):
                           (if (or current-is-S-assign
                                   (eq ess-S-assign-key-last insert-S-assign))
                               ess-S-assign-key-last
                             insert-S-assign))))
        (message "[ess-toggle-S-assign-key:] current: '%s', new: '%s'"
                 current-action new-action)
        (define-key ess-mode-map          ess-S-assign-key new-action)
        (define-key inferior-ess-mode-map ess-S-assign-key new-action)
        (if (not (and force current-is-S-assign))
            (setq ess-S-assign-key-last current-action))))))

(defvar polymode-mode)
(defun ess-smart-S-assign ()
  "Act as smart `ess-S-assign' key: insert `ess-S-assign', unless in string/comment.
If the underscore key is pressed a second time, the assignment
operator is removed and replaced by the underscore.  `ess-S-assign',
typically \" <- \", can be customized.  In ESS modes other than R/S,
the underscore is always inserted."
  (interactive)
  ;;(insert (if (ess-inside-string-or-comment-p (point)) "_" ess-S-assign))
  (save-restriction
    (ignore-errors
      (when (and (eq major-mode 'inferior-ess-mode)
                 (> (point) (process-mark (get-buffer-process (current-buffer)))))
        (narrow-to-region (process-mark (ess-get-process)) (point-max)))
      (and ess-noweb-mode
           (ess-noweb-in-code-chunk)
           (ess-noweb-narrow-to-chunk))
      (and (fboundp 'pm/narrow-to-span)
           polymode-mode
           (pm/narrow-to-span)))
    (if (or
         (ess-inside-string-or-comment-p (point))
         (not (equal ess-language "S")))
        (insert ess-smart-S-assign-key)
      ;; else:
      (ess-insert-S-assign))))
(defalias 'ess-smart-underscore 'ess-smart-S-assign)

(defun ess-insert-S-assign ()
  "Insert the assignment operator `ess-S-assign', unless it is already there.
In that case, it is removed and replaced by `ess-smart-S-assign-key'.
  `ess-S-assign', typically \" <- \", can be customized."
  (interactive)
  ;; one keypress produces ess-S-assign; a second keypress will delete
  ;; ess-S-assign and instead insert _
  ;; Rather than trying to count a second _ keypress, just check whether
  ;; the current point is preceded by ess-S-assign.
  (let ((assign-len (length ess-S-assign)))
    (if (and
         (>= (point) (+ assign-len (point-min))) ;check that we can move back
         (save-excursion
           (backward-char assign-len)
           (looking-at ess-S-assign)))
        ;; If we are currently looking at ess-S-assign, replace it with _
        (progn
          (delete-char (- assign-len))
          (insert ess-smart-S-assign-key))
      (if (string= ess-smart-S-assign-key "_")
          (delete-horizontal-space))
      (insert ess-S-assign))))

;;; Setting / Unsetting the smart S-assign-key behavior -----------------

;; Two basic building blocks, used below:
(defun ess--unset-smart-S-assign-key ()
  (define-key ess-mode-map          "_" nil)
  (define-key inferior-ess-mode-map "_" nil)
  (define-key ess-mode-map          ess-smart-S-assign-key nil); 'self-insert-command
  (define-key inferior-ess-mode-map ess-smart-S-assign-key nil))
(defun ess--activate-smart-S-assign-key ()
  (define-key ess-mode-map          ess-smart-S-assign-key 'ess-smart-S-assign)
  (define-key inferior-ess-mode-map ess-smart-S-assign-key 'ess-smart-S-assign))


;; Written such that whimps can have (ess-disable-smart-S-assign) in .emacs :
(defun ess-disable-smart-S-assign (activate)
  "Disable or activate (if prefix argument ACTIVATE is set) the smart assignment
operator `ess-S-assign'.  That, typically \" <- \", can be customized."
  (interactive "P")
  (if activate
      (ess--activate-smart-S-assign-key)
    (ess--unset-smart-S-assign-key)))
(defalias 'ess-disable-smart-underscore 'ess-disable-smart-S-assign)

(defun ess-toggle-S-assign (force)
  "Set the `ess-smart-S-assign-key' (by default \"_\"
 [underscore]) key to \\[ess-smart-S-assign] or back to
`ess-smart-S-assign-key'.  Toggle the current definition, unless
FORCE is non-nil, where \\[ess-smart-S-assign] is set
unconditionally.

If you as per default have `ess-smart-S-assign-key' set to
underscore, note that using \"C-q _\" will always just insert the
underscore character."
  (interactive "P")
  (let ((current-key (lookup-key ess-mode-map ess-smart-S-assign-key))
        (default-key (lookup-key ess-mode-map "_")))
    (if (and (or default-key current-key)
             ;; (stringp current-key) (string= current-key ess-S-assign)
             (not force))
        (ess--unset-smart-S-assign-key)
      ;; else : "force" or current-key is "nil", i.e. default
      (ess--activate-smart-S-assign-key))))
(defalias 'ess-toggle-underscore 'ess-toggle-S-assign)
;; NOTA BENE: "_" is smart *by default* :
;; -----  The user can always customize `ess-S-assign' ...
(ess-toggle-S-assign 'force-to-S-assign)

(defun ess-add-MM-keys ()
  "Define MM's user keys, currently \\<ess-mode-map>\\[ess-insert-function-outline], and
 \\<inferior-ess-mode-map>\\[ess-execute-screen-options]."
  (interactive)
  (require 'ess-mode); typically unnecessary
  (require 'ess-inf); dito
  (define-key ess-mode-map          "\C-cf" 'ess-insert-function-outline)
  (define-key inferior-ess-mode-map "\C-cw" 'ess-execute-screen-options)

  ;; Make M-- : [Alt] + [-] (in addition to / instead of  "_" = (on US-keyboard) [Shift]+ [-]
  ;; Note this overwrites 'M--' as "negative argument" (still on 'C--'):
  (define-key ess-mode-map          [?\M--] 'ess-insert-S-assign)
  (define-key inferior-ess-mode-map [?\M--] 'ess-insert-S-assign)
  )


(defun ess-dump-args-and-go (Sfunc) ; &optional buff)
  "Dump the function name, with arguments, to a buffer for editing.

Currently, this needs to:
   1. set the buffer to the right mode, with the right settings
   2. format the statement,
   3. c/function/Sfunc/
and I need to relearn emacs lisp (but I had to, anyway."

  (interactive "sFunction ? ")
  (let* ((buffname "ess-complete.R")
         (buf (ess-execute (format "args(%s)" Sfunc) t buffname)))
    (pop-to-buffer buf)
    (message "here yet?")
    (while (search-forward "function" nil t)
      (replace-match Sfunc nil t))
    (ess-setq-vars-local ess-customize-alist); (current-buffer))
    (setq major-mode 'ess-mode)
    (use-local-map ess-mode-map)
    (set-syntax-table ess-mode-syntax-table)
    ))

(defun ess-chm-display-help-on-object (object &rest args)
  (ess-eval-linewise (concat "help(" object ")")))


;;; S imenu support

;; don't use syntax classes, bad for etags
(defvar ess-imenu-S-generic-expression
  '(("Functions" "^\\(.+\\)[ \t\n]*<-[ \t\n]*function[ ]*(" 1)
    ("Classes" "^.*setClass(\\(.*\\)," 1)
    ("Coercions" "^.*setAs(\\([^,]+,[^,]*\\)," 1) ; show from and to
    ("Generics" "^.*setGeneric(\\([^,]*\\)," 1)
    ("Methods" "^.*set\\(Group\\|Replace\\)?Method(\\([^,]+,[^,]*\\)" 2)
    ;;[ ]*\\(signature=\\)?(\\(.*,?\\)*\\)," 1)
    ;;
    ;;("Other" "^\\(.+\\)\\s-*<-[ \t\n]*[^\\(function\\|read\\|.*data\.frame\\)]" 1)
    ("Package" "^.*\\(library\\|require\\)(\\(.*\\)" 2)
    ("Data" "^\\(.+\\)[ \t\n]-*<-[ \t\n]*\\(read\\|.*data\.frame\\).*(" 1)))

(defun ess-imenu-S (&optional arg)
  "S Language Imenu support for ESS."
  (interactive)
  (setq imenu-generic-expression ess-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu-S"))

(defalias 'ess-imenu-R 'ess-imenu-S)


 ;;; Speedbar stuff.
(defun ess-S-initialize-speedbar ()
  "Extend to all extensions; see initialization, and edit."
  (speedbar-add-supported-extension ".R")
  (speedbar-add-supported-extension ".S")
  (speedbar-add-supported-extension ".s")
  (speedbar-add-supported-extension ".q"))

                                        ;(if (featurep 'speedbar)
                                        ;    (progn
                                        ;      (message "enabling speedbar support")
                                        ;      (require 'speedbar)
                                        ;      (ess-S-initialize-speedbar)))

(eval-when-compile
  (condition-case nil
      (progn
        (require 'speedbar)
        (when (featurep 'speedbar)

          (defun S-speedbar-buttons (buffer)
            "attempted hack."

            ;;(speedbar-make-tag-line)
            ;;(speedbar-insert-button)
            (speedbar-with-writable))

          (fset 'R-speedbar-buttons 'S-speedbar-buttons)

          (defun S-speedbar-menu-items  ( )
            "Need to write.")

          (ess-S-initialize-speedbar)))
    (error nil)))

;;; On a PC, the default is S+.
;; Elsewhere (unix and linux) the default is S+
(cond  (ess-microsoft-p
        ;; MS-Windows-------------------------------------------------

        ;;        (fset 'S
        ;;           (if (equal (file-name-nondirectory shell-file-name) "cmdproxy.exe")
        ;;               'S+-msdos
        ;;             'S+))
        (defun S-by-icon (&rest x)
          (interactive)
          (message "Please start S+ from the icon.
 Then you can connect emacs to it with `M-x S-existing'.")
          )
        (fset 'S 'S-by-icon)
        (fset 'S-existing
              (if (equal (file-name-nondirectory shell-file-name) "cmdproxy.exe")
                  'S+-msdos-existing
                'S+-existing))
        (fset 'Sqpe 'Sqpe+)
        (fset 's-mode 'S+-mode)
        (fset 's-transcript-mode 'S+-transcript-mode))

       (t ;;((eq system-type 'gnu/linux)
        ;; Linux etc (including Mac OSX !?) --------------------------
        (fset 'S 'S+)
        (fset 's-mode 'S+-mode)
        (fset 's-transcript-mode 'S+-transcript-mode)))

;;;;* Alias S-mode to s-mode
;;; Emacs will set the mode for a file based on the file's header.
;;; The mode name is indicated by putting it between -*- on the top line.
;;; (Other commands can go here too, see an Emacs manual.)
;;; For a file you also load, you will want a leading # (comment to S)
;;; Emacs will downcase the name of the mode, e.g., S, so we must provide
;;; s-mode in lower case too.  That is, "#-*- S-*-" invokes s-mode and
;;; not S-mode.
(fset 'S-transcript-mode 's-transcript-mode)
(fset 'S-mode 's-mode)

(provide 'ess-s-l)

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

;;; ess-s-l.el ends here
