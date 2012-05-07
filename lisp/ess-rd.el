;; ess-rd.el --- Support for editing R documentation (Rd) source

;; Copyright (C) 1997--2005  A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: KH <Kurt.Hornik@ci.tuwien.ac.at>
;; Created: 25 July 1997
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS (Emacs Speaks Statistics).

;; This file is free software; you may redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any
;; later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; A copy of the GNU General Public License is available on the World
;; Wide Web at http://www.gnu.org/copyleft/gpl.html.  You can also
;; obtain it by writing to the Free Software Foundation, Inc., 675 Mass
;; Ave, Cambridge, MA 02139, USA.

;;; Code:

;; To stave off byte compiler errors
(eval-when-compile (require 'ess-help))

(defvar essddr-version "0.9-1"
  "Current version of ess-rd.el.")

(defvar essddr-maintainer-address
  "ESS Core Team <ess-core@r-project.org>"
  "Current maintainer of ess-rd.el.")

(defun Rd-active-mark () nil)           ;silence compiler.
(if (featurep 'xemacs)
    ;; Special support for XEmacs (curtesy of auctex):
    (defun Rd-active-mark ()
      (and zmacs-regions (mark)))

  ;; else:  special support for GNU Emacs
  (defun Rd-active-mark ()
    (and transient-mark-mode mark-active))
  )

(autoload 'ess-eval-region              "ess-inf" "[autoload]" t)
(autoload 'ess-eval-line-and-step       "ess-inf" "[autoload]" t)
(autoload 'ess-switch-process           "ess-inf" "[autoload]" t)
(autoload 'ess-switch-to-ESS            "ess-inf" "[autoload]" t)
(autoload 'ess-switch-to-end-of-ESS     "ess-inf" "[autoload]" t)

(autoload 'ess-help-mode                "ess-help" "[autoload]" t)
(autoload 'ess-nuke-help-bs             "ess-help" "[autoload]" t)

(defvar Rd-mode-abbrev-table nil
  "Abbrev table for R documentation keywords.
All Rd mode abbrevs start with a grave accent (`).")
(if Rd-mode-abbrev-table
    ()
  (define-abbrev-table 'Rd-mode-abbrev-table ())
  (define-abbrev Rd-mode-abbrev-table "`ag" "\\arguments")
  (define-abbrev Rd-mode-abbrev-table "`al" "\\alias")
  (define-abbrev Rd-mode-abbrev-table "`au" "\\author")
  (define-abbrev Rd-mode-abbrev-table "`bf" "\\bold")
  (define-abbrev Rd-mode-abbrev-table "`co" "\\code")
  (define-abbrev Rd-mode-abbrev-table "`de" "\\describe")
  (define-abbrev Rd-mode-abbrev-table "`dn" "\\description")
  (define-abbrev Rd-mode-abbrev-table "`dt" "\\details")
  (define-abbrev Rd-mode-abbrev-table "`em" "\\emph")
  (define-abbrev Rd-mode-abbrev-table "`en" "\\enumerate")
  (define-abbrev Rd-mode-abbrev-table "`ex" "\\examples")
  (define-abbrev Rd-mode-abbrev-table "`fi" "\\file")
  (define-abbrev Rd-mode-abbrev-table "`fo" "\\format")
  (define-abbrev Rd-mode-abbrev-table "`it" "\\item")
  (define-abbrev Rd-mode-abbrev-table "`iz" "\\itemize")
  (define-abbrev Rd-mode-abbrev-table "`kw" "\\keyword")
  (define-abbrev Rd-mode-abbrev-table "`li" "\\link")
  (define-abbrev Rd-mode-abbrev-table "`me" "\\method")
  (define-abbrev Rd-mode-abbrev-table "`na" "\\name")
  (define-abbrev Rd-mode-abbrev-table "`no" "\\note")
  (define-abbrev Rd-mode-abbrev-table "`re" "\\references")
  (define-abbrev Rd-mode-abbrev-table "`sa" "\\seealso")
  (define-abbrev Rd-mode-abbrev-table "`se" "\\section")
  (define-abbrev Rd-mode-abbrev-table "`so" "\\source")
  (define-abbrev Rd-mode-abbrev-table "`ss" "\\subsection")
  (define-abbrev Rd-mode-abbrev-table "`sy" "\\synopsis")
  (define-abbrev Rd-mode-abbrev-table "`ta" "\\tabular")
  (define-abbrev Rd-mode-abbrev-table "`ti" "\\title")
  (define-abbrev Rd-mode-abbrev-table "`us" "\\usage")
  (define-abbrev Rd-mode-abbrev-table "`va" "\\value"))

(defvar Rd-mode-syntax-table nil
  "Syntax table for Rd mode.")
(if Rd-mode-syntax-table
    ()
  (setq Rd-mode-syntax-table (copy-syntax-table text-mode-syntax-table))
  (modify-syntax-entry ?\\ "\\" Rd-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" Rd-mode-syntax-table)
  (modify-syntax-entry ?\} "){" Rd-mode-syntax-table)
  ;; Nice for editing, not for parsing ...
  (modify-syntax-entry ?\( "()" Rd-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" Rd-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" Rd-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" Rd-mode-syntax-table)
  ;; To get strings right
  ;; (modify-syntax-entry ?\' "\"" Rd-mode-syntax-table)
  (modify-syntax-entry ?\" "\"" Rd-mode-syntax-table)
  ;; To make abbrevs starting with a grave accent work ...
  (modify-syntax-entry ?\` "w" Rd-mode-syntax-table)
  ;; Comments
  (modify-syntax-entry ?\% "<" Rd-mode-syntax-table)
  (modify-syntax-entry ?\n ">" Rd-mode-syntax-table))

(defvar Rd-mode-parse-syntax-table nil
  "Syntax table for parsing Rd mode.")
(if Rd-mode-parse-syntax-table
    ()
  (setq Rd-mode-parse-syntax-table
        (copy-syntax-table Rd-mode-syntax-table))
  ;; To make parse-partial-sexps do the thing we want for computing
  ;; indentations
  (modify-syntax-entry ?\( "_" Rd-mode-parse-syntax-table)
  (modify-syntax-entry ?\) "_" Rd-mode-parse-syntax-table)
  (modify-syntax-entry ?\[ "_" Rd-mode-parse-syntax-table)
  (modify-syntax-entry ?\] "_" Rd-mode-parse-syntax-table))

(defvar Rd-section-names
  '("Rdversion" "arguments" "alias" "author" "concept" "describe" "description"
    "details" "docType" "encoding" "enumerate" "examples" "format"
    "itemize" "keyword" "name" "note" "preformatted" "references"
    "seealso" "section" "source" "subsection" "synopsis"
    "tabular" "title" "usage"
    "value"))

(defvar Rd-keywords
  '(
    ;; the next two lines: only valid in R <= 2.8.1
    ;; commented out on 2011-01-14 for ESS version 5.13:
    ;; "Alpha" "Gamma" "alpha" "beta" "epsilon" "lambda" "mu" "pi" "sigma"
    ;; "ge" "le" "left" "right"
    ;;
    "R" "RdOpts" "S3method" "S4method" "Sexpr" "acronym"
    "bold" "cite" "code" "command" "cr" "dQuote" "deqn" "dfn" "dontrun"
    "dontshow" "donttest" "dots" "email" "emph" "enc" "env" "eqn" "figure" "file"
    "href" "if" "ifelse"
    "item" "kbd" "ldots" "linkS4class" "link" "method"
    "newcommand" "option" "out"
    "pkg" "sQuote" "renewcommand"
    "samp" "strong" "tab" "url" "var" "verb"
    ))

;; Need to fix Rd-bold-face problem.
;;
;; (defvar Rd-bold-face 'bold)
                                        ;(defvar Rd-bold-face nil)
                                        ;(make-face Rd-bold-face "R documentation bold face")
                                        ;(make-face-bold Rd-bold-face

(defvar Rd-font-lock-keywords
  (list
   (cons
    (concat "\\\\\\("
            (mapconcat 'identity Rd-section-names "\\|")
            "\\>\\)")
    'font-lock-reference-face) ; Rd-bold-face
   (cons
    (concat "\\\\\\("
            (mapconcat 'identity Rd-keywords "\\|")
            "\\>\\)")
    'font-lock-keyword-face)
   '("^#\\(ifn?def\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-builtin-face)
     (2 font-lock-variable-name-face nil t))
   '("^#\\(endif\\)" 1 font-lock-builtin-face))
  "Additional Rd expressions to highlight.")

(defvar Rd-indent-level 2
  "*Indentation of Rd code with respect to containing blocks.")

(defvar Rd-mode-map nil
  "Keymap used in Rd mode.")
(if Rd-mode-map
    ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\t" 'indent-according-to-mode)
    (define-key map "\C-j" 'reindent-then-newline-and-indent)
    (define-key map "\C-m" 'reindent-then-newline-and-indent)
    (define-key map "\C-c\C-p" 'Rd-preview-help)
    (define-key map "\C-c\C-j" 'Rd-mode-insert-item)
    (define-key map "\C-c\C-e" 'Rd-mode-insert-skeleton)
    (define-key map "\C-c\C-f" 'Rd-font)
    ;;  ^C^F ^E : \emph{ . }
    ;;  ^C^F ^C : \code{ . }
    ;;  ^C^F ^L : \link{ . }
    ;;  ^C^F  L : \code{\link{ . }}  etc
    (define-key map "\C-c\C-s" 'Rd-mode-insert-section)
    (define-key map "\C-c\C-n" 'ess-eval-line-and-step)
    (define-key map "\C-c\C-r" 'ess-eval-region)
    (define-key map "\C-c\C-c" 'ess-eval-function-or-paragraph-and-step)
    (define-key map "\C-c\C-v" 'ess-display-help-on-object)
    (define-key map "\C-c\C-w" 'ess-switch-process); is on C-c C-s in ess-mode..
    (define-key map "\C-c\C-y" 'ess-switch-to-ESS)
    (define-key map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
    (setq Rd-mode-map map)))

(defvar Rd-mode-menu
  (list "Rd"
        ["Markup [word]"                Rd-font t]
        ["Insert Item"                  Rd-mode-insert-item t]
        ["Insert Section"               Rd-mode-insert-section t]
        ["Insert Skeleton"              Rd-mode-insert-skeleton t]
        "-"
        ["Preview"                      Rd-preview-help t]
        "-"
        ["Eval Line"                    ess-eval-line-and-step t]
        ["Eval Region"                  ess-eval-region t]
        ["Switch to ESS Process"        ess-switch-to-ESS t]
        ["Switch the ESS Process"       ess-switch-process t]
        ["Switch to end{ESS Pr}"        ess-switch-to-end-of-ESS t]
        "-"
        ["Toggle Abbrev Mode"           abbrev-mode t]
        ["Toggle Auto-Fill Mode"        auto-fill-mode t]
        "-"
        ["Submit Bug Report"            Rd-submit-bug-report t]
        "-"
        ["Describe Rd Mode"             Rd-describe-major-mode t])
  "Menu used in Rd mode.")

(defvar Rd-mode-hook nil
  "*Hook to be run when Rd mode is entered.")

(defvar Rd-to-help-command "R CMD Rd2txt"
  "*Shell command for converting R documentation source to help text.")


(defvar Rd-font-list
  '((?\C-b "\\bold{"    "}")
    (?\C-c "\\code{"    "}")
    (?\C-e "\\emph{"    "}")
    (?\C-l "\\link{"    "}")
    (?l "\\code{\\link{" "}}")
    (?\C-m "\\email{"   "}")
    (?\C-q "\\eqn{"     "}")
    (?\C-u "\\url{"     "}")
    )
  "List of ``fonts'' used by Rd-font.

Each entry is a list.
The first element is the key to activate the font.
The second element is the string to insert before point, and the third
element is the string to insert after point."
  )


;;;###autoload
(defun Rd-mode ()
  "Major mode for editing R documentation source files.

This mode makes it easier to write R documentation by helping with
indentation, doing some of the typing for you (with Abbrev mode) and by
showing keywords, strings, etc. in different faces (with Font Lock mode
on terminals that support it).

Type \\[list-abbrevs] to display the built-in abbrevs for Rd keywords.

Keybindings
===========

\\{Rd-mode-map}

Variables you can use to customize Rd mode
==========================================

`Rd-indent-level'
  Indentation of Rd code with respect to containing blocks.
  Default is 2.

Turning on Rd mode runs the hook `Rd-mode-hook'.

To automatically turn on the abbrev(iate) features, add the
following lines to your `.emacs' file:

  (add-hook 'Rd-mode-hook
            (lambda ()
              (abbrev-mode 1)))
"

  (interactive)
  (text-mode)
  (kill-all-local-variables)
  (use-local-map Rd-mode-map)
  (setq mode-name "Rd")
  (setq major-mode 'Rd-mode)
  (setq local-abbrev-table Rd-mode-abbrev-table)
  (set-syntax-table Rd-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) 'Rd-mode-indent-line)
  (set (make-local-variable 'fill-column) 72)
  (set (make-local-variable 'comment-start-skip) "\\s<+\\s-*")
  (set (make-local-variable 'comment-start) "% ")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'font-lock-defaults)
       '(Rd-font-lock-keywords nil nil))
  ;; (set (make-local-variable 'parse-sexp-ignore-comments) t)

  (require 'easymenu)
  (easy-menu-define Rd-mode-menu-map Rd-mode-map
    "Menu keymap for Rd mode." Rd-mode-menu)
  (easy-menu-add Rd-mode-menu-map Rd-mode-map)

  (turn-on-auto-fill)
  (message "Rd mode version %s" essddr-version)
  (setq ess-language "S" ess-dialect  "R"); (buffer local)
  (run-hooks 'Rd-mode-hook))

;; FIXME: The following should be moved to ess-utils.el, no? (MM thinks)
(defun ess-point (position)
  "Returns the value of point at certain positions."
  (save-excursion
    (cond
     ((eq position 'bol)  (beginning-of-line))
     ((eq position 'eol)  (end-of-line))
     ((eq position 'boi)  (back-to-indentation))
     ((eq position 'bonl) (forward-line 1))
     ((eq position 'bopl) (forward-line -1))
     (t (error "unknown buffer position requested: %s" position)))
    (point)))

(defun Rd-describe-major-mode ()
  "Describe the current major mode."
  (interactive)
  (describe-function major-mode))

(defun Rd-mode-in-verbatim-p ()
  (let ((pos (point)))
    (save-excursion
      (if (and (re-search-backward
                "\\\\\\(usage\\|examples\\|synopsis\\)" nil t)
               (re-search-forward "\\s(" nil t))
          (condition-case ()
              (progn
                (up-list 1)
                (< pos (point)))
            (error t))
        nil))))

(defun Rd-mode-in-preprocessor-line-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*#\\(ifdef\\|endif\\)")))

(defun Rd-mode-calculate-indent ()
  "Return appropriate indentation for current line in Rd mode."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((Rd-mode-in-verbatim-p)
      ;; Don't do anything in verbatims
      nil)
     ((Rd-mode-in-preprocessor-line-p)
      ;; Indent to 0
      0)
     (t
      (let ((p (progn
                 (re-search-forward "[ \t]*\\s)*" (ess-point 'eol) t)
                 (point))))
        (if (or (< (forward-line -1) 0)
                (Rd-mode-in-verbatim-p))
            0
          (set-syntax-table Rd-mode-parse-syntax-table)
          (while (and (or (looking-at "[ \t]*$")
                          (Rd-mode-in-preprocessor-line-p))
                      (not (bobp)))
            (forward-line -1))
          (re-search-forward "[ \t]*\\s)*" (ess-point 'eol) t)
          (prog1
              (+ (current-indentation)
                 (* (car (parse-partial-sexp (point) p))
                    Rd-indent-level))
            (set-syntax-table Rd-mode-syntax-table))))))))

(defun Rd-mode-indent-line ()
  "Indent current line as Rd source."
  (interactive)
  (let ((ic (Rd-mode-calculate-indent))
        (rp (- (current-column) (current-indentation))))
    (if ic                              ; Not inside a verbatim
        (if (< ic 0)
            (error "Unmatched parenthesis")
          (indent-line-to ic)
          (if (> rp 0)
              (move-to-column (+ ic rp)))))))

(defun Rd-mode-insert-item ()
  (interactive)
  (reindent-then-newline-and-indent)
  (insert "\\item{")
  )

(defun Rd-mode-insert-section ()
  (interactive)
  (let ((s (ess-completing-read
            "Insert section: "
            (mapcar (lambda (x) (cons x x)) Rd-section-names)
            nil t)))
    (if (string= s "")
        (progn (insert "\\section{}{") (backward-char 2))
      (insert (format "\\%s{" s)))))

(defun Rd-mode-insert-skeleton ()
  (interactive)
  ;; Hmm: in theory this should be kept in sync with prompt()
  ;; ---  maybe using prompt() [or promptClass()...] would be better anyway?!
  (insert "\\name{}\n")
  (insert "\\alias{}\n")
  (insert "\\title{}\n")
  (insert "\\description{\n}\n")
  (insert "\\usage{\n}\n")
  (insert "\\arguments{\n}\n")
  (insert "\\value{\n}\n")
  (insert "\\details{\n}\n")
  (insert "\\references{\n}\n")
  (insert "\\seealso{\n}\n")
  (insert "\\examples{\n}\n")
  (insert "\\author{}\n")
  (insert "\\keyword{}\n"))

;; This is an `easy' version of (defun TeX-font ..) in AUCtex's  tex.el ;
;;  see TeX-font-list and also LaTeX-font-list in latex.el

(defun Rd-font (what)
  "Insert template for font command.
 WHAT determines the font to use, as specified by `Rd-font-list'."
  (interactive "c")
  ;;TeX had : (Rd-update-style)
  (let* ((entry (assoc what Rd-font-list))
         (before (nth 1 entry))
         (after (nth 2 entry)))
    (cond ((null entry) ;; help on possibilities :
           (let ((help
                  (concat
                   "Rd Markup (available from C-c C-f):\n\n\t"
                   "KEY          Rd-Markup\n\n"
                   (mapconcat
                    (lambda (entry)
                      ;; A textual description of an ENTRY in TeX-font-list.
                      (concat (format "%11s  "
                                      (key-description
                                       (char-to-string (nth 0 entry))))
                              (format "%14s %-3s"
                                      (nth 1 entry) (nth 2 entry))))
                    Rd-font-list "\n"))))
             (with-output-to-temp-buffer "*Help*"
               (set-buffer "*Help*")
               (insert help))))

          ((Rd-active-mark)
           (save-excursion
             (cond ((> (mark) (point))
                    (insert before)
                    (goto-char (mark))
                    (insert after))
                   (t
                    (insert after)
                    (goto-char (mark))
                    (insert before)))))
          (t
           (insert before)
           (save-excursion
             (insert after))))))

(defun Rd-preview-help ()
  "Preview the current buffer contents using `Rd-to-help-command'.
If the current buffer is not associated with a file, create a
temporary one in `temporary-file-directory'.
"
  (interactive)
  (require 'ess-help)
  (let ((file  buffer-file-name)
        (pbuf (get-buffer-create "R Help Preview"))
        del-p shcmd)
    (unless file
      (setq file  (make-temp-file "RD_" nil ".Rd"))
      (write-region (point-min) (point-max) file)
      (setq del-p t))
    (setq shcmd (format "%s '%s'" Rd-to-help-command file))
    (set-buffer pbuf)
    (erase-buffer)
    (ess-write-to-dribble-buffer
     (format "Rd-preview-help: (shell-command |%s| t)" shcmd))
    (shell-command shcmd t)
    (setq ess-help-sec-regex ess-help-R-sec-regex
          ess-help-sec-keys-alist ess-help-R-sec-keys-alist)
    (ess-nuke-help-bs)
    (ess-help-mode)
    (when del-p (delete-file file))
    (unless (get-buffer-window pbuf 'visible)
      (display-buffer pbuf t))))

;; Bug reporting
(defun Rd-submit-bug-report ()
  "Submit a bug report on Rd mode via mail."
  (interactive)
  (require 'reporter)
  (and
   (y-or-n-p "Do you want to submit a bug report? ")
   (reporter-submit-bug-report
    essddr-maintainer-address
    (concat "Emacs version " emacs-version)
    (list
     'essddr-version
     'Rd-indent-level))))

;; Provide ourself
(provide 'essddr)

;; ess-rd.el ends here
