;;; essd-sas.el --- SAS customization

;; Copyright (C) 1997 Richard M. Heiberger and A. J. Rossini

;; Author: Richard M. Heiberger <rmh@astro.ocis.temple.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 20 Aug 1997
;; Modified: $Date: 1997/09/08 19:39:18 $
;; Version: $Revision: 1.8 $
;; RCS: $Id: essl-sas.el,v 1.8 1997/09/08 19:39:18 rossini Exp $
;;
;; Keywords: start up, configuration.

;; This file is part of ESS.

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
;;; This is based upon Version 1.4 of SAS mode:







;;; autoloads originally in ess-site.  

(autoload 'SAS-transcript-mode
  "ess-trns" "ESS source eval mode" t)




;;;    sas-mode:  indent, run etc, SAS programs.
;;;    Copyright (C) 1994 Tom Cook
;;;  Author:   Tom Cook
;;;            Dept. of Biostatistics
;;;            University of Wisconsin - Madison
;;;            Madison, WI 53706
;;;            cook@biostat.wisc.edu
;;;
;;;  Acknowledgements: 
;;;  Menu code for XEmacs/Lucid emacs and startup mods
;;;  contributed by arossini@biostats.hmc.psu.edu
;;;
;;; Last change: 2/1/95


(defvar sas-indent-width 4
  "*Amount to indent sas statements")
(defvar sas-indent-ignore-comment "*"
  "*Comments with start with this string are ignored in indentation.")
(defvar sas-require-confirmation t
  "*Require confirmation when revisiting sas-output which has changed on disk.")
;; added sas-program 4/29/94.  user can specify a different version of sas.
(defvar sas-program "sas" "*Name of program which runs sas.")
(defvar sas-pre-run-hook nil
  "Hook to execute prior to running SAS vis submit-sas.")
(defvar sas-options-string ""
  "*Options to be passed to sas as if typed on the command line.")
(defvar sas-notify t "*Beep and display message when job is done?")  ;; added 4/7/94
(defvar sas-error-notify t
  "*If sas-notify is t, then indicate errors in log file upon completion")
;; added 5/2/94
(defvar sas-get-options nil "Options to be passed to SAS in sas-get-dataset")
(defvar sas-get-options-history nil "History list of Options passed to SAS in sas-get-dataset")
(defvar sas-page-number-max-line 3
  "*Number of lines from the page break in which to search for the page number")
(defvar sas-notify-popup nil
  "*If t (and sas-notify is also t), causes emacs to create a
popup window when the SAS job is finished.")
(defvar sas-tmp-libname "_tmp_" "*Libname to use for sas-get-dataset.")

(defvar sas-file-name nil)
(defvar sas-buffer-name nil)
(defvar sas-file-root nil)
(defvar sas-submitable nil)
(defvar sas-dataset nil)


(defvar SAS-syntax-table nil "Syntax table for SAS code.")

(if SAS-syntax-table
    nil
  (setq SAS-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" SAS-syntax-table)
  (modify-syntax-entry ?+  "."  SAS-syntax-table)
  (modify-syntax-entry ?-  "."  SAS-syntax-table)
  (modify-syntax-entry ?=  "."  SAS-syntax-table)
  (modify-syntax-entry ?%  "."  SAS-syntax-table)
  (modify-syntax-entry ?<  "."  SAS-syntax-table)
  (modify-syntax-entry ?>  "."  SAS-syntax-table)
  (modify-syntax-entry ?&  "."  SAS-syntax-table)
  (modify-syntax-entry ?|  "."  SAS-syntax-table)
  (modify-syntax-entry ?\' "\"" SAS-syntax-table)
  (modify-syntax-entry ?*  "<"  SAS-syntax-table) ; open comment
  (modify-syntax-entry ?\; ">"  SAS-syntax-table) ; close comment
  (modify-syntax-entry ?_  "."  SAS-syntax-table)  
  (modify-syntax-entry ?*  "."  SAS-syntax-table)
  (modify-syntax-entry ?<  "."  SAS-syntax-table)
  (modify-syntax-entry ?>  "."  SAS-syntax-table)
  (modify-syntax-entry ?/  "."  SAS-syntax-table))

(defvar SAS-mode-font-lock-keywords
  '(("/\\*.\\*/"       . font-lock-comment-face)
    ("^ *\\*[^/]*.;"   . font-lock-comment-face)
    ("; *\\*[^/]*.;"   . font-lock-comment-face)
    ("%include [^;]*;" . font-lock-preprocessor-face)
    ("&+[a-z0-9_]*\\>" . font-lock-keyword-face)
    ("^[ \t]*%let[ \t]+\\([a-z0-9_]*\\)" . font-lock-keyword-face)
    ("\\<\\(array\\|length\\|var\\|class\\)\\>" . font-lock-keyword-face)
    ("^[ \t]*\\(proc\\|data\\|%macro\\|run\\|%mend\\|endsas\\)[ \t;]" . font-lock-function-name-face)
    ("\\<\\(retain\\|format\\|input\\|infile\\|by\\|set\\|merge\\|label\\|options\\|where\\|%?if\\|%?then\\|%?else\\|%?while\\|%?do\\|%?until\\|%?end\\|%let\\|%str\\)\\>" 
     . font-lock-keyword-face)
    ("^[ \t]*\\(infile\\|proc\\|%macro\\|data\\)\\>[ \t]+\\([a-z0-9_.]*\\)")
    ("\\b\\(data\\|out\\)\\>[ \t]*=[ \t]*\\([a-z0-9_.]*\\)")
    ("^[ \t]*\\(set\\|merge\\)[ \t]+\\([^();]*\\)")
    ("^[ \t]*\\(set\\|merge\\)[ \t]+[a-z0-9_.]*[ \t]*([^)]*)[ \t]*\\([^();]*\\)")
    ("^[ \t]*\\(set\\|merge\\)[ \t]+[a-z0-9_.]*[ \t]*([^)]*)[ \t]*[a-z0-9_.]*[ \t]*([^)]*)[ \t]*\\([^();]*\\)")
    ;;("^[ \t]*\\(set\\|merge\\)\\>\\([^;]*\\);")
    ;;("\\b\\(set\\|merge\\)\\>[^()]*\\((.*)\\)")
    ("%[a-z0-9_]*\\>" . font-lock-preprocessor-face))
  "Font Lock regexs for SAS.")

(defvar SAS-editing-alist
  '((sentence-end                 . ";[\t\n */]*")
    (paragraph-start              . "^[ \t]*$")
    (paragraph-separate           . "^[ \t]*$")
    (paragraph-ignore-fill-prefix . t)
    (fill-paragraph-function      . 'lisp-fill-paragraph)
    (adaptive-fill-mode           . nil)
    (indent-line-function         . 'sas-indent-line)
    (indent-region-function       . 'lisp-indent-region)
    (require-final-newline        . t)
    (comment-start                . "\\*\\|/\\*")
    (comment-start-skip           . "\\*+")
    (comment-end                  . ";\\|\\*/")
    (comment-column               . 40)
    (comment-indent-function      . 'lisp-comment-indent)
    (parse-sexp-ignore-comments   . t)
    (ess-set-style                . ess-default-style)
    (ess-local-process-name       . nil)
    (ess-keep-dump-files          . nil)
    (ess-mode-syntax-table        . SAS-syntax-table)
    (font-lock-defaults           . '(SAS-mode-font-lock-keywords)))
  "General options for editing SAS source files.")

(defun beginning-of-sas-statement (arg &optional comment-start)
  "Move point to beginning of current sas statement."
  (interactive "P")
  (let ((pos (point)))
    (if (search-forward ";" nil 1) (forward-char -1))
    (re-search-backward ";[ \n*/]*$" (point-min) 1 arg)
    (skip-chars-forward "\ \t\n\f;")
    (if comment-start nil
          (if (looking-at "\\*/")
              (progn (forward-char 2)
                     (skip-chars-forward "\ \t\n\f;")))
          (while (looking-at "/\\*")
            (if (not (search-forward "*/" pos t 1)) ;;(;; (point-max) 1 1)
                (forward-char 2))
            (skip-chars-forward "\ \t\n\f")
            ))
      ))





(defun sas-indent-line ()
  "Indent function for SAS mode."
  (interactive)
  (let (indent prev-end 
               (pos (- (point-max) (point)))
               (case-fold-search t)
               (cur-ind (current-indentation))
               (comment-col (sas-comment-start-col)) ;; 2/1/95 TDC
               )
    (save-excursion
      (cond ((progn 
               (back-to-indentation)
               (or (bobp)
                   (looking-at
                    "data[ ;]\\|proc[ ;]\\|run[ ;]\\|endsas[ ;]\\|g?options[ ;]\\|%macro[ ;]\\|%mend[ ;]")))
;;;  Case where current statement is DATA, PROC, etc...
             (setq prev-end (point))
             (goto-char (point-min))
;;;  Added 6/27/94
;;;  May get fooled if %MACRO, %DO, etc embedded in comments
             (setq indent (+ (* (- (sas-how-many "^[ \t]*%macro\\|[ \t]+%do"
                                              prev-end)
                                (sas-how-many "^[ \t]*%mend\\|%end" prev-end))
                             sas-indent-width) comment-col)))  ;; 2/1/95 TDC
;;;  Case where current line begins with sas-indent-ignore-comment
            ((progn               ;; added 6/27/94  to leave "* ;" comments alone.
               (back-to-indentation)
               (and (not (looking-at "*/"))
                    (looking-at (concat sas-indent-ignore-comment "\\|/\\*"))))
             (setq indent (current-indentation)))
;;;  Case where current statement not DATA, PROC etc...
            (t (beginning-of-line 1)
               (skip-chars-backward " \n\f\t")
               (if (bobp) nil
                 (backward-char 1))
               (cond
                ((looking-at ";")       ;  modified 1/31/95
                 (setq indent (sas-next-statement-indentation)))
                ((save-excursion;; added 4/28/94 to properly check
                   (if (bobp) () (backward-char 1));; for end of comment
                   (setq prev-end (point))
                   (looking-at "*/"));;  improved 1/31/95
                 (save-excursion                            
                   (search-backward "*/" (point-min) 1 1)  ;; comment start is first /*
                   (search-forward "/*" prev-end 1 1)      ;; after previous */ 
                   (backward-char 2)                       ;; 2/1/95 TDC
                   (skip-chars-backward " \n\f\t")
                   (setq indent
                         (if (bobp) 0
                           (if (looking-at ";") (sas-next-statement-indentation)
                             (+ (current-indentation) sas-indent-width))))))
                  
                  ((save-excursion;; added 6/27/94 to leave "* ;" comments alone
                     (progn
                       (beginning-of-sas-statement 1 t)
                       (and (not (looking-at "*/"))
                            (looking-at sas-indent-ignore-comment))))
                   (setq indent cur-ind))
                  ((progn (beginning-of-sas-statement 1) (bobp));; added 4/13/94
                   (setq indent sas-indent-width));; so the first line works
                  (t
                   (if (progn
                         (save-excursion
                           (beginning-of-line 1)
                           (skip-chars-backward " \n\f\t")
                           (if (bobp) nil (backward-char 1))
                           (or (looking-at ";")
                               (bobp) (backward-char 1) (looking-at "\\*/"))))
                       (setq indent (+ (current-indentation) sas-indent-width))
                     (setq indent (current-indentation))))))))
    (save-excursion 
      (let (beg end)
        (back-to-indentation)
        (setq end (point))
        (beginning-of-line 1)
        (setq beg (point))
        (delete-region beg end)
        (indent-to indent)))
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

(defun indent-sas-statement (arg)
  "Indent all continuation lines sas-indent-width spaces from first
line of statement."
  (interactive "p")
  (let (end)
  (save-excursion
    (if (> arg 0)
        (while (and (> arg 0) (search-forward ";" (point-max) 1 1))
          (setq end (point))
          (if (bobp) nil (backward-char 1))
          (beginning-of-sas-statement 1)
          (forward-char 1)
          (indent-region (point)
			 end
			 (+ (current-column) (1- sas-indent-width)))
          (search-forward ";" (point-max) 1 1)
          (setq arg (1- arg)))))))

;; added 9/31/94 
(defun sas-next-statement-indentation ()
  "Returns the correct indentation of the next sas statement.
The current version assumes that point is at the end of the statement.
This will (hopefully) be fixed in later versions."
  (if (bobp) 0
    (save-excursion
      (let ((prev-end (point)))
        (beginning-of-sas-statement 1)
        (while (and (not (bobp))
                    (not (looking-at "*/"))
                    (looking-at sas-indent-ignore-comment))
          (skip-chars-backward " \n\f\t")
          (if (bobp) nil
            (backward-char 1))
          (setq prev-end (point))
          (beginning-of-sas-statement 1 t))
        (if (or
             (looking-at
              "data[ \n\t;]\\|proc[ \n\t]\\|%?do[ \n\t;]\\|%macro[ \n\t]\\|/\\*")
             (save-excursion
               (re-search-forward
                "\\b%?then\\>[ \n\t]*\\b%?do\\>\\|\\b%?else\\>[ \n\t]*\\b%?do\\>"
                prev-end 1 1)))  ;;; fixed 1/30/95 to avoid being fooled by
                                            ;;;variable names starting with "do"
            (+ (current-indentation) sas-indent-width)
          (if (looking-at "%?end[ ;\n]\\|%mend[ ;\n]\\|\\*/")
              (max (- (current-indentation) sas-indent-width) 0)
            (current-indentation)))
        ))))

;; added 2/1/95
(defun sas-comment-start-col ()
  "If the current line is inside a /* */ comment, returns column in which the
opening /* appears.  returns 0 otherwise."
  (let ((pos (point)))
    (save-excursion
      (if (and (search-backward "*/" (point-min) 1 1)
               (search-forward "/*" pos 1 1))
          (current-indentation)
        0))))


;;  Created 6/27/94 to verify that RUN; statements match PROC and DATA
;;  statements.  Useful since indentation my be goofy w/o "RUN;"
(defun sas-check-run-statements ()
  "Check to see that \"run\" statements are matched with proc, data statements."
  (interactive)
  (let (pos (ok t) (eob-ok t))
    (save-excursion
      (beginning-of-line)
      (while ok
        (if (re-search-forward "\\(^[ \t]*run[ ;]\\)\\|\\(^[ \t]*proc \\|^[ \t]*data[ ;]\\)" nil 1)
              (if (match-beginning 2)
                  (if (re-search-forward "\\(^[ \t]*run[ ;]\\)\\|\\(^[ \t]*proc \\|^[ \t]*data[ ;]\\)" nil t)
                      (progn (setq pos (point))
                             (setq ok (match-beginning 1)))
                    (setq eob-ok nil pos (point-max))))
                (setq ok nil)))
    (setq ok (eobp)))
  (if (and ok eob-ok) (message "Run statements match")
    (goto-char pos)
    (beep)
    (message "Missing Run Statement."))))


(defun sas-fix-life-tables (start end)
  "Remove censored and duplicate observations from life tables generated by
Proc Lifetest.  Operates on current region.  A major space saver if there is
heavy censoring."
  (interactive "r")
  (save-excursion 
    (shell-command-on-region
     start end
     "sed \"\\?          *\\.          *\\.          *\\.    ?d\"" t)))
    ;;(vip-goto-line 1)
    ;;(setq ex-g-flag nil
          ;;ex-g-variant nil)
    ;;(vip-ex "1,$g/           \\.           \\.           \\.    /d")))

(defun sas-fix-page-numbers (offset &optional page-num)
  "Fix number of current page in sas output files after editing.  Add
OFFSET to actual page number."
  (interactive "P")
  (if (not offset) (setq offset 0))
  (if (not page-num) (setq page-num (sas-page-number)))
  (save-excursion
    (if (/= (preceding-char) ?\C-l) (backward-page 1))
    (let (end len mstart mend)
      (save-excursion
        (forward-line sas-page-number-max-line)
        (setq end (point)))
      (if (re-search-forward
           "\\(^[0-9]+[ ]\\)\\|\\([ ][0-9]+$\\)"
           end t)
          (progn (setq len (- (match-end 0) (match-beginning 0))
                       mstart (match-beginning 0)
                       mend (match-end 0))
                 (delete-region mstart mend)
                 (if (eolp)
                 (insert (format
                          (concat "%" len "d") (+ page-num offset)))
                 (insert (substring
                          (concat (+ (sas-page-number) offset) "      ")
                          0 len))))))))

(defun sas-page-fix (start)
  "Fix page numbers in sas output from point to end of file.  If START is given this will be the number for the current page."
  (interactive "P")
  (let (offset (pnum (sas-page-number)))
  (if (not start) (setq offset 0)
    (setq offset (- start pnum)))
  (while (not (eobp))
    (sas-fix-page-numbers offset pnum)
    (setq pnum (1+ pnum))
    (forward-page 1))))

(defun fix-page-breaks ()
  "Fix page breaks in SAS 6 print files."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "\f") (delete-char 1))
    (replace-regexp "^\\(.+\\)\f" "\\1\n\f\n")
    (goto-char (point-min))
    (replace-regexp "^\f\\(.+\\)" "\f\n\\1")
    (goto-char (point-min))
    (replace-regexp "
$" "")
    (goto-char (point-min))
    (replace-regexp "
\\([^
\\$]+\\)" "\n\\1")
    (goto-char (point-max))
    (if (not (bobp))
        (progn (backward-char 1)
               (if (not (looking-at "\n"))
                   (progn (forward-char 1) (open-line 1)))))
    ;;;(basic-save-buffer)
    )
  )

(defun sas-page-number ()
  ;; like what-page except it returns an integer page number
  "Return page number of point in current buffer."
  (let ((opoint (point))) (save-excursion
       (goto-char (point-min))
       (1+ (sas-how-many page-delimiter opoint)))))

(defun sas-how-many (regexp &optional end)
  ;; a copy of `how-many' which returns an integer
  ;; rather than a message
  "Return number of matches for REGEXP following point."
  (let ((count 0) opoint)
    (save-excursion
     (while (and (not (eobp))
		 (progn (setq opoint (point))
			(re-search-forward regexp end t)))
       (if (= opoint (point))
	   (forward-char 1)
	 (setq count (1+ count))))
     count)))

(defun beginning-of-sas-proc ()
  "Move point to beginning of sas proc, macro or data step."
  (interactive)
  (let ((case-fold-search t))
    (forward-char -1)
    (while (not (or (looking-at "data\\|proc\\|%macro")
                    (bobp)))
      (re-search-backward "proc\\|data\\|%macro" (point-min) 1)
      (beginning-of-sas-statement 1))))

(defun next-sas-proc (arg)
  "Move point to beginning of next sas proc."
  (interactive "P")
  (let ((case-fold-search t))
    (forward-char 1)
    (if (re-search-forward
         "^[ \t]*\\(data[ ;]\\|proc[ ;]\\|endsas[ ;]\\|g?options[ ;]\\|%macro[ ;]\\)"
         nil t arg)
        (beginning-of-sas-statement 1)
      (forward-char -1))))

(defun set-sas-file-name ()
  "Stores the name of the current sas file"
  (let ((name (buffer-file-name)))
    (cond ((not name))
          ((string-match (substring name -4 nil) "\\.sas\\|\\.lst\\|\\.log")
	   
           (setq sas-file-name (substring name 0 (- (length name) 4)))
           (setq sas-buffer-name (buffer-name))
	   (setq sas-file-root (substring sas-buffer-name 0 (- (length sas-buffer-name) 4)))
	   
           )
          (t (message "This file does not have a standard suffix")))))

;;  created 6/27/94
(defun sas-set-alternate-file-name (name)
  "Stores the NAME of an alternate sas file.  When this file is submitted with
submit-sas, the  alternate file will be submitted instead.  sas-submitable
is automatically sets to t."
    (interactive "f")
    (cond ((string-match (substring name -4 nil) "\\.sas\\|\\.lst\\|\\.log")
           (setq sas-file-name (substring name 0 (- (length name) 4)))
           (setq sas-submitable t))
          (t (message "This file does not have a standard suffix"))))

(defun switch-to-sas-source ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file "sas"))

(defun switch-to-sas-lst ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file "lst"))

(defun switch-to-sas-log ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file "log"))

(defun switch-to-sas-source-other-window ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file-other-window "sas"))

(defun switch-to-sas-lst-other-window ()
  "Switches to sas source file associated with the current file"
  (interactive)
    (switch-to-sas-file-other-window "lst"))

(defun switch-to-sas-log-other-window ()
  "Switches to sas source file associated with the current file"
  (interactive)
     (switch-to-sas-file-other-window "log"))

;;(defun switch-to-sas-file (suff &optional revert silent)
;;  "Switches to sas \"SUFF\" file associated with the current file"
;;  (let* ((sfile sas-file-name)
;;	 (buf (get-file-buffer (concat sfile "." suff)))
;;	 (sas-require-confirmation
;;           (and sas-require-confirmation (not revert))))
;;     (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
;;         (find-file (concat sfile "." suff))
;;       (progn (switch-to-buffer buf)
;;              (if (not (verify-visited-file-modtime (current-buffer)))
;;                  (progn (revert-buffer t t)
;;                         (if (not silent)
;;                             (message "File has changed on disk.  Buffer automatically updated."))))))
;;     (setq sas-file-name sfile))
;;   (if (string-equal suff "sas")
;;       (if (not (string-equal major-mode "sas-mode"))
;;           (sas-mode))
;;     (if (not (string-equal major-mode "sasl-mode"))
;;         (sasl-mode))))
;; 
;;(defun switch-to-sas-file-other-window (suff)
;;  "Switches to sas \"SUFF\" file associated with the current file"
;;  (let* ((sfile sas-file-name)
;;	 (buf (get-file-buffer (concat sfile "." suff))))
;;    (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
;;	(find-file-other-window (concat sfile "." suff))
;;      (progn (switch-to-buffer-other-window buf)
;;	     (if (not (verify-visited-file-modtime (current-buffer)))
;;		 (progn (revert-buffer t t)
;;			(message "File has changed on disk.  Buffer automatically updated.")))))
;;    (setq sas-file-name sfile))
;;  (if (string-equal suff "sas")
;;      (if (not (string-equal major-mode "sas-mode"))
;;	  ;;(sas-mode)
;;	  )
;;    (if (not (string-equal major-mode "sasl-mode"))
;;	;;(sasl-mode)
;;	)))

(defun switch-to-sas-file (suff)
  "Switches to sas \"SUFF\" file associated with the current file"
  (switch-to-buffer (set-sas-file-buffer suff)))

(defun switch-to-sas-file-other-window (suff)
  "Switches to sas \"SUFF\" file associated with the current file"
  (switch-to-buffer-other-window (set-sas-file-buffer suff)))

;;  The following was created 6/7/94 to handle buffers without messing up
;;  windows.

(defun set-sas-file-buffer (suff &optional revert silent)
  "Sets current buffer to sas \"SUFF\" file associated with the current file"
  (let* ((sfile sas-file-name)
         (buf (get-file-buffer (concat sfile "." suff)))
         (sas-require-confirmation
          (and sas-require-confirmation (not revert))))
    (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
        (set-buffer (find-file-noselect (concat sfile "." suff)))
      (progn (set-buffer buf)
             (if (not (verify-visited-file-modtime (current-buffer)))
                 (progn (revert-buffer t t)
                        (if (not silent)
                            (message "File has changed on disk.  Buffer automatically updated."))))))
    (setq sas-file-name sfile))
  (if (string-equal suff "sas")
      (if (not (string-equal major-mode "sas-mode"))
          ;;(sas-mode)
	  )
    (if (not (string-equal major-mode "sasl-mode"))
	;;(sasl-mode)
	))
  (current-buffer))

(defun switch-to-sas-process-buffer ()
  "Switch to sas-process-buffer"
  (interactive)
  (let (buf proc-name)
    (setq proc-name (concat "SAS" sas-file-name)
          buf (concat "*" proc-name "*"))
    (switch-to-buffer-other-window buf)))

(defun submit-sas ()
  ;; 6/17/94  added sas-submitable local variable.
  "Submit SAS file as shell command."
  (interactive)
  (if ;; can file be run, or is it only for inclusion?
      (or sas-submitable
          (progn
            (beep)
            (y-or-n-p
             (format 
	      "Submission is disabled for this file.  Submit it anyway? "))
	    ))
      (progn 
	;; if buffer name has changed, tell user
        (if (or
             (string-equal sas-buffer-name (buffer-name)) 
             (not
              (y-or-n-p
               (format 
		"The name of this buffer has changed.  Submit the new file? "))
	      ))
            (setq sas-buffer-name (buffer-name))
          (set-sas-file-name))
        (let ((sas-file sas-file-name)
	      (sas-root sas-file-root)
	      ;;(sas-buf  sas-buffer-name)
	      proc-name
	      buf)

	  ;; Save buffer to SAS the right file :-).
          (if (buffer-modified-p)
              (if (y-or-n-p (format "Buffer %s is modified. Save it? "
                                    (buffer-name)))
                  (save-buffer)))
          (setq proc-name (concat "SAS" sas-file)
		buf (concat "*" proc-name "*"))
          (if (get-buffer buf)
	      (save-window-excursion (switch-to-buffer buf)
                                     (erase-buffer) 
				     (setq default-directory 
					   (file-name-directory sas-file))))

          (run-hooks 'sas-pre-run-hook)  ;; added 8/24/94
	  (message "----  Submitting SAS job   ----")
	  ;; (switch-to-buffer buf)
	  (make-comint proc-name 
		       sas-program     ;added sas-program 4/29/94
		       nil
		       sas-root)
	  (save-window-excursion
	    (switch-to-buffer buf)
	    (setq sas-file-name sas-file)
	    (bury-buffer buf))

          (message "----  SAS job submitted  ----   ")

          (if sas-notify;;  added 4/7/94
              (set-process-sentinel (get-process proc-name) 'sas-sentinel)
            (display-buffer buf t))))
    (message "----  File not submitted  ----")))

;; 5/2/94 Modified sas-sentinel to check for errors in log file upon
;; completion.
(defun sas-sentinel (proc arg);; created 4/7/94
  "Notify user that SAS run is done"
  (beep)
  ;;(if (string-equal arg "finished\n")
  (save-excursion
    (let (msg buf win (sbuf (concat "*" (process-name proc) "*")))
      (setq msg
            (format "SAS %s %s"
                    (substring arg 0 -1)
                    (if sas-error-notify 
                        ;;(save-window-excursion
                        (progn
                        (set-buffer sbuf)
                        (setq buf (set-sas-file-buffer "log" t t))
                        (goto-char (point-min))
                        (setq win (get-buffer-window buf))
                        (save-window-excursion
                          (if win
                              (progn 
                                (select-window win)
                                (if (re-search-forward "^ERROR" nil t)
                                    " (See .log file for errors)"
                                  ""))
                            (switch-to-buffer buf)
                            (if (re-search-forward "^ERROR" nil t)
                                " (See .log file for errors)"
                              ""))))
                    "")))
    (set-buffer sbuf)
    (goto-char (point-max))
    (insert msg)
    (bury-buffer (get-buffer sbuf))
    ;;(if (and sas-notify-popup window-system)
    ;;    (x-popup-dialog t
    ;;		(list "SAS Menu" (cons msg  nil) )))
    ;;(if (not (minibuffer-window-active-p)) (princ msg))
    (princ msg)
    )))



;; 5/2/94 Modified run-sas-on-region to separate log and output buffers.
;; 
;;(defun run-sas-on-region (start end append &optional buffer)
;;  "Submit region to SAS"
;;  (interactive "r\nP")
;;  (message "----  Running SAS  ----")
;;  (let ((sfile sas-file-name)
;;        (shell-file-name "/bin/sh")
;;        serror buff)
;;    (setq buffer (or buffer "*SAS output*"))
;;    (save-excursion
;;      (shell-command-on-region
;;       start end;; added sas-program 
;;       (concat sas-program " -nonews -stdio 2> /tmp/_temp_.log" nil))
;;      (get-buffer-create "*SAS Log*")
;;      (save-window-excursion
;;        (switch-to-buffer "*SAS Log*")
;;        (erase-buffer)
;;        (insert-file-contents "/tmp/_temp_.log")
;;        (delete-file "/tmp/_temp_.log")
;;        (setq serror (re-search-forward "^ERROR" nil t))
;;        (if serror () (bury-buffer)))
;;      (setq buff (get-buffer-create buffer))
;;      (save-window-excursion
;;        (switch-to-buffer buff)
;;        (setq sas-file-name sfile)
;;        (if append
;;            (progn
;;              (end-of-buffer)
;;              (insert "\f\n"))
;;          (erase-buffer))
;;        (if (get-buffer "*Shell Command Output*")
;;            (progn (insert-buffer "*Shell Command Output*")
;;                   (kill-buffer "*Shell Command Output*"))
;;          (insert "SAS completed with no output."))
;;        (if append () (sasl-mode))
;;        (message "----  SAS Complete ----")))
;;    (if (not serror)
;;        (switch-to-buffer-other-window  buff)
;;      (switch-to-buffer-other-window "*SAS Log*")
;;      (goto-char serror)
;;      (beep)
;;      (message "Error found in log file.")
;;      )))
  
(defun switch-to-dataset-log-buffer ()
  "Switch to log buffer for run-sas-on-region."
  (interactive)
  (switch-to-buffer-other-window "*SAS Log*"))

(defun switch-to-dataset-source-buffer ()
  "Switch to source buffer for run-sas-on-region."
  (interactive)
  (switch-to-buffer-other-window (format " *sas-tmp-%s*" sas-dataset)))

;;(defun sas-get-dataset (filename &optional arg opts-p append buffer vars)
;;  "Run proc contents and proc print on SAS dataset.  Automatically prompts 
;;for SAS options to use.  Default options are defined by the variable
;;`sas-get-options'.  Output may be updated from within output buffer with
;;C-cr if dataset changes.  Also, the source code which generates the output
;;may be edited with C-cs.  Typing C-cr within the output buffer reexecutes
;;the (modified) source code."
;;  (interactive "fName of SAS dataset (file name):")
;;  (let ((file (file-name-nondirectory filename))
;;        (dir (file-name-directory filename))
;;        (opts sas-get-options)
;;        (minibuffer-history sas-get-options-history)
;;        buf); fsize)
;;    (setq buffer (or buffer (concat "*" file "*")))
;;    (setq opts (if opts-p opts (read-string "SAS options: " opts)))
;;    (setq sas-get-options-history minibuffer-history)
;;    (cond ((string-match (substring file -6 nil) "\\.ssd01")
;;      (setq file (substring file 0 (- (length file) 6))))
;;    (t (error "This file is not a SAS dataset.")))
;;    (setq buf (format " *sas-tmp-%s*" file))
;;    (get-buffer-create buf)
;;    (save-window-excursion
;;      (switch-to-buffer buf)
;;      (erase-buffer)
;;      (setq default-directory dir)
;;      (if opts 
;;          (insert (format "options  %s ;\n" opts)))
;;      (insert (format "title \"Contents of SAS dataset `%s'\" ;\n" file))
;;      (insert (format "libname %s '%s' ;\n" sas-tmp-libname dir))
;;      (if (not (equal arg 1))
;;               (insert (format "proc contents data = %s.%s ;\n" sas-tmp-libname file)))
;;      (if (equal arg 2) ()
;;        (insert (format "proc print data = %s.%s ;\n" sas-tmp-libname file))
;;        (if vars (insert (format "  var %s ;\n" vars))))
;;      (run-sas-on-region (point-min) (point-max) append
;;                         buffer)
;;      (get-buffer buffer)
;;      (if append () (sasd-mode))  ;; added 5/5/94 
;;      (setq sas-dataset file))
;;    (if (get-buffer-window buffer t)
;;        (raise-frame (window-frame (get-buffer-window buffer t)))
;;    (display-buffer buffer (not append)))
;;    ))
    
;;(defun revert-sas-dataset ()
;;  "Revert current sas dataset from disk version"
;;  (interactive)
;;  (let* ((file sas-dataset)
;;        (buf (format " *sas-tmp-%s*" file))
;;        (pos (point)))
;;      (save-window-excursion
;;        (switch-to-buffer buf)
;;        (run-sas-on-region (point-min) (point-max) nil
;;                           (concat "*" file ".ssd01*"))
;;        )
;;      (goto-char pos)  ;; added 6/9/94
;;    (sasd-mode)  ;; added 5/5/94 
;;    (setq sas-dataset file)))

(defun sas-insert-local-variables ()  ;; created 6/17/94 
  "Add local variables code to end of sas source file."
  (interactive)
  (save-excursion
    (if (re-search-forward "* *Local Variables: *;" nil t)
        ()
      (goto-char (point-max))
      (insert "

**  Local Variables:  ;
**  End:  ;
page ;
"))))



(provide 'essl-sas)

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
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; essl-sas.el ends here
