;;; ess-compat.el --- simple determination of Emacs/XEmacs and version #.

;; Copyright (C) 2000--2005 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 07 June 2000
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

;; This file is part of ESS

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

;; This file contains functions for easily determining features of the
;; version of Emacs that we are using.   In particular, it look for
;; version number, customize support, as well as Emacs/XEmacs, for
;; flaggin support later on.

;;; Code:

;;; Define a function to make it easier to check which version we're
;;; running.
;; no longer in use; 2013-12-30:
(defun ess-running-emacs-version-or-newer (major minor)
  (or (> emacs-major-version major)
      (and (= emacs-major-version major)
           (>= emacs-minor-version minor))))

                                        ;(defvar ess-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(defvar ess-local-custom-available (featurep 'custom)
  "Value is nil if custom.el not available, t if available.
Only a concern with earlier versions of Emacs.")

;; FIXME:  When emacs is started from Cygwin shell in Windows,
;;         we have (equal window-system 'x) -and should use "--ess" in *d-r.el
(defvar ess-microsoft-p (or (eq system-type 'ms-dos)
			    (eq system-type 'windows-nt))
  "Value is t if the OS is one of Microsoft's, nil otherwise.")


;; These definitions are for Emacs versions < 20.4 or XEmacs
;; These are taken verbatim from the file emacs-20.6/lisp/w32-fns.el
;;
;; Note: 20.3 and 19.x NTemacs users are strongly encouraged to upgrade to
;; version 20.4 or higher.  NTemacs 20.2 is not supported by ESS.

;; XEmacs 20.x needs this
(if (not (fboundp 'find-buffer-visiting))
    (fset 'find-buffer-visiting 'get-file-buffer))
;; XEmacs <= 21.4.15 needs this:
(defalias 'ess-line-beginning-position
  (if (fboundp 'line-beginning-position)
      'line-beginning-position
    'point-at-bol))

(if (and (not (featurep 'xemacs))
         (string-match "XEmacs\\|Lucid" emacs-version))
    (provide 'xemacs))

;; XEmacs 21.x and Emacs 20.x need this
(cond ((fboundp 'replace-regexp-in-string)
       (defalias 'ess-replace-regexp-in-string 'replace-regexp-in-string))
      ((featurep 'xemacs)
       (defun ess-replace-regexp-in-string(regexp replace string)
         "Mimic GNU Emacs function replace-regexp-in-string with XEmacs' replace-in-string"
         (replace-in-string string regexp replace)))

      ;; GNU emacs <= 20 -- take Emacs' 21(.3)'s definition:
      (t (defun ess-replace-regexp-in-string (regexp rep string &optional
                                                     fixedcase literal subexp start)
           "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (ess-replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

           ;; To avoid excessive consing from multiple matches in long strings,
           ;; don't just call `replace-match' continually.  Walk down the
           ;; string looking for matches of REGEXP and building up a (reversed)
           ;; list MATCHES.  This comprises segments of STRING which weren't
           ;; matched interspersed with replacements for segments that were.
           ;; [For a `large' number of replacments it's more efficient to
           ;; operate in a temporary buffer; we can't tell from the function's
           ;; args whether to choose the buffer-based implementation, though it
           ;; might be reasonable to do so for long enough STRING.]
           (let ((l (length string))
                 (start (or start 0))
                 matches str mb me)
             (save-match-data
               (while (and (< start l) (string-match regexp string start))
                 (setq mb (match-beginning 0)
                       me (match-end 0))
                 ;; If we matched the empty string, make sure we advance by one char
                 (when (= me mb) (setq me (min l (1+ mb))))
                 ;; Generate a replacement for the matched substring.
                 ;; Operate only on the substring to minimize string consing.
                 ;; Set up match data for the substring for replacement;
                 ;; presumably this is likely to be faster than munging the
                 ;; match data directly in Lisp.
                 (string-match regexp (setq str (substring string mb me)))
                 (setq matches
                       (cons (replace-match (if (stringp rep)
                                                rep
                                              (funcall rep (match-string 0 str)))
                                            fixedcase literal str subexp)
                             (cons (substring string start mb) ; unmatched prefix
                                   matches)))
                 (setq start me))
               ;; Reconstruct a string from the pieces.
               (setq matches (cons (substring string start l) matches)) ; leftover
               (apply #'concat (nreverse matches)))))
         )
      )

;; remassoc exists as a built-in function in xemacs, but
;; not in GNU emacs
;;
(if (not (functionp 'remassoc))
    (defun remassoc (key a)
      "remove an association pair from an alist"
      (if a
          (let ((pair (car a)))
            (if (equal (car pair) key)
                (cdr a)
              (cons pair (remassoc key (cdr a))))))))

(if (not (fboundp 'w32-using-nt))
    (defun w32-using-nt ()
      "Return non-nil if literally running on Windows NT (i.e., not Windows 9X)."
      (and (eq system-type 'windows-nt) (getenv "SystemRoot"))))

(if (and (featurep 'xemacs)
         (fboundp 'extent-at)
         (fboundp 'make-extent)
         (fboundp 'set-extent-property))
    (defun ess-xemacs-insert-glyph (gl)
      "Insert a glyph at the left edge of point."
      (let ((prop 'myimage) ;; myimage is an arbitrary name, chosen to
            ;;                 (hopefully) not conflict with any other
            ;;                 properties. Change it if necessary.
            extent)
        ;; First, check to see if one of our extents already exists at
        ;; point.  For ease-of-programming, we are creating and using our
        ;; own extents (multiple extents are allowed to exist/overlap at the
        ;; same point, and it's quite possible for other applications to
        ;; embed extents in the current buffer without your knowledge).
        ;; Basically, if an extent, with the property stored in "prop",
        ;; exists at point, we assume that it is one of ours, and we re-use
        ;; it (this is why it is important for the property stored in "prop"
        ;; to be unique, and only used by us).
        (if (not (setq extent (extent-at (point) (current-buffer) prop)))
            (progn
              ;; If an extent does not already exist, create a zero-length
              ;; extent, and give it our special property.
              (setq extent (make-extent (point) (point) (current-buffer)))
              (set-extent-property extent prop t)
              ))
        ;; Display the glyph by storing it as the extent's "begin-glyph".
        (set-extent-property extent 'begin-glyph gl))))

;; XEmacs and NTemacs 19.x need these
(if (not (boundp 'w32-system-shells))
    (defvar w32-system-shells '("cmd" "cmd.exe" "command" "command.com"
                                "4nt" "4nt.exe" "4dos" "4dos.exe"
                                "ndos" "ndos.exe")
      "List of strings recognized as Windows NT/9X system shells.")
  )

(if (not (fboundp 'w32-system-shell-p))
    (defun w32-system-shell-p (shell-name)
      (and shell-name
           (member (downcase (file-name-nondirectory shell-name))
                   w32-system-shells)))
  )

(if (not (fboundp 'w32-shell-name))
    (defun w32-shell-name ()
      "Return the name of the shell being used."
      (or (and (boundp 'explicit-shell-file-name) explicit-shell-file-name)
          (getenv "ESHELL")
          (getenv "SHELL")
          (and (w32-using-nt) "cmd.exe")
          "command.com"))
  )

;; XEmacs and NTemacs 20.3 need this
(if (not (fboundp 'w32-shell-dos-semantics)) (defun w32-shell-dos-semantics ()
                                               "Return t if the interactive shell being used expects msdos shell semantics."
                                               (or (w32-system-shell-p (w32-shell-name))
                                                   (and (member (downcase (file-name-nondirectory (w32-shell-name)))
                                                                '("cmdproxy" "cmdproxy.exe"))
                                                        (w32-system-shell-p (getenv "COMSPEC")))))
  )

;; XEmacs need this (unless configured with  --with-mule=yes)
(if (not (boundp 'enable-multibyte-characters))
    (defvar enable-multibyte-characters nil
      "Non-nil means the buffer contents are regarded as multi-byte characters.
 This concept is handled completely differently on Xemacs."))

(defvar ess-has-tooltip
  (and (not (featurep 'xemacs))
       (>= emacs-major-version 21))
  "non-nil if 'tooltip can be required; typically nil for Xemacs.")

;; XEmacs on Windows needs this
(if (and ess-microsoft-p
         (not (fboundp 'w32-short-file-name)))
    (cond ((fboundp 'win32-short-file-name)
           (fset 'w32-short-file-name 'win32-short-file-name))
          ((fboundp 'mswindows-short-file-name)
           (fset 'w32-short-file-name 'mswindows-short-file-name))
          (t
           (warn "None of 'w32-short-file-name, 'win32-short-file-name,
or 'mswindows-short-file-name are defined!
You will have to manually set   ess-program-files (in ess-custom.el) to
the correct \"8.3\"-style directory name."))))


(defun ess-sleep ()
  "Put emacs to sleep for `ess-sleep-for-shell' seconds (floats work).
Sometimes its necessary to wait for a shell prompt."
  (if (featurep 'xemacs) (sleep-for ess-sleep-for-shell)
    (sleep-for 0 (truncate (* ess-sleep-for-shell 1000)))))

(unless (fboundp 'use-region-p)
  ;; emacs 23 needs this
  (defun use-region-p ()
    "Return t if the region is active and it is appropriate to act on it.
This is used by commands that act specially on the region under
Transient Mark mode.

The return value is t if Transient Mark mode is enabled and the
mark is active; furthermore, if `use-empty-active-region' is nil,
the region must not be empty.  Otherwise, the return value is nil.

For some commands, it may be appropriate to ignore the value of
`use-empty-active-region'; in that case, use `region-active-p'."
    (and (region-active-p)
         (or use-empty-active-region (> (region-end) (region-beginning)))))

  (defun region-active-p ()
    "Return t if Transient Mark mode is enabled and the mark is active.

Some commands act specially on the region when Transient Mark
mode is enabled.  Usually, such commands should use
`use-region-p' instead of this function, because `use-region-p'
also checks the value of `use-empty-active-region'."
    (and transient-mark-mode mark-active)))

(provide 'ess-compat)

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

;;; ess-compat.el ends here
