;;; ess-sas-a.el --- clean-room implementation of many SAS-mode features

;; Copyright (C) 1997--2009 A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Rodney A. Sparapani
;; Maintainer: ESS-core@r-project.org
;; Created: 17 November 1999
;; Keywords: languages

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Code:

;;; Table of Contents
;;; Section 1:  Variable Definitions
;;; Section 2:  Function Definitions
;;; Section 3:  Key Definitions

;;; Section 1:  Variable Definitions

(defvar ess-sas-file-path "."
  "Full path-name of the sas file to perform operations on.")

(defcustom ess-sas-data-view-libname " "
  "*SAS code to define a library for `ess-sas-data-view-fsview'
or `ess-sas-data-view-insight'."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-data-view-submit-options
  (if ess-microsoft-p "-noenhancededitor -nosysin -log NUL:"
    "-nodms -nosysin -log /dev/null -terminal")
  "*The command-line options necessary for your OS with respect to
`ess-sas-data-view-fsview' and `ess-sas-data-view-insight'."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-data-view-fsview-command "; proc fsview data="
  "*SAS code to open a SAS dataset with `ess-sas-data-view-fsview'."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-data-view-fsview-statement " "
  "*SAS code to perform a PROC FSVIEW statement with `ess-sas-data-view-fsview'."
  :group 'ess-sas
  :type  'string)

(make-variable-buffer-local 'ess-sas-data-view-fsview-statement)

(defcustom ess-sas-data-view-insight-command "; proc insight data="
  "*SAS code to open a SAS dataset with `ess-sas-data-view-insight'."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-data-view-insight-statement " "
  "*SAS code to perform a PROC FSVIEW statement with `ess-sas-data-view-insight'."
  :group 'ess-sas
  :type  'string)

(make-variable-buffer-local 'ess-sas-data-view-insight-statement)

(defcustom ess-sas-graph-view-suffix-regexp
  "[.]\\([eE]?[pP][sS]\\|[pP][dD][fF]\\|[gG][iI][fF]\\|[jJ][pP][eE]?[gG]\\|[tT][iI][fF][fF]?\\)"
  "*GSASFILE suffix regexp."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-graph-view-viewer-alist
  ;;creates something like
  ;;'(("[pP][dD][fF]" . "/usr/local/bin/acroread") ("[eE]?[pP][sS]" . "/usr/local/bin/gv")))
  (let ((ess-tmp-alist nil)
        (ess-tmp-ps nil) (ess-tmp-pdf nil))

    (setq ess-tmp-ps (executable-find (if ess-microsoft-p "gsview32" "gsview")))

    (if (not ess-tmp-ps) (setq ess-tmp-ps (executable-find "gv")))

    (if (not ess-tmp-ps) (setq ess-tmp-ps (executable-find "ghostview")))

    (setq ess-tmp-pdf (executable-find "evince"))

    (if (not ess-tmp-pdf) (setq ess-tmp-pdf (executable-find "xpdf")))

    (if (not ess-tmp-pdf) (setq ess-tmp-pdf (if ess-microsoft-p "acrord32" "acroread")))

    (if (and ess-tmp-ps ess-tmp-pdf)
        (setq ess-tmp-alist (list (cons "[eE]?[pP][sS]" ess-tmp-ps)
                                  (cons "[pP][dD][fF]" ess-tmp-pdf)))

      (if ess-tmp-ps
          (setq ess-tmp-alist (list (cons "[eE]?[pP][sS]" ess-tmp-ps)
                                    (cons "[pP][dD][fF]" ess-tmp-ps))))))

  "*Associate file name extensions with graphics image file viewers."
  :group 'ess-sas
  :type  'string)

;;(defcustom ess-sas-smart-back-tab nil
;;    "*Set to t to make C-TAB insert an end/%end; statement to close a block."
;;    :group 'ess-sas
;;)

(defcustom ess-sas-log-max 0
  "*If >0 and .log file exceeds this many bytes, just \"refresh\" this many bytes."
  :group 'ess-sas
  :type  'integer)

(defcustom ess-sas-rtf-font-name "Bitstream Vera Sans Mono"
  "*Name of font to create MS RTF with"
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-shell-buffer "*shell*"
  "*Name that you want to use for the shell buffer; buffer-local."
  :group 'ess-sas
  :type  'string)

(make-variable-buffer-local 'ess-sas-shell-buffer)

(defcustom ess-sas-shell-buffer-remote-host nil
  "*Remote host that you want to open a shell on."
  :group 'ess-sas
  :type '(choice (const nil) string))

(make-variable-buffer-local 'ess-sas-shell-buffer-remote-host)

(defcustom ess-sas-shell-buffer-remote-init "ssh"
  "*Command to open a shell on a remote host."
  :group 'ess-sas
  :type  'string)

(make-variable-buffer-local 'ess-sas-shell-buffer-remote-init)

(defcustom ess-sas-submit-mac-virtual-pc nil
  "*Non-nil means that you want to run Windows SAS in a
Virtual PC emulator on your Mac; buffer-local."
  :group 'ess-sas
  :type 'boolean)

(make-variable-buffer-local 'ess-sas-submit-mac-virtual-pc)

(defcustom ess-sas-submit-command sas-program
  "*Command to invoke SAS in batch; buffer-local."
  :group 'ess-sas
  :type  'string)

(make-variable-buffer-local 'ess-sas-submit-command)

(defcustom ess-sas-submit-command-options "-rsasuser"
  "*Options to pass to SAS in batch; buffer-local."
  :group 'ess-sas
  :type  'string)

(make-variable-buffer-local 'ess-sas-submit-command-options)

(defvar ess-sas-submit-method
  (if ess-microsoft-p
      (if (w32-shell-dos-semantics) 'ms-dos 'sh)
    (if (or (equal system-type 'Apple-Macintosh)
            (and ess-sas-submit-mac-virtual-pc (equal system-type 'darwin)))
        'apple-script 'sh))
  "Method used by `ess-sas-submit'.
The default is based on the value of the emacs variable `system-type'
and, on Windows, the function `w32-shell-dos-semantics'.
'sh               if *shell* runs sh, ksh, csh, tcsh or bash
'ms-dos           if *shell* follows MS-DOS semantics
'apple-script     *shell* unavailable in Mac Classic, use AppleScript,
                  also for Windows SAS in Virtual PC on Mac OS X

Unix users will get 'sh by default.

Windows users running bash in *shell* will get 'sh by default.

Windows users running MS-DOS in *shell* will get 'ms-dos by default.

Users accessing a remote machine with `telnet', `rlogin', `ssh', etc.,
should set this variable to 'sh regardless of their local shell
(since their remote shell is 'sh).")

(make-variable-buffer-local 'ess-sas-submit-method)

(defcustom ess-sas-graph-view-viewer-default
  (if ess-microsoft-p "explorer"
    (if (equal ess-sas-submit-method 'sh) "sdtimage"))
  "*Default graphics image file viewer."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-submit-post-command
  (if (equal ess-sas-submit-method 'sh) "&"
    (if ess-microsoft-p "-icon"))
  "*Command-line statement to post-modify SAS invocation"
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-submit-pre-command ;;"nohup"
  (if (equal ess-sas-submit-method 'sh)
      ;; nice is tricky, higher numbers give you lower priorities
      ;; if you are using csh/tcsh, the default priority is 4
      ;; if you are using most other shells, the default priority is 10,
      ;; and some implementations are higher, i.e. zsh unless you
      ;; specify "setopt no_bg_nice" in your ~/.zshrc
      ;; therefore, on the same machine, you can run at a higher or
      ;; lower priority by changing shells, although, the command
      ;; line is the same!
      ;; the following code should give you a priority of 10 regardless
      ;; of which shell is in use, but it will default to the old
      ;; behavior if csh or variant is not recognized
      ;; this should avoid the necessity of each user needing to set this
      ;; variable correctly based on the shell that they use and provide
      ;; an environment where all shells are treated equally

      (let* ((temp-shell (getenv "SHELL"))
             ;; AJR: old CYGWIN versions return nil for (getenv
             ;; "SHELL"), so we need to deal with it 'cause I have to
             (temp-char (if temp-shell
                            (string-match "/" temp-shell)
                          nil)))
        (while temp-char
          (setq temp-shell (substring temp-shell (+ 1 temp-char)))
          (setq temp-char (string-match "/" temp-shell)))

        (cond ((or (equal temp-shell "csh") (equal temp-shell "tcsh"))
               "nohup nice +6")
              (t "nohup nice")))
    (if ess-microsoft-p "start"))
  "*Command-line statement to precede SAS invocation, e.g. start or nohup"
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-suffix-1 "txt"
  "*The first suffix to associate with SAS."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-suffix-2 "csv"
  "*The second suffix to associate with SAS."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-suffix-regexp
  (concat "[.]\\([sS][aA][sS]\\|[lL][oO][gG]\\|[lL][sS][tT]"
          (if ess-sas-suffix-1 (concat
                                "\\|" (downcase ess-sas-suffix-1) "\\|" (upcase ess-sas-suffix-1)))
          (if ess-sas-suffix-2 (concat
                                "\\|" (downcase ess-sas-suffix-2) "\\|" (upcase ess-sas-suffix-2)))
          "\\)")
  "*Regular expression for SAS suffixes."
  :group 'ess-sas
  :type  'string)

(defcustom ess-sas-tab-stop-list
  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)
  "List of tab stop positions used by `tab-to-tab-stop' in ESS[SAS]."
  :group 'ess-sas)

(defcustom ess-sas-temp-root "-temp"
  "*Appended to root name of the temporary .sas file for `ess-sas-submit-region'."
  :group 'ess-sas
  :type  'string)

(defvar ess-sas-versions '("sas")
  "List of partial strings for versions of SAS to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of SAS is made available.  For example, if the
file \"sas8\" is found and this variable includes the string
\"sas\", a function called `M-x SAS8' will be available to run that
version of SAS.
If duplicate versions of the same program are found (which happens if
the same path is listed on `exec-path' more than once), they are
ignored by calling `ess-uniq-list'.
If you set this variable, you need to restart Emacs (and set this variable
before ess-site is loaded) for it to take effect.")


;;; Section 2:  Function Definitions


(defun ess-ebcdic-to-ascii-search-and-replace ()
  "*Search and replace EBCDIC text with ASCII equivalents."
  (interactive)
  (let ((ess-tmp-dd (executable-find "dd")) (ess-tmp-recode (executable-find "recode"))
        (ess-tmp-util nil) (ess-tmp-util-args nil))

    (if ess-tmp-dd (progn
                     (setq ess-tmp-util ess-tmp-dd)
                     (setq ess-tmp-util-args "conv=ascii"))

      (setq ess-tmp-util ess-tmp-recode)
      (setq ess-tmp-util-args "EBCDIC..ISO-8859-1"))

    (if ess-tmp-util
        (while (search-forward-regexp "[^\f\t\n -~][^\f\t\n -?A-JQ-Yb-jp-y]*[^\f\t\n -~]?" nil t)
          (call-process-region (match-beginning 0) (match-end 0)
                               ess-tmp-util t (list t nil) t ess-tmp-util-args)))))


(defun ess-exit-notify-sh (string)
  "Detect completion or failure of submitted job and notify the user."
                                        ;(let* ((exit-done "\\[[0-9]+\\]\\ *\\+*\\ *\\(Exit\\|Done\\).*$")
  (let* ((exit-done "\\[[0-9]+\\]\\ *\\+*\\ *\\(Exit\\|Done\\)[^\r\n]*") ; GNU Emacs needs this
         (beg (string-match exit-done string)))
    (if beg
        (message (substring string beg (match-end 0))))))


(defun ess-sas-append-log ()
  "Append ess-temp.log to the current .log file."
  (interactive)
  (ess-sas-goto "log" 'revert)
  (goto-char (point-max))
  (insert-file-contents (concat (ess-sas-temp-root) ".log"))
  (save-buffer))

(defun ess-sas-append-lst ()
  "Append ess-temp.lst to the current .lst file."
  (interactive)
  (ess-sas-goto "lst" 'revert)
  (goto-char (point-max))
  (insert-file-contents (concat (ess-sas-temp-root) ".lst"))
  (save-buffer))

(defun ess-sas-backward-delete-tab ()
  "Moves the cursor to the previous tab-stop, deleting any characters
on the way."
  (interactive)

  (let* (;; point of search
         ;;(ess-sas-search-point nil)
         ;; column of search
         ;;(ess-sas-search-column nil)
         ;; limit of search
         ;;(ess-sas-search-limit nil)
         ;; text to be inserted after a back-tab, if any
         ;;(ess-sas-end-text "end;")
         ;; current-column
         (ess-sas-column (current-column))
         ;; remainder of current-column and sas-indent-width
         (ess-sas-remainder (% ess-sas-column sas-indent-width)))

    (if (not (= ess-sas-column 0))
        (progn
          (if (= ess-sas-remainder 0)
              (setq ess-sas-remainder sas-indent-width))

          (let ((backward-delete-char-untabify-method 'nil))
            (backward-delete-char-untabify ess-sas-remainder t)
            (setq ess-sas-column (- ess-sas-column ess-sas-remainder))
            (move-to-column ess-sas-column)
            (setq left-margin ess-sas-column))
          ))
    ))

;; this feature was far too complicated to perfect
;;      (if ess-sas-smart-back-tab (progn
;;        (save-excursion
;;          (setq ess-sas-search-point
;;              (search-backward-regexp "end" nil t))

;;          (if (and ess-sas-search-point
;;              (search-backward-regexp "%" (+ ess-sas-search-point -1) t))
;;              (setq ess-sas-search-point (+ ess-sas-search-point -1))
;;          )

;;          (if (and ess-sas-search-point
;;              (not (equal ess-sas-column (current-column))))
;;              (setq ess-sas-search-point nil))
;;          )

;;        (save-excursion
;;          (setq ess-sas-search-point
;;              (search-backward-regexp "do\\|select"
;;                  ess-sas-search-point t))

;;          (setq ess-sas-search-column (current-column))

;;          (if ess-sas-search-point (progn
;;              (save-excursion
;;               (search-backward-regexp "^" nil t)
;;               (setq ess-sas-search-limit (point))
;;              )

;;              (if (search-backward-regexp "if.*then\\|else" ess-sas-search-limit t)
;;                  (setq ess-sas-search-point (point)))

;;              (if (search-backward-regexp "%" ess-sas-search-limit t) (progn
;;                  (setq ess-sas-end-text "%end;")
;;                  (setq ess-sas-search-point (point))
;;              ))

;;              (setq ess-sas-search-column (current-column))

;;              (if (not (equal ess-sas-column ess-sas-search-column))
;;                 (setq ess-sas-search-point nil))
;;        )))

;;        (if ess-sas-search-point (insert ess-sas-end-text))
;;         ))

(defun ess-sas-cd ()
  "Change directory, taking into account various issues with respect to
`ess-sas-file-path'."
                                        ;(interactive)
  (ess-sas-file-path)
  (ess-sas-goto-shell t)
  (comint-send-input)
  (if (equal ess-sas-submit-method 'sh)
      (insert "cd " (car (last (split-string (file-name-directory ess-sas-file-path)
                                             "\\([a-zA-Z][a-zA-Z]:\\|]\\)"))))
    (if (equal ess-sas-submit-method 'ms-dos) (progn
                                                (if (string-equal ":" (substring ess-sas-file-path 1 2)) (progn
                                                                                                           (insert (substring ess-sas-file-path 0 2))
                                                                                                           (comint-send-input)))
                                                (insert "cd \"" (convert-standard-filename
                                                                 (file-name-directory ess-sas-file-path)) "\""))))
  (comint-send-input))

(defun ess-sas-create-local-variables-alist (&optional file-or-buffer)
  "Create an alist of local variables from file-or-buffer, use the
current buffer if nil."

  (if file-or-buffer (set-buffer (ess-get-file-or-buffer file-or-buffer)))

  (ess-change-alist 'ess-kermit-remote-directory ess-kermit-remote-directory nil))

(defun ess-sas-data-view-fsview (&optional ess-sas-data)
  "Open a dataset for viewing with PROC FSVIEW."
  (interactive)
  (ess-save-and-set-local-variables)

  (save-excursion (let ((ess-tmp-sas-data nil)
                        (ess-tmp-sas-data-view-fsview-statement ess-sas-data-view-fsview-statement)
                        (ess-search-regexp
                         "[ \t=]\\([a-zA-Z_][a-zA-Z_0-9]*[.][a-zA-Z_][a-zA-Z_0-9]*\\)\\(&.*\\)?[. ,()\t;/]")
                        (ess-search-except
                         "^\\([wW][oO][rR][kK]\\|[fF][iI][rR][sS][tT]\\|[lL][aA][sS][tT]\\)[.]"))

                    (if ess-sas-data nil (save-match-data
                                           (search-backward-regexp "[ \t=]" nil t)

                                           (save-excursion
                                             (setq ess-tmp-sas-data
                                                   (ess-search-except ess-search-regexp ess-search-except)))

                                           (if (not ess-tmp-sas-data)
                                               (setq ess-tmp-sas-data
                                                     (ess-search-except ess-search-regexp ess-search-except t)))

                                           (setq ess-sas-data (read-string "Permanent SAS Dataset: " ess-tmp-sas-data))

                                           (ess-sas-goto-shell t)
                                           (ess-sas-cd)

                                           (insert (concat ess-sas-submit-pre-command " " ess-sas-submit-command
                                                           " -initstmt \"" ess-sas-data-view-libname ess-sas-data-view-fsview-command
                                                           ess-sas-data ";" ess-tmp-sas-data-view-fsview-statement "; run;\" "
                                                           ess-sas-submit-command-options " "
                                                           ess-sas-data-view-submit-options " " ess-sas-submit-post-command))
                                           (comint-send-input)
                                           )))))

(defun ess-sas-data-view-insight (&optional ess-sas-data)
  "Open a dataset for viewing with PROC INSIGHT."
  (interactive)
  (ess-save-and-set-local-variables)

  (save-excursion (let ((ess-tmp-sas-data nil)
                        (ess-tmp-sas-data-view-insight-statement ess-sas-data-view-insight-statement)
                        (ess-search-regexp
                         "[ \t=]\\([a-zA-Z_][a-zA-Z_0-9]*[.][a-zA-Z_][a-zA-Z_0-9]*\\)\\(&.*\\)?[. ,()\t;]")
                        (ess-search-except
                         "^\\([wW][oO][rR][kK]\\|[fF][iI][rR][sS][tT]\\|[lL][aA][sS][tT]\\)[.]"))

                    (if ess-sas-data nil (save-match-data
                                           (search-backward-regexp "[ \t=]" nil t)

                                           (save-excursion
                                             (setq ess-tmp-sas-data
                                                   (ess-search-except ess-search-regexp ess-search-except)))

                                           (if (not ess-tmp-sas-data)
                                               (setq ess-tmp-sas-data
                                                     (ess-search-except ess-search-regexp ess-search-except t)))

                                           (setq ess-sas-data (read-string "Permanent SAS Dataset: " ess-tmp-sas-data))

                                           (ess-sas-goto-shell t)
                                           (ess-sas-cd)

                                           (insert (concat ess-sas-submit-pre-command " " ess-sas-submit-command
                                                           " -initstmt \"" ess-sas-data-view-libname ess-sas-data-view-insight-command
                                                           ess-sas-data ";" ess-tmp-sas-data-view-insight-statement "; run;\" "
                                                           ess-sas-data-view-submit-options " " ess-sas-submit-post-command))
                                           (comint-send-input)
                                           )))))

(defun ess-sas-graph-view ()
  "Open a GSASFILE for viewing."
  (interactive)
                                        ;  (ess-sas-file-path)
  (ess-sas-goto-log 'no-error-check)

  (save-excursion (let (
                        (ess-tmp-length (length ess-sas-graph-view-viewer-alist))
                        (ess-tmp-counter 0)
                        (ess-tmp-graph nil)
                        (ess-tmp-graph-alist nil)
                        (ess-tmp-glyph nil)
                        (ess-tmp-graph-regexp
                                        ; (concat "[ ]RECORDS[ ]WRITTEN[ ]+TO[ ]\n?[ ]*\\(\\(\n\\|[^.]\\)*"
                         (concat "[ ][rR][eE][cC][oO][rR][dD][sS][ ][wW][rR][iI][tT][tT][eE][nN][ ]+[tT][oO][ ]\n?[ ]*\\(.*"
                                 ess-sas-graph-view-suffix-regexp "\\)")))
                                        ;           (concat "['\"]\\(.*" ess-sas-graph-suffix-regexp "\\)['\"]")))

                    (save-match-data
                      (search-backward-regexp "[ \t=]" nil t)

                      (save-excursion
                        (setq ess-tmp-graph (ess-search-except ess-tmp-graph-regexp)))

                      (if (not ess-tmp-graph)
                          (setq ess-tmp-graph (ess-search-except ess-tmp-graph-regexp nil t)))

                      (setq ess-tmp-graph (read-string "GSASFILE: "
                                                       (or ess-tmp-graph ess-sas-file-path)))

                      (if (fboundp 'ess-xemacs-insert-glyph) (progn
                                                               (if (string-match "[.][gG][iI][fF]" ess-tmp-graph)
                                                                   (setq ess-tmp-glyph 'gif)
                                                                 ;;else
                                                                 (if (string-match "[.][jJ][pP][eE]?[gG]" ess-tmp-graph)
                                                                     (setq ess-tmp-glyph 'jpeg)))))

                      ;;GNU Emacs graphics file image viewing mode loaded?
                      (if (and (boundp 'auto-image-file-mode) auto-image-file-mode
                               (string-match "[.][jJ][pP][eE]?[gG]" ess-tmp-graph))
                          (find-file ess-tmp-graph)
                        ;;else XEmacs graphics file image viewing mode loaded?
                        (if (and (fboundp 'image-mode)
                                 (string-match "[.]\\([jJ][pP][eE]?[gG]\\|[gG][iI][fF]\\)"
                                               ess-tmp-graph))
                            (find-file ess-tmp-graph)
                          ;;else XEmacs graphics file image viewing primitives loaded?
                          (if ess-tmp-glyph (progn
                                              (switch-to-buffer (file-name-nondirectory ess-tmp-graph))
                                              (ess-xemacs-insert-glyph
                                               (make-glyph (vector ess-tmp-glyph :file ess-tmp-graph))))

                            ;;else use the appropriate graphics file image viewer
                            (while (< ess-tmp-counter ess-tmp-length)
                              (setq ess-tmp-graph-alist
                                    (nth ess-tmp-counter ess-sas-graph-view-viewer-alist))
                              (setq ess-tmp-graph-regexp (car ess-tmp-graph-alist))

                              (if (string-match
                                   (concat "[.]" ess-tmp-graph-regexp) ess-tmp-graph)
                                  (progn
                                    (ess-sas-goto-shell t)
                                    (insert ess-sas-submit-pre-command " "
                                            (cdr ess-tmp-graph-alist) " " ess-tmp-graph
                                            (if (equal ess-sas-submit-method 'sh) " &"))
                                    (setq ess-tmp-glyph 'alist)
                                    (setq ess-tmp-counter ess-tmp-length))
                                ;;else
                                (setq ess-tmp-counter (+ ess-tmp-counter 1))))

                            (if (not ess-tmp-glyph) (progn
                                                      (ess-sas-goto-shell t)
                                                      (insert ess-sas-submit-pre-command " "
                                                              ess-sas-graph-view-viewer-default " " ess-tmp-graph
                                                              (if (equal ess-sas-submit-method 'sh) " &"))))

                            (comint-send-input))))))))

(defun ess-sas-file-path (&optional force)
  "Define `ess-sas-file-path' to be the current buffer depending on suffix."
  (interactive)

  (save-match-data (let ((ess-sas-temp-file (expand-file-name (buffer-name))))
                     (if (or force (string-match ess-sas-suffix-regexp ess-sas-temp-file)) ;;(progn
                         (setq ess-sas-file-path
                               (nth 0 (split-string ess-sas-temp-file "[<]")))))))
;; (setq ess-directory (file-name-directory ess-sas-file-path)))))))

(defun ess-sas-file-path-remote-host ()
  "Return the remote host, if any, associated with `ess-sas-file-path'."
  (interactive)

  (let* ((temp-colon-pos (string-match ":" ess-sas-file-path))
         (temp-list
          (if (or (not temp-colon-pos) (> temp-colon-pos 2))
              (if (equal ess-sas-file-path ".") nil
                (split-string (file-name-directory ess-sas-file-path)
                              "\\(@\\|:\\|]\\)"))
            (list ess-sas-file-path)))
         (temp-list-length (length temp-list)))
    (if (= temp-list-length 1) (setq temp-list nil)
      (if (= temp-list-length 2) (setq temp-list (car temp-list))
        (setq temp-list (nth 1 temp-list))))

    (if temp-list (setq temp-list
                        (car (last (split-string temp-list "/")))))
    temp-list))

(defun ess-sas-goto (suffix &optional revert no-create)
  "Find a file associated with a SAS file by suffix and revert if necessary."
  (let ((ess-temp-regexp (concat ess-sas-suffix-regexp "\\(@.+\\)?")))
    (save-match-data
      (if (or (string-match ess-temp-regexp (expand-file-name (buffer-name)))
              (string-match ess-temp-regexp ess-sas-file-path))
          (progn
            (ess-sas-file-path)
            (let* (
                   (ess-sas-temp-file (replace-match (concat "." suffix) t t
                                                     ess-sas-file-path))
                   (ess-sas-temp-buff (find-buffer-visiting ess-sas-temp-file))
                   (ess-temp-kermit-remote-directory ess-kermit-remote-directory))

              (if ess-sas-temp-buff (switch-to-buffer ess-sas-temp-buff)
                ;; else
                (if no-create (setq revert nil)
                  (if (file-exists-p ess-sas-temp-file)
                      (find-file ess-sas-temp-file))))
              ;; else
              ;;        (let* ((ess-sas-buffer-list (buffer-list))
              ;;         (ess-sas-buffer-list-index 0)
              ;;         (ess-sas-buffer-list-file nil)
              ;;         (ess-sas-buffer-list-length (length ess-sas-buffer-list)))
              ;;      (while (< ess-sas-buffer-list-index ess-sas-buffer-list-length)
              ;;                (setq ess-sas-buffer-list-file
              ;;              (buffer-file-name (nth ess-sas-buffer-list-index ess-sas-buffer-list)))
              ;;                (if (and ess-sas-buffer-list-file
              ;;              (string-match (concat "." suffix) ess-sas-buffer-list-file))
              ;;              (switch-to-buffer (nth ess-sas-buffer-list-index ess-sas-buffer-list))
              ;;              (setq ess-sas-buffer-list-index ess-sas-buffer-list-length)
              ;;                )
              ;;                (setq ess-sas-buffer-list-index (+ 1 ess-sas-buffer-list-index))
              ;;    )))

              (if (and (not no-create)
                       (or (string-equal suffix "log")
                           (string-equal suffix "lst")))
                  (ess-kermit-get (file-name-nondirectory ess-sas-temp-file)
                                  ess-temp-kermit-remote-directory))

              (if revert
                  (if (and (> ess-sas-log-max 0) (string-equal suffix "log")
                           (> (ess-num-or-zero (nth 7 (file-attributes ess-sas-temp-file)))
                              ess-sas-log-max))
                      (progn
                        (insert-file-contents ess-sas-temp-file nil 0
                                              ess-sas-log-max t)
                        t)

                    (ess-revert-wisely)) nil)))))))

;;(defun ess-sas-file (suffix &optional revert)
;;  "Please use `ess-sas-goto' instead."
;;  (let* ((tail (downcase (car (split-string
;;          (car (last (split-string (buffer-name) "[.]"))) "[<]"))))
;;(if (fboundp 'file-name-extension) (file-name-extension (buffer-name))
;;               (substring (buffer-name) -3)))
;;       (tail-in-tail-list (member tail (list "sas" "log" "lst"
;;                           ess-sas-suffix-1 ess-sas-suffix-2)))
;;       (root (if tail-in-tail-list (expand-file-name (buffer-name))
;;               ess-sas-file-path))
;;       (ess-sas-arg (concat (file-name-sans-extension root) "." suffix))
;;       (ess-sas-buf (find-buffer-visiting ess-sas-arg)))
;;    (if (equal tail suffix) (if revert (ess-revert-wisely))
;;      (if (not ess-sas-buf) (find-file ess-sas-arg)
;;          (switch-to-buffer ess-sas-buf)
;;          (if revert (ess-revert-wisely))))))


(defun ess-sas-goto-file-1 ()
  "Switch to ess-sas-file-1 and revert from disk."
  (interactive)
  (ess-sas-goto ess-sas-suffix-1 'revert))

(defun ess-sas-goto-file-2 ()
  "Switch to ess-sas-file-2 and revert from disk."
  (interactive)
  (ess-sas-goto ess-sas-suffix-2 'revert))

(defun ess-sas-goto-log (&optional ess-tmp-no-error-check)
  "Switch to the .log file, revert from disk and search for error messages."
  (interactive)

  (let ((ess-sas-error (concat
                        "^ERROR [0-9]+-[0-9]+:\\|^ERROR:\\|_ERROR_=1 _N_=\\|_ERROR_=1[ ]?$"
                        "\\|NOTE: MERGE statement has more than one data set with repeats"
                        "\\|NOTE: Variable .* is uninitialized."
                        "\\|NOTE: SAS went to a new line when INPUT statement reached past"
                        "\\|NOTE 485-185: Informat .* was not found"
                        "\\|NOTE: Estimated G matrix is not positive definite."
                        "\\|NOTE: Compressing data set .* increased size by"
                        "\\|NOTE: ERROR DETECTED IN ANNOTATE="
                        "\\|WARNING: Apparent symbolic reference .* not resolved."
                        "\\|WARNING: Length of character variable has already been set."
                        "\\|WARNING: Not all variables in the list "
                        "\\|WARNING: RUN statement ignored due to previous errors."
                        "\\|WARNING: Values exist outside the axis range"
                        "\\|Bus Error In Task\\|Segmentation Violation In Task"))
        (ess-sas-save-point nil)); (ess-sas-pop-mark nil))

    (if (ess-sas-goto "log" 'revert) (progn
                                       (setq ess-sas-save-point (point))
                                       (goto-char (point-min)))
      (setq ess-sas-save-point (point)))

                                        ;(if (number-char-or-marker-p ess-sas-save-point) (progn
    (if ess-tmp-no-error-check (goto-char ess-sas-save-point)
      (if (or (search-forward-regexp ess-sas-error nil t)
              (and (goto-char (point-min))
                   (search-forward-regexp ess-sas-error nil t)))
          t
                                        ; this feature never worked quite right (and was XEmacs only to boot)
                                        ; after highlighting an error message, moving point would cause an unwanted
                                        ; highlighting between point and mark; why god, why?!?
                                        ;
                                        ;       (if (and (boundp 'zmacs-regions) zmacs-regions)
                                        ;           (progn
                                        ;               (if ess-sas-pop-mark (pop-mark)
                                        ;                   (setq ess-sas-pop-mark t))
                                        ;               (push-mark (match-beginning 0) t)
                                        ;               (zmacs-activate-region)))
        (goto-char ess-sas-save-point)))))

(defun ess-sas-goto-lst ()
  "Switch to the .lst file and revert from disk."
  (interactive)
  (ess-sas-goto "lst" 'revert))

(defun ess-sas-goto-sas (&optional revert)
  "Switch to the .sas file."
  (interactive)
  (ess-sas-goto "sas" revert))

(defun ess-sas-goto-shell (&optional set-buffer)
  "Set `ess-sas-file-path' and goto `ess-sas-shell-buffer'.  If
optional argument is non-nil, then set-buffer rather than switch."
  (interactive)
  (ess-sas-file-path)

                                        ; The following let* block is an attempt to deal with remote directories.
  (let* ((temp-shell-buffer-remote-host
          (or ess-sas-shell-buffer-remote-host (ess-sas-file-path-remote-host)))
         (temp-shell-buffer-remote-init ess-sas-shell-buffer-remote-init)
         (temp-shell-buffer
          (if temp-shell-buffer-remote-host
              (concat "*" temp-shell-buffer-remote-host "*")
            ess-sas-shell-buffer))
         )

    (if (get-buffer temp-shell-buffer)
        (if set-buffer (set-buffer temp-shell-buffer)
          (switch-to-buffer temp-shell-buffer))
      (shell)
      (rename-buffer temp-shell-buffer)
      (ess-sleep) ; GNU Emacs needs this

      (if temp-shell-buffer-remote-host (progn
                                          (insert (concat
                                                   temp-shell-buffer-remote-init " " temp-shell-buffer-remote-host))
                                          (comint-send-input))
        )

      (if (eq ess-sas-submit-method 'sh)
          (add-hook 'comint-output-filter-functions 'ess-exit-notify-sh)) ;; 19.28
      ;; nil t) works for newer emacsen
      )
    )

  (goto-char (point-max))
                                        ; (insert "cd " ess-temp-directory)
                                        ; (comint-send-input))
  )

(defun ess-sas-interactive ()
  "And now for something completely different."
  (interactive)
  (ess-sas-file-path)

  (let ((ess-temp-sas-file
         (nth 0 (split-string
                 (car (last (split-string ess-sas-file-path "\\([a-zA-Z][a-zA-Z]:\\|]\\)"))) "[.]"))))
    ;;    (message "%s" ess-temp-sas-file)
    (setq ess-sas-shell-buffer "*iESS[SAS]*")
    (ess-sas-goto-shell)
    (insert (concat ess-sas-submit-command " " ess-sas-submit-command-options
                    " -altlog " ess-temp-sas-file ".log -altprint "
                    ess-temp-sas-file ".lst -stdio < /dev/tty"))
    (comint-send-input)
    (ess-add-ess-process)
    (ess-sas-goto-sas)
    (setq ess-sas-submit-method 'iESS)
    (setq ess-eval-visibly-p nil)
    ))
;;(defun ess-sas-interactive ()
;;    (interactive)
;;    (ess-sas-file-path)
;;    (setq ess-sas-submit-method 'iESS)
;;
;;    (let ((ess-temp-stderr " ") (ess-temp-stdout " ") (ess-temp-stdin " "))
;;    (setq ess-sas-shell-buffer "*LOG*")
;;    (ess-sas-goto-shell)
;;    (insert "tty")
;;    (comint-send-input)
;;    (sleep-for ess-sleep-for)
;;    (save-excursion (setq ess-temp-stderr (ess-search-except "\\(/dev/[a-z0-9/]+\\)" nil t)))
;;    (setq ess-sas-shell-buffer "*OUTPUT*")
;;    (ess-sas-goto-shell)
;;    (insert "tty")
;;    (comint-send-input)
;;    (sleep-for ess-sleep-for)
;;    (save-excursion (setq ess-temp-stdout (ess-search-except "\\(/dev/[a-z0-9/]+\\)" nil t)))
;;    (setq ess-sas-shell-buffer "*PROGRAM*")
;;    (ess-sas-goto-shell)
;;;;    (insert "tty")
;;    (comint-send-input)
;;    (sleep-for ess-sleep-for)
;;    (insert "sh")
;;    (comint-send-input)
;;    (sleep-for ess-sleep-for)
;;    (save-excursion (setq ess-temp-stdin (ess-search-except "\\(/dev/[a-z0-9/]+\\)" nil t)))
;;    (insert (concat ess-sas-submit-command " " ess-sas-submit-command-options " -stdio <"
;;      ess-temp-stdin " >1 " ess-temp-stdout " >2 " ess-temp-stderr))
;;    (comint-send-input)
;;    (ess-add-ess-process)
;;    (ess-sas-goto-sas)
;;))

(defun ess-sas-kill-buffers ()
  "Kill all buffers related to a .sas file."
  (interactive)
  (ess-sas-file-path)
  (ess-sas-goto "log" nil t)
  (kill-buffer nil)
  (ess-sas-goto "lst" nil t)
  (kill-buffer nil)
  (ess-sas-goto ess-sas-suffix-1 nil t)
  (kill-buffer nil)
  (ess-sas-goto ess-sas-suffix-2 nil t)
  (kill-buffer nil)
  (ess-sas-goto "sas" nil t)
  (kill-buffer nil)
  )

                                        ;  (condition-case nil
                                        ;      (progn
                                        ;        (require 'rtf-support)
                                        ;        (when (featurep 'rtf-support)

(defun ess-rtf-replace-chars ()
  "Convert a text file to an MS RTF file."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\n" nil t) (replace-match "\\par\n" nil t))
  (goto-char (point-min))
  (while (re-search-forward "\f" nil t) (replace-match "\\page\n" nil t))
  (goto-char (point-min))
  (while (re-search-forward "\t" nil t) (replace-match "\\tab" nil t)))

(defun ess-sas-rtf-portrait (&optional ess-tmp-font-size)
  "Creates an MS RTF portrait file from the current buffer."
  (interactive)
  (ess-sas-file-path t)
  (ess-revert-wisely)

                                        ;    (if (equal ess-tmp-font-size nil)
                                        ;       (setq ess-tmp-font-size "21"))

  (let
      ((ess-temp-rtf-file
        (replace-regexp-in-string "[.][^.]*$" ".rtf" ess-sas-file-path)))
                                        ;(rtf-export ess-temp-rtf-file)
    (copy-file ess-sas-file-path ess-temp-rtf-file t)
    (ess-sas-goto "rtf" t)
    (ess-rtf-replace-chars)
                                        ;(goto-char (point-min))
    ;;(replace-regexp "\\\\fmodern .*;" (concat "\\\\fmodern " ess-sas-rtf-font-name ";"))
                                        ;(if (re-search-forward "\\\\fmodern .*;" nil t)
                                        ;    (replace-match (concat "\\\\fmodern " ess-sas-rtf-font-name ";") nil nil))
                                        ;(goto-line 2)
    (goto-char (point-min))
    (insert (concat
             "{\\rtf1\\ansi{\\fonttbl\\f1\\fmodern " ess-sas-rtf-font-name ";}\n"
             "\\margl720\\margr720\\margt720\\margb720\n"
             "{\\colortbl;\\red0\\green0\\blue0;\\red0\\green0\\blue255;\\red0\\green255\\blue255;\\red0\\green255\\blue0;\\red255\\green0\\blue255;\\red255\\green0\\blue0;\\red255\\green255\\blue0;\\red255\\green255\\blue255;\\red0\\green0\\blue128;\\red0\\green128\\blue128;\\red0\\green128\\blue0;\\red128\\green0\\blue128;\\red128\\green0\\blue0;\\red128\\green128\\blue0;\\red128\\green128\\blue128;\\red192\\green192\\blue192;}\n"
             "{\\stylesheet{\\s15\\plain\\f1\\fs16\\cf1\\cb8\\lang1024 Emacs Text;}{\\*\\cs16 \\additive\\f1\\fs16\\cf1\\cb8\\lang1024 Emacs Base Style;}}\n"
             "{\\plain\\s15{\\cs16\\cs16\\f1\\fs16\\cf1\\cb8\\lang1024{\\cs16\\f1\\fs16\\cf1\\cb8\\lang1024\n"))

    (goto-char (point-max))
    (insert "}}}}\n")
                                        ;(goto-char (point-min))
    ;;(while (replace-regexp "\\\\fs[0-9]+" (concat "\\\\fs" ess-tmp-font-size)) nil)
                                        ;(while (re-search-forward "\\\\fs[0-9]+" nil t)
                                        ;  (replace-match (concat "\\\\fs" ess-tmp-font-size) nil nil))
    (save-buffer)
    (kill-buffer (current-buffer))))

(defun ess-sas-rtf-us-landscape ()
  "Creates an MS RTF US landscape file from the current buffer."
  (interactive)
  (ess-sas-rtf-portrait "16")
  (ess-sas-goto "rtf" t)
  (goto-char (point-min))
  (forward-line 3)
  (insert (concat "{\\*\\pgdsctbl\n"
                  "{\\pgdsc0\\pgdscuse195\\lndscpsxn\\pgwsxn15840\\pghsxn12240\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\pgdscnxt0 Default;}}\n"
                  "\\landscape\\paperh12240\\paperw15840\\margl1800\\margr1800\\margt1440\\margb1440\\sectd\\sbknone\\lndscpsxn\\pgwsxn15840\\pghsxn12240\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\ftnbj\\ftnstart1\\ftnrstcont\\ftnnar\\aenddoc\\aftnrstcont\\aftnstart1\\aftnnrlc\n"))
  (save-buffer)
  (kill-buffer (current-buffer)))

(defun ess-sas-rtf-a4-landscape ()
  "Creates an MS RTF A4 landscape file from the current buffer."
  (interactive)
  (ess-sas-rtf-portrait "16")
  (ess-sas-goto "rtf" t)
  (goto-char (point-min))
  (forward-line 3)
  (insert (concat "{\\*\\pgdsctbl\n"
                  "{\\pgdsc0\\pgdscuse195\\lndscpsxn\\pgwsxn16837\\pghsxn11905\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\pgdscnxt0 Default;}}\n"
                  "\\landscape\\paperh11905\\paperw16837\\margl1800\\margr1800\\margt1440\\margb1440\\sectd\\sbknone\\lndscpsxn\\pgwsxn16837\\pghsxn11905\\marglsxn1800\\margrsxn1800\\margtsxn1440\\margbsxn1440\\ftnbj\\ftnstart1\\ftnrstcont\\ftnnar\\aenddoc\\aftnrstcont\\aftnstart1\\aftnnrlc\n"))
  (save-buffer)
  (kill-buffer (current-buffer)))
                                        ;))
                                        ;    (error nil))

(defun ess-sas-submit ()
  "Save the .sas file and submit to shell using a function that
depends on the value of  `ess-sas-submit-method'"
  (interactive)
  (ess-sas-file-path)
  (ess-sas-goto-sas)
  (save-buffer)
  (hack-local-variables)
                                        ;(ess-save-and-set-local-variables)

  (cond
   ((eq ess-sas-submit-method 'apple-script)
    (ess-sas-submit-mac ess-sas-submit-command
                        ess-sas-submit-command-options))
   ((eq ess-sas-submit-method 'ms-dos)
    (ess-sas-submit-windows ess-sas-submit-command
                            ess-sas-submit-command-options))
   ((eq ess-sas-submit-method 'iESS)
    (ess-sas-submit-iESS ess-sas-submit-command
                         ess-sas-submit-command-options))
   ((eq ess-sas-submit-method 'sh)
    (ess-sas-submit-sh ess-sas-submit-command
                       ess-sas-submit-command-options))
   (t (ess-sas-submit-sh ess-sas-submit-command
                         ess-sas-submit-command-options)))
                                        ;  (ess-sas-goto-sas)
  )

(defun ess-sas-submit-iESS (arg1 arg2)
  "iESS
Submit a batch job in an inferior-ESS buffer.  The buffer should
(1) have telnet access and be running a shell on a remote machine
or
(2) be running a shell on the local machine.

The user can telnet to the remote computer and then declare the
*telnet-buffer* to be an inferior ESS buffer with the `ess-add-ess-process'
command.  When using a remote computer, the .sas file must live on the
remote computer and be accessed through `ange-ftp'.  When
`ess-sas-submit' saves a file, it is therefore saved on the remote
computer.  The various functions such as `ess-sas-goto-lst' retrieve
their files from the remote computer.  Local copies of the .sas .lst
.log and others may be made manually with `write-buffer'."
  ;;  (ess-eval-linewise (concat "cd  default-directory))
(ess-force-buffer-current "Process to load into: ")
(ess-eval-linewise
 (concat "cd " (car (last
                     (split-string (file-name-directory ess-sas-file-path) "\\(:\\|]\\)")))))
(ess-eval-linewise (concat arg1 " " arg2 " " (buffer-name) " &")))

(defun ess-sas-submit-mac (arg1 arg2)
  "If you are using Mac SAS, then arg1, `ess-sas-submit-command', should be
the AppleScript command \"invoke SAS using program file\", and, if necessary,
arg2, `ess-sas-submit-command-options', is a string of the form
\"with options { \\\"option-1\\\", \\\"option-2\\\", etc.}\".  If you are
using Windows SAS with the PC emulator Virtual PC, then `ess-sas-submit-command'
should be ..."
                                        ;(ess-save-and-set-local-variables)

  (do-applescript (concat arg1 " \""
                          (if (not ess-sas-submit-mac-virtual-pc)
                              (unix-filename-to-mac default-directory))
                          (buffer-name) "\"" arg2)))

(defun ess-sas-submit-region ()
  "Write region to temporary file, and submit to SAS."
  (interactive)
  (ess-sas-file-path)
  (hack-local-variables t)
  (write-region (region-beginning) (region-end)
                (concat (ess-sas-temp-root) ".sas"))

  (let ((arg1 ess-sas-submit-command)
        (arg2 ess-sas-submit-command-options))
    (save-excursion
      (ess-sas-goto-shell t)

      (if (and (w32-shell-dos-semantics)
               (string-equal ":" (substring ess-sas-file-path 1 2)))
          (progn
            (insert (substring ess-sas-file-path 0 2))
            (comint-send-input)
            ))

      (insert "cd \"" (convert-standard-filename
                       (file-name-directory ess-sas-file-path)) "\"")
      (comint-send-input)

      (insert (concat ess-sas-submit-pre-command " " arg1
                      " " arg2
                      " " (ess-sas-temp-root) " " ess-sas-submit-post-command))
      (comint-send-input)
      ))
  )

(defun ess-sas-submit-sh (arg1 arg2)
  "Unix or bash in the *shell* buffer.
Multiple processing is supported on this platform.
SAS may not be found in your PATH.  You can alter your PATH to include
SAS or you can specify the PATHNAME (PATHNAME can NOT contain spaces),
i.e. let arg1 be your local equivalent of
\"/usr/local/sas612/sas\"."
  (if (string-equal (substring
                     (file-name-nondirectory ess-sas-file-path) 0 1) ess-kermit-prefix)
      (progn
        (ess-kermit-send)
        (ess-sas-goto-shell t)
        (insert ess-sas-submit-pre-command " " arg1 " "
                (substring (file-name-sans-extension
                            (file-name-nondirectory ess-sas-file-path)) 1)
                " " arg2 " " ess-sas-submit-post-command)
        (comint-send-input))
    ;;else
    (ess-sas-goto-shell t)
    (ess-sas-cd)
                                        ;      (insert "cd " (car (last (split-string (file-name-directory ess-sas-file-path)
                                        ;"\\([a-zA-Z][a-zA-Z]:\\|]\\)"))))
                                        ;      (comint-send-input)
    (insert ess-sas-submit-pre-command " " arg1 " "
            (file-name-sans-extension (file-name-nondirectory ess-sas-file-path))
            " " arg2 " " ess-sas-submit-post-command))
                                        ;    (ess-sleep)
  (comint-send-input))

(defun ess-sas-submit-windows (arg1 arg2)
  "Windows using MS-DOS prompt in the *shell* buffer.
Multiple processing is supported on this platform.
On most Windows installations, SAS will not be found in your
PATH so you should alter your PATH to include SAS, i.e.

SET PATH=%PATH%;C:\\Program Files\\SAS

Or you can specify the PATHNAME directly (you must escape
spaces by enclosing the string in \\\"'s), i.e. let
`ess-sas-submit-command' be \"\\\"C:\\Program Files\\SAS\\sas.exe\\\"\".
Keep in mind that the maximum command line length in MS-DOS is
127 characters so altering your PATH is preferable."
                                        ;(ess-save-and-set-local-variables)
  (ess-sas-goto-shell t)
  (if (string-equal ":" (substring ess-sas-file-path 1 2))
      (progn
        (insert (substring ess-sas-file-path 0 2))
        (comint-send-input)
        )
    )
  (insert "cd \"" (convert-standard-filename
                   (file-name-directory ess-sas-file-path)) "\"")
  (comint-send-input)
  (insert ess-sas-submit-pre-command " " arg1 " -sysin \""
          (file-name-sans-extension (file-name-nondirectory ess-sas-file-path)) "\" "
          arg2 " " ess-sas-submit-post-command)
  (comint-send-input))

(defun ess-sas-tab-to-tab-stop ()
  "Tab to next tab-stop and set left margin."
  (interactive)
  (tab-to-tab-stop)
  (setq left-margin (current-column))
  )

(defun ess-sas-temp-root ()
  "Return `ess-sas-file-path' sans extension with `ess-sas-temp-root' appended."
  (concat (file-name-sans-extension ess-sas-file-path) ess-sas-temp-root))

(defun ess-sas-transcript (&optional strip)
  "Comment .log messages to create a .sas program; use C-u to strip."
  (interactive "P")
  (save-excursion
    (goto-char (point-min))

    (while (search-forward-regexp (concat
                                   "^\\(\\(1[ \t]+The SAS System\\|\\|NOTE\\|WARNING\\|ERROR\\|"
                                   "[ \t]+\\(\\(real\\|cpu\\) time\\|Licensed to\\|Engine:\\|"
                                   "Physical Name:\\|File Name=\\|Owner Name=\\|Group Name=\\|"
                                   "Access Permission=\\|File Size (bytes)=\\|Pipe command=\\|"
                                   "RECFM=[DFNPV],LRECL=\\|[0-9]+:[0-9]+[ /t]+[0-9]+:[0-9]+\\|"
                                   "[1-9][0-9]* at [0-9]+:[0-9]+[ /t]+[1-9][0-9]* at [0-9]+:[0-9]+\\)\\).*$"
                                   "\\|[0-9]+\\([ \t]+!\\)?\\|MPRINT([_A-Z]+):\\|"
                                   "[ \t]+\\(values at the places given by: (Line):(Column).\\|"
                                   "The m\\(in\\|ax\\)imum record length was [1-9][0-9]*.\\|"
                                   "One or more lines were truncated.\\|"
                                   "Each place is given by: (Number of times) at (Line):(Column).\\|"
                                   "[0-9][0-9]:[0-9][0-9] [MTWFS][aeioudhnrst]+day, [JFMASOND]"
                                   "[aeiouybcghlmnprstv]+ [1-9][0-9]?, 20[0-9][0-9]\\)\\)")
                                  nil t) (replace-match (if strip " " "/*\\&*/") t))
    ))

(defun ess-sas-toggle-sas-listing-mode (&optional force)
  "Toggle SAS-listing-mode for .lst files."
  (interactive)
  (ess-sas-goto-lst)

  (if (equal (cdr (assoc "\\.[lL][sS][tT]\\'" auto-mode-alist)) 'SAS-listing-mode) (progn
                                                                                     (setq auto-mode-alist (delete '("\\.[lL][sS][tT]\\'" . SAS-listing-mode) auto-mode-alist))
                                                                                     (setq buffer-read-only nil)
                                                                                     (ess-listing-minor-mode 0))
    (setq auto-mode-alist (append '(("\\.[lL][sS][tT]\\'" . SAS-listing-mode)) auto-mode-alist))
    (setq buffer-read-only t)
    (ess-listing-minor-mode 1)))

(defun ess-sas-toggle-sas-log-mode ()
  "Toggle SAS-log-mode for .log files."
  (interactive)

  (ess-sas-goto-log)
  (kill-buffer nil)

                                        ;  (if (equal (cdr (assoc "\\.[lL][oO][gG]\\'" auto-mode-alist)) 'SAS-log-mode) (progn
                                        ;      (setq auto-mode-alist (delete '("\\.[lL][oO][gG]\\'" . SAS-log-mode) auto-mode-alist))
                                        ;      (setq buffer-read-only nil)
                                        ;      (ess-transcript-minor-mode 0)
                                        ;      (font-lock-mode 0))
                                        ;      (setq auto-mode-alist (append '(("\\.[lL][oO][gG]\\'" . SAS-log-mode)) auto-mode-alist))
                                        ;      (setq buffer-read-only t)
                                        ;      (ess-transcript-minor-mode 1)
                                        ;      (font-lock-mode 1)
                                        ;      (font-lock-fontify-buffer))

  (if (equal (cdr (assoc "\\.[lL][oO][gG]\\'" auto-mode-alist)) 'SAS-log-mode)
      (setq auto-mode-alist (delete '("\\.[lL][oO][gG]\\'" . SAS-log-mode) auto-mode-alist))
    (setq auto-mode-alist (append '(("\\.[lL][oO][gG]\\'" . SAS-log-mode)) auto-mode-alist)))
  (ess-sas-goto-log))

(defun ess-sas-versions-create ()
  "Generate the `M-x SASV' functions for starting other versions of SAS.
See `ess-sas-versions' for strings that determine which functions are created.

The local variable `ess-sas-versions-created' is used to return list of
the new SAS defuns, if any, that were created.  The defuns will normally
be placed on the menubar upon ESS initialisation."

  ;; This works by creating a temp buffer where the template function is
  ;; edited so that V is replaced by the version number
  (let ((template "")
        (beg)
        (versions)
        (version)
        (eval-buf (get-buffer-create "*ess-temp-sas-evals*"))
        (ess-sas-versions-created)
        )
    ;;
    ;; This is the template function used for creating M-x SASV.
    (setq template "(defun SASV (&optional start-args)
  \"Call SASV, i.e., the SAS version 'SASV' using ESS.
This function was generated by `ess-sas-versions-create'.\"
  (interactive \"P\")
  (let ((inferior-SAS-program-name \"SASV\"))
    (SAS start-args)))

")
    (save-excursion
      (set-buffer eval-buf)
      ;; clear the buffer.
      (delete-region (point-min) (point-max))

      ;; Find which versions of SAS we want.  Remove the pathname, leaving just
      ;; the name of the executable.
      (setq versions
            (ess-uniq-list
             (mapcar 'file-name-nondirectory
                     (apply 'nconc
                            (mapcar 'ess-find-exec-completions
                                    ess-sas-versions)))))
      (ess-write-to-dribble-buffer
       (format "(SAS): ess-sas-versions-create making M-x defuns for %s"
               (mapconcat 'identity versions " ")))
      (setq ess-sas-versions-created versions) ;keep copy for returning at end.
      ;; Iterate over each string in VERSIONS, creating a new defun each time.
      (while versions
        (setq version (car versions)
              versions (cdr versions))
        (setq beg (point))
        (insert template)
        (goto-char beg)
        (while (search-forward "SASV" nil t)
          (replace-match version t t))
        (goto-char (point-max))
        )
      ;; buffer has now been created with defuns, so eval them!
      (eval-buffer)
      (kill-buffer eval-buf)
      )
    ess-sas-versions-created))


;;; Section 3:  Key Definitions


(defun ess-sas-edit-keys-set (&optional arg)
  "Set TAB/RET key in `SAS-mode'.
If arg is nil
    TAB is `sas-indent-line' and
    RET is `newline-and-indent'.
Else
    TAB is `ess-sas-tab-to-tab-stop',
    C-TAB is `ess-sas-backward-delete-tab' and
    RET is `newline'."
  (interactive)

  (if arg
      (progn
        (if (and (equal emacs-major-version 19) (equal emacs-minor-version 28))
            (define-key sas-mode-local-map [C-tab] 'ess-sas-backward-delete-tab)
          ;;else
          (define-key sas-mode-local-map [(control tab)] 'ess-sas-backward-delete-tab))
        (define-key sas-mode-local-map [return] 'newline)
        (define-key sas-mode-local-map "\t" 'ess-sas-tab-to-tab-stop))
    ;;else
    (define-key sas-mode-local-map [return] 'newline-and-indent)
    (define-key sas-mode-local-map "\t" 'sas-indent-line)))

(defvar ess-sas-edit-keys-toggle nil
  "Toggle TAB/RET key in `SAS-mode'.
nil binds TAB to `sas-indent-line' and RET to `newline-and-indent'.
Non-nil binds TAB to `ess-sas-tab-to-tab-stop',
C-TAB to `ess-sas-backward-delete-tab', and RET to `newline'.")

(defun ess-sas-edit-keys-toggle (&optional arg)
  "Toggle `ess-sas-edit-keys-toggle'.  Optional arg is still
accepted for backward compatibility, however, arg is ignored."
  (interactive)

  (setq ess-sas-edit-keys-toggle (not ess-sas-edit-keys-toggle))
  (ess-sas-edit-keys-set ess-sas-edit-keys-toggle)
  )

(defvar ess-sas-global-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions in all modes.")

(defun ess-sas-global-pc-keys ()
  "PC-like SAS key definitions"
  (interactive)
  (when (featurep 'rtf-support)
    (global-set-key [(control f1)] 'ess-sas-rtf-portrait)
    (global-set-key [(control f2)] 'ess-sas-rtf-us-landscape))
  (global-set-key (quote [f2]) 'ess-revert-wisely)
  (global-set-key (quote [f3]) 'ess-sas-goto-shell)
  (global-set-key (quote [f4]) 'ess-sas-goto-file-1)
  (global-set-key (quote [f5]) 'ess-sas-goto-sas)
  (global-set-key (quote [f6]) 'ess-sas-goto-log)
  (global-set-key [(control f6)] 'ess-sas-append-log)
  (global-set-key (quote [f7]) 'ess-sas-goto-lst)
  (global-set-key [(control f7)] 'ess-sas-append-lst)
  (global-set-key (quote [f8]) 'ess-sas-submit)
  (global-set-key [(control f8)] 'ess-sas-submit-region)
  (global-set-key (quote [f9]) 'ess-sas-data-view-fsview)
  (global-set-key [(control f9)] 'ess-sas-data-view-insight)
  (global-set-key (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (global-set-key [(control f10)] 'ess-sas-toggle-sas-listing-mode)
  (global-set-key (quote [f11]) 'ess-sas-goto-file-2)
  (global-set-key [(control f11)] 'ess-ebcdic-to-ascii-search-and-replace)
  (global-set-key (quote [f12]) 'ess-sas-graph-view)
  (if (and ess-sas-edit-keys-toggle
           (equal emacs-major-version 19) (equal emacs-minor-version 28))
      (global-set-key [C-tab] 'ess-sas-backward-delete-tab)
                                        ;else
    (global-set-key [(control tab)] 'ess-sas-backward-delete-tab))
                                        ;(define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  (setq ess-sas-global-pc-keys t)
  (setq ess-sas-global-unix-keys nil)
  (setq ess-sas-local-pc-keys nil)
  (setq ess-sas-local-unix-keys nil)
  )

(defvar ess-sas-global-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions in all modes.")

(defun ess-sas-global-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (interactive)
  (when (featurep 'rtf-support)
    (global-set-key [(control f1)] 'ess-sas-rtf-portrait)
    (global-set-key [(control f2)] 'ess-sas-rtf-us-landscape))
  (global-set-key (quote [f2]) 'ess-revert-wisely)
  (global-set-key (quote [f3]) 'ess-sas-submit)
  (global-set-key [(control f3)] 'ess-sas-submit-region)
  (global-set-key (quote [f4]) 'ess-sas-goto-sas)
  (global-set-key (quote [f5]) 'ess-sas-goto-log)
  (global-set-key [(control f5)] 'ess-sas-append-log)
  (global-set-key (quote [f6]) 'ess-sas-goto-lst)
  (global-set-key [(control f6)] 'ess-sas-append-lst)
  (global-set-key (quote [f7]) 'ess-sas-goto-file-1)
  (global-set-key (quote [f8]) 'ess-sas-goto-shell)
  (global-set-key (quote [f9]) 'ess-sas-data-view-fsview)
  (global-set-key [(control f9)] 'ess-sas-data-view-insight)
  (global-set-key (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (global-set-key [(control f10)] 'ess-sas-toggle-sas-listing-mode)
  (global-set-key (quote [f11]) 'ess-sas-goto-file-2)
  (global-set-key [(control f11)] 'ess-ebcdic-to-ascii-search-and-replace)
  (global-set-key (quote [f12]) 'ess-sas-graph-view)
  (if (and ess-sas-edit-keys-toggle
           (equal emacs-major-version 19) (equal emacs-minor-version 28))
      (global-set-key [C-tab] 'ess-sas-backward-delete-tab)
                                        ;else
    (global-set-key [(control tab)] 'ess-sas-backward-delete-tab))
                                        ;(define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  (setq ess-sas-global-pc-keys nil)
  (setq ess-sas-global-unix-keys t)
  (setq ess-sas-local-pc-keys nil)
  (setq ess-sas-local-unix-keys nil)
  )

(defvar ess-sas-local-pc-keys nil
  "Non-nil if function keys use PC-like SAS key definitions
in SAS-mode and related modes.")

(defun ess-sas-local-pc-keys ()
  "PC-like SAS key definitions."
  (interactive)
  (when (featurep 'rtf-support)
    (define-key sas-mode-local-map [(control f1)] 'ess-sas-rtf-portrait)
    (define-key sas-mode-local-map [(control f2)] 'ess-sas-rtf-us-landscape))
  (define-key sas-mode-local-map (quote [f2]) 'ess-revert-wisely)
  (define-key sas-mode-local-map (quote [f3]) 'ess-sas-goto-shell)
  (define-key sas-mode-local-map (quote [f4]) 'ess-sas-goto-file-1)
  (define-key sas-mode-local-map (quote [f5]) 'ess-sas-goto-sas)
  (define-key sas-mode-local-map (quote [f6]) 'ess-sas-goto-log)
  (define-key sas-mode-local-map [(control f6)] 'ess-sas-append-log)
  (define-key sas-mode-local-map (quote [f7]) 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [(control f7)] 'ess-sas-append-lst)
  (define-key sas-mode-local-map (quote [f8]) 'ess-sas-submit)
  (define-key sas-mode-local-map [(control f8)] 'ess-sas-submit-region)
  (define-key sas-mode-local-map (quote [f9]) 'ess-sas-data-view-fsview)
  (define-key sas-mode-local-map [(control f9)] 'ess-sas-data-view-insight)
  (define-key sas-mode-local-map (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (define-key sas-mode-local-map [(control f10)] 'ess-sas-toggle-sas-listing-mode)
  (define-key sas-mode-local-map (quote [f11]) 'ess-sas-goto-file-2)
  (define-key sas-mode-local-map [(control f11)] 'ess-ebcdic-to-ascii-search-and-replace)
  (define-key sas-mode-local-map (quote [f12]) 'ess-sas-graph-view)
                                        ;(define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  (setq ess-sas-global-pc-keys nil)
  (setq ess-sas-global-unix-keys nil)
  (setq ess-sas-local-pc-keys t)
  (setq ess-sas-local-unix-keys nil)
  )

(defvar ess-sas-local-unix-keys nil
  "Non-nil if function keys use Unix-like SAS key definitions
in SAS-mode and related modes.")

(defun ess-sas-local-unix-keys ()
  "Unix/Mainframe-like SAS key definitions"
  (interactive)
  (when (featurep 'rtf-support)
    (define-key sas-mode-local-map [(control f1)] 'ess-sas-rtf-portrait)
    (define-key sas-mode-local-map [(control f2)] 'ess-sas-rtf-us-landscape))
  (define-key sas-mode-local-map (quote [f2]) 'ess-revert-wisely)
  (define-key sas-mode-local-map (quote [f3]) 'ess-sas-submit)
  (define-key sas-mode-local-map [(control f3)] 'ess-sas-submit-region)
  (define-key sas-mode-local-map (quote [f4]) 'ess-sas-goto-sas)
  (define-key sas-mode-local-map (quote [f5]) 'ess-sas-goto-log)
  (define-key sas-mode-local-map [(control f5)] 'ess-sas-append-log)
  (define-key sas-mode-local-map (quote [f6]) 'ess-sas-goto-lst)
  (define-key sas-mode-local-map [(control f6)] 'ess-sas-append-lst)
  (define-key sas-mode-local-map (quote [f7]) 'ess-sas-goto-file-1)
  (define-key sas-mode-local-map (quote [f8]) 'ess-sas-goto-shell)
  (define-key sas-mode-local-map (quote [f9]) 'ess-sas-data-view-fsview)
  (define-key sas-mode-local-map [(control f9)] 'ess-sas-data-view-insight)
  (define-key sas-mode-local-map (quote [f10]) 'ess-sas-toggle-sas-log-mode)
  (define-key sas-mode-local-map [(control f10)] 'ess-sas-toggle-sas-listing-mode)
  (define-key sas-mode-local-map (quote [f11]) 'ess-sas-goto-file-2)
  (define-key sas-mode-local-map [(control f11)] 'ess-ebcdic-to-ascii-search-and-replace)
  (define-key sas-mode-local-map (quote [f12]) 'ess-sas-graph-view)
                                        ;(define-key sas-mode-local-map "\C-c\C-p" 'ess-sas-file-path)
  (setq ess-sas-global-pc-keys nil)
  (setq ess-sas-global-unix-keys nil)
  (setq ess-sas-local-pc-keys nil)
  (setq ess-sas-local-unix-keys t)
  )

(provide 'ess-sas-a)

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

;;; ess-sas-a.el ends here
