




;;-*-emacs-lisp-*-
;;;  file name: sas.el
;;;
;;;  Version 1.4
;;; 
;;;    sas-mode:  indent, run etc, SAS programs.
;;;    Copyright (C) 1994 Tom Cook
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;  Author:   Tom Cook
;;;            Dept. of Biostatistics
;;;            University of Wisconsin - Madison
;;;            Madison, WI 53706
;;;            cook@biostat.wisc.edu
;;   Last change: 2/1/95
;;
;;
;;  Acknowledgements:  
;;
;;  Menu code for XEmacs/Lucid emacs and startup mods
;;  contributed by arossini@biostats.hmc.psu.edu

;;  startup mods.  1/30/95

(if (assoc "\\.sas$" auto-mode-alist) nil
  (setq auto-mode-alist
        (append 
         '(("\\.sas$" . sas-mode)
           ;; ("\\.s$" . S-mode) ;; Uncomment this if you use .s for S files
           ("\\.lst$" . sasl-mode))
         auto-mode-alist)))


; (require 'sas-site)  Need to think about this before making permanent.

;;  variables section

(defvar sas-mode-version "1.4" 
  "Version number for Sas-mode")

(defvar sas-mode-map () "")
(defvar sasl-mode-map () "")
(defvar sasd-mode-map () "")
(defvar sas-mode-syntax-table nil "")
(defvar sasl-mode-syntax-table nil "")
(defvar sas-require-confirmation t
  "*Require confirmation when revisiting sas-output which has changed on disk.")
(defvar sas-file-name () "Root (sans .sas) name of sas file.  Includes directory.")
(defvar sas-file-root () "base name (sans .sas) of file.")

;; added sas-program 4/29/94.  user can specify a different version of sas.
(defvar sas-program "sas" "*Name of program which runs sas.")

;; user variables 
(defvar sas-mode-hook nil "")
(defvar sasl-mode-hook nil "")
(defvar sasd-mode-hook nil "")
(defvar sas-pre-run-hook nil
  "Hook to execute prior to running SAS vis submit-sas.")
(defvar sas-custom-file-name "~/.sas"
  "Customization file. If you plan to change this, this variable must be
set before loading sas-mode.")

(defvar sas-options-string ""
  "*Options to be passed to sas as if typed on the command line.")
(defvar sas-indent-width 4 "*Amount to indent sas statements")
(defvar sas-notify t "*Beep and display message when job is done?")  ;; added 4/7/94
(defvar sas-error-notify t
  "*If sas-notify is t, then indicate errors in log file upon completion")
;; added 5/2/94
(defvar sas-get-options nil "Options to be passed to SAS in sas-get-dataset")
(defvar sas-get-options-history nil "History list of Options passed to SAS in sas-get-dataset")
(defvar sas-dataset nil "Name of sas dataset associated with this buffer")
(make-variable-buffer-local 'sas-dataset)
(defvar sas-page-number-max-line 3
  "*Number of lines from the page break in which to search for the page number")
(defvar sas-submitable t
  "*If t the current sas program is submittable.  This variable exists so that
certain files, which are not intended to be run alone but rather %included
won't be run by mistake.")
(make-variable-buffer-local 'sas-submitable)

(defvar sas-indent-ignore-comment "*"
  "*Comments with start with this string are ignored in indentation.")

(defvar sas-notify-popup nil
  "*If t (and sas-notify is also t), causes emacs to create a
popup window when the SAS job is finished.")

(defvar sas-buffer-name nil
  "Stores the name of the buffer for the sas source code in order to check
whether or not the file has changed.")
(make-variable-buffer-local 'sas-buffer-name)
(defvar sas-tmp-libname "_tmp_" "*Libname to use for sas-get-dataset.")

;; keymaps etc...

(require 'shell)

(if (not sas-mode-syntax-table)
    (setq sas-mode-syntax-table (make-syntax-table)))

(if (not sasl-mode-syntax-table)
    (setq sasl-mode-syntax-table (make-syntax-table)))

(if sas-mode-map ()
   (setq sas-mode-map (make-sparse-keymap))
   (define-key sas-mode-map "\C-c\C-i" 'indent-sas-statement)
   (define-key sas-mode-map "\C-c\C-a" 'beginning-of-sas-statement)
   (define-key sas-mode-map "\e\C-a" 'beginning-of-sas-proc)
   (define-key sas-mode-map "\e\C-e" 'next-sas-proc)
   (define-key sas-mode-map "\C-cs" 'switch-to-sas-source)
   (define-key sas-mode-map "\C-cS" 'switch-to-sas-source-other-window)
   (define-key sas-mode-map "\C-cg" 'switch-to-sas-log)
   (define-key sas-mode-map "\C-cG" 'switch-to-sas-log-other-window)
   (define-key sas-mode-map "\C-cl" 'switch-to-sas-lst)
   (define-key sas-mode-map "\C-cL" 'switch-to-sas-lst-other-window)
   (define-key sas-mode-map "\C-cr" 'run-sas-on-region)
   (define-key sas-mode-map "\C-c\C-l" 'submit-sas)
   (define-key sas-mode-map "\C-cd" 'sas-get-dataset)
   (define-key sas-mode-map "\C-c\C-c" 'switch-to-sas-process-buffer)

   (cond ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
	       (= emacs-major-version 19))
	  (define-key sas-mode-map [menu-bar sas]
	    (cons "SAS" (make-sparse-keymap "sas")))
	  (define-key sas-mode-map [menu-bar sas get-dataset]
	    '("View a SAS dataset" . sas-get-dataset))
	  (define-key sas-mode-map [menu-bar sas indent-current]
	    '("Indent current statment" . indent-sas-statement))
	  (define-key sas-mode-map [menu-bar sas log-other-window]
	    '("SAS log other window" . switch-to-sas-log-other-window))
	  (define-key sas-mode-map [menu-bar sas log]
	    '("SAS log" . switch-to-sas-log))
	  (define-key sas-mode-map [menu-bar sas lst-other-window]
	    '("SAS lst other window" . switch-to-sas-lst-other-window))
	  (define-key sas-mode-map [menu-bar sas lst]
	    '("SAS lst " . switch-to-sas-lst))
	  (define-key sas-mode-map [menu-bar sas run-on-region]
	    '("Submit Region " . run-sas-on-region))
	  (define-key sas-mode-map [menu-bar sas run]
	    '("Submit File " . submit-sas))
	  ))
   )

(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       ;; XEmacs menu code
       (defvar sas-mode-menu
         '("SAS"
           ["View a SAS dataset" sas-get-dataset t]
           ["Indent current statment" indent-sas-statement t]
           ["SAS log other window" switch-to-sas-log-other-window t]
           ["SAS log" switch-to-sas-log t]
           ["SAS lst other window" switch-to-sas-lst-other-window t]
           ["SAS lst" switch-to-sas-lst t]
           ["Submit Region" run-sas-on-region t]
           ["Submit File" submit-sas t]))
       
       (defun sas-mouse-menu (e)
         (interactive "e")
         (mouse-set-point e)
         (beginning-of-line)
         (search-forward ":" nil t)
         (popup-menu sas-mode-menu))
       
       (defun sas-install-menubar ()
         (if default-menubar
             (let ((menu (cond ((eq major-mode 'sas-mode) sas-mode-menu)
                               (t (error "not in SAS mode")))))
               (set-buffer-menubar (copy-sequence default-menubar))
               (add-menu nil "SAS" (cdr menu)))))
       
       (add-hook 'sas-mode-hook 'sas-install-menubar)

       (require 'sas-fontlock)
       (add-hook 'sas-mode-hook 'turn-on-font-lock)))


(if sasl-mode-map ()
   (setq sasl-mode-map (make-sparse-keymap))
   (define-key sasl-mode-map "\C-cs" 'switch-to-sas-source)
   (define-key sasl-mode-map "\C-cS" 'switch-to-sas-source-other-window)
   (define-key sasl-mode-map "\C-cg" 'switch-to-sas-log)
   (define-key sasl-mode-map "\C-cG" 'switch-to-sas-log-other-window)
   (define-key sasl-mode-map "\C-cl" 'switch-to-sas-lst)
   (define-key sasl-mode-map "\C-cL" 'switch-to-sas-lst-other-window)
   (define-key sasl-mode-map "\C-cp" 'sas-fix-page-numbers)
   (define-key sasl-mode-map "\C-cP" 'sas-page-fix)
   (define-key sasl-mode-map "\C-cf" 'fix-page-breaks)
   (define-key sasl-mode-map "\C-cd" 'sas-get-dataset)
   (define-key sasl-mode-map "\C-c\C-c" 'switch-to-sas-process-buffer)

   (cond ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
	       (= emacs-major-version 19))
 	  (define-key sasl-mode-map [menu-bar sas]
	    (cons "SAS" (make-sparse-keymap "sas")))
	  (define-key sasl-mode-map [menu-bar sas get-dataset]
	    '("View a SAS dataset" . sas-get-dataset))
	  (define-key sasl-mode-map [menu-bar sas renumber-pages]
	    '("Renumber remaining pages" . sas-page-fix))
	  (define-key sasl-mode-map [menu-bar sas renumber-page]
	    '("Renumber current page" . sas-fix-page-numbers))
	  (define-key sasl-mode-map [menu-bar sas log-other-window]
	    '("SAS log other window " . switch-to-sas-log-other-window))
	  (define-key sasl-mode-map [menu-bar sas log]
	    '("SAS log" . switch-to-sas-log))
	  (define-key sasl-mode-map [menu-bar sas lst-other-window]
	    '("SAS lst other window" . switch-to-sas-lst-other-window))
	  (define-key sasl-mode-map [menu-bar sas lst]
	    '("SAS lst " . switch-to-sas-lst))
	  (define-key sasl-mode-map [menu-bar sas source-other-window]
	    '("SAS source other window" . switch-to-sas-source-other-window))
	  (define-key sasl-mode-map [menu-bar sas source]
	    '("SAS source" . switch-to-sas-source))
	  ))
   )


(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       ;; XEmacs menu code.
       (defvar sasl-mode-menu
	 '("SASl"
	   ["View a SAS dataset" sas-get-dataset t]
	   ["Renumber remaining pages" sas-page-fix t]
	   ["Renumber current page" sas-fix-page-numbers t]
	   ["SAS log other window" switch-to-sas-log-other-window t]
	   ["SAS log" switch-to-sas-log t]
	   ["SAS lst other window" switch-to-sas-lst-other-window t]
	   ["SAS lst" switch-to-sas-lst t]
	   ["SAS source other window" switch-to-sas-source-other-window t]
	   ["SAS source" switch-to-sas-source t]))
       
       (defun sasl-mouse-menu (e)
	 (interactive "e")
	 (mouse-set-point e)
	 (beginning-of-line)
	 (search-forward ":" nil t)
	 (popup-menu sasl-mode-menu))
       
       (defun sasl-install-menubar ()
	 (if default-menubar
	     (let ((menu (cond ((eq major-mode 'sasl-mode) sasl-mode-menu)
			       (t (error "not in SASl mode")))))
	       (set-buffer-menubar (copy-sequence default-menubar))
	       (add-menu nil "SASl" (cdr menu)))))

       (add-hook 'sasl-mode-hook 'sasl-install-menubar)
       ))


(if sasd-mode-map ()
  (setq sasd-mode-map (make-sparse-keymap))
  (define-key sasd-mode-map "\C-cr" 'revert-sas-dataset)
  (define-key sasd-mode-map "\C-cs" 'switch-to-dataset-source-buffer)
  (define-key sasd-mode-map "\C-cg" 'switch-to-dataset-log-buffer)
  (define-key sasd-mode-map "\C-cd" 'sas-get-dataset)
  (cond ((and (not (string-match "XEmacs\\|Lucid" emacs-version))
	       (= emacs-major-version 19))

	 ;; FSF Emacs Menu code
	 (define-key sasd-mode-map [menu-bar sas]
	   (cons "SAS" (make-sparse-keymap "sas")))
	 (define-key sasd-mode-map [menu-bar sas get-dataset]
	   '("View a SAS dataset" . sas-get-dataset))
	 (define-key sasd-mode-map [menu-bar sas dataset-log-buffer]
	   '("SAS log" . switch-to-dataset-log-buffer))
	 (define-key sasd-mode-map [menu-bar sas source-buffer]
	   '("SAS source " . switch-to-dataset-source-buffer))
	 (define-key sasd-mode-map [menu-bar sas revert]
	   '("Revert dataset" . revert-sas-dataset))
	 ))
  )


(cond ((string-match "XEmacs\\|Lucid" emacs-version)
       ;; XEmacs menu code
       (defvar sasd-mode-menu
	 '("SASd"
	   ["View a SAS dataset" sas-get-dataset t]
	   ["Revert dataset" revert-sas-dataset t]
	   ["SAS log" switch-to-sas-log t]
	   ["SAS source" switch-to-sas-source t]))
       (defun sasd-mouse-menu (e)
	 (interactive "e")
	 (mouse-set-point e)
	 (beginning-of-line)
	 (search-forward ":" nil t)
	 (popup-menu sasd-mode-menu))
       (defun sasd-install-menubar ()
	 (if default-menubar
	     (let ((menu (cond ((eq major-mode 'sasd-mode) sasd-mode-menu)
			       (t (error "not in SASd mode")))))
	       (set-buffer-menubar (copy-sequence default-menubar))
	       (add-menu nil "Sasd" (cdr menu)))))
       (add-hook 'sasd-mode-hook 'sasd-install-menubar)
       ))


;;  function definitions 

(defun sas-mode-variables ()
  (make-local-variable 'sentence-end)
  (setq sentence-end ";[\t\n */]*")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "^[ \t]*$")
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "^[ \t]*$")
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'sas-indent)      ;; use my indent
  ;(setq indent-line-function 'sas-indent-line)  ;; use riggle indent
  (make-local-variable 'comment-start)
  (setq comment-start "\\*\\|/\\*")
  (make-local-variable 'comment-end)
  (setq comment-end ";\\|\\*/")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\*+")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'sas-file-name)
  (make-local-variable 'sas-file-root)
  )


(defun sas-mode ()
  "Major mode for editing and running sas programs.

INDENTATION:
-----------
TAB indents the current line.  Usually the indentation of the current line
is derived from the previous line.  If this line is not indented correctly,
the wrong indentation could be propagated.  The function `indent-region' is
useful for reindenting large sections of code.  LFD (C-j) breaks the current
line and starts the following line with the correct indentation.

  In order to work correctly, the following conventions (and maybe more)
should be adhered to.  These conventions are not be unreasonable, and are
probably good coding form.

  1)  Only one sas program statement per line.  (`;` should be the last
  non-white character on a given line.)

  2) While SAS seems to allow the use of keywords like PROC, DATA and DO as
  variable names their use may confuse sas-mode, especially if they appear
  at the beginning of a line.

  3) The strings \"%macro\", \"%do\", \"%end\", \"%mend\" may confuse the
  indentation commands if they are embedded in comments.  \"%do\" should be
  ok if NOT preceeded by white-space, while the others are only troublesome
  if they are the first non-white characters on a line.

  3) All data steps and proc steps should be followed by a run statement.
  This convention is required to make macros indent properly.  Failure to
  adhere to this convention won't goof things up too much.  The function
  `sas-check-run-statements' can be used to check this.

  4) Comments contained between `/*' `*/' pairs will be indented as if they
  were ordinary statements except for the first, whose indentation will be
  uneffected.  Comments between `*' and `;' will be indented as if they were
  ordinary statements unless the beginning of the comment matches the
  variable `sas-indent-ignore-comment'.  Such comments will be ignored by
  the indentation commands.  The default value for
  `sas-indent-ignore-comment' is \"*\" (which is matched by all such
  comments) so unless this variable is changed, all comments of this type
  will be ignored.

INVOKING SAS:
------------ 
SAS programs can be submitted by running `submit-sas' (bound to
\\[submit-sas]).  A subregion of code can be submitted with
`run-sas-on-region' (\\[run-sas-on-region]).  Moving between the .sas, .log
and .lst files can be easily achieved with keys described below.  

Occasionally a SAS run will need intervention.  Usually when it is out of
disk space.  If you notice that SAS have stopped running, but hasn't
notified you, you may want to check the process buffer (usually \\[switch-to-sas-process-buffer]).
This buffer is essentially a shell buffer so that you may interact with SAS
as if you had submitted the job from the shell.  Furthermore, if you want to
terminate a job that is running, you may switch to the process buffer and
type C-c C-c.

The function `sas-get-dataset' (\\[sas-get-dataset]) prompts for the name of
a sas dataset (as a system file including the .ssd01 extension) and produces
a buffer containing the output from PROC CONTENTS and PROC PRINT for the
given dataset.  You will be prompted for a string `sas-get-options' which is
often useful for setting linesize, or restricting the number of records
shown for a large file for example.  This variable has it's own history list
so that previous values of this variable can be recalled with
\\[backward-list] and \\[forward-list].

MOTION COMMANDS:
---------------
There are three motion commands built into sas-mode:

`beginning-of-sas-statement' (\\[beginning-of-sas-statement]) moves point to
the beginning of the current statement.  This is useful for multi-line
statements.  

`beginning-of-sas-proc' (\\[beginning-of-sas-proc]) moves point to the
beginning of the current PROC or DATA step.

`next-sas-proc' (\\[next-sas-proc]) moves point to the beginning of the NEXT
PROC or DATA step.

KEYBINDINGS:
-----------
\\{sas-mode-map}

CUSTOMIZATION:
-------------
Customization can be achieved in several ways.

  1) Entry into sas-mode runs `sas-mode-hook' for source files.  Entry into
  sasl-mode runs `sasl-mode-hook' for .log and .lst files.

  2) A number of variables ares listed below and can be set in .emacs using
  `setq'.  

  3) Alternate keybindings and other customizations can be placed in a file
  called .sas in your home directory.  This file can be redefined using the
  variable `sas-custom-file-name'.  (In .emacs)

SAS-MODE VARIABLES:
------------------
Some customization variables (and their defaults) are as follows:

 sas-indent-width 4
    is the amount continuation lines, do loops, etc are indented.

 sas-require-confirmation t
    This means that when you use the sas-mode commands to switch buffers it
    will automatically update with the disk version.  The default value is t
    which means that emacs will ask if you want the new version if it has
    changed.  (Typically after rerunning a sas program.)

 sas-notify t
    When you use `submit-sas' (\\[submit-sas]) to run your sas program it
    runs in the \"background\" so that you can use emacs while it is
    running.  By default emacs will beep and generate a message upon
    completion.  Setting sas-notify to nil displays a process buffer (like
    shell-mode) and the notification takes place in that buffer, but emacs
    won't notify you when it's done.

sas-error-notify t
    By default, when sas-notify is `t' emacs will automatically reread the
    log file to see if there are any errors in it.  The message it gives you
    should indicate the presence of errors and switching to the log file
    will put you at the first error message.  If you don't want to
    automatically overwrite the old version of the log file, then set
    sas-error-notify to nil as above.

 sas-options-string \"\"
    This is a list of options that sas automatically runs when you submit
    with sas-mode as if you had typed them on the command line when you
    execute sas. The default is \"\", but you can say things like

    (setq sas-options-string \"-linesize 132 -noovp\")

    which is equivalent to running sas with
        
    sas -linesize 132 -noovp filename

 sas-program \"sas\"
    Name of the program which invokes SAS.  For example, if you want a
    different version of SAS, including the statement,

    (setq sas-program \"sas607\")

    in .emacs or .sas will cause sas-mode to invoke sas607.

 sas-indent-ignore-comment \"*\"
    If you want certain comments but not others to be ignored by indentation
    commands you can set this variable to a regular expression matched by
    such comments.  For example if sas-indent-ignore-comment is set to
    \"*\\*\" (the second \"*\" needs the \\ since it is part of a regular
    expression), then comments starting with \"**\" will be ignored while
    comments starting with a single \"*\" will be treated as ordinary
    comments.

 sas-notify-popup t
    If t, and sas-notify is t, emacs creates a pop-up window when it has
    completed.  You must click the mouse in order to continue.

 sas-custom-file-name ~/.sas
    Name of file containing customization commands.  If you want to change
    this file name, it must be done BEFORE loading sas-mode, in ~/.emacs is
    preferable.  This file is loaded AFTER loading sas-mode, so any
    keybindings or functions may be redefined here without being changed by
    sas-mode.

 sas-submitable t
    If nil, requires confirmation before executing with submit-sas.  This is
    useful for files which are not intended to be run on their own, but rather 
    using %include in another file.  This feature prevents the user from
    inadvertently submitting such a file.  This variable (as well as any others
    may be automatically set by inserting code like the following at the end of
    the file.

      * Local Variables: ;
      * sas-submitable: nil ;
      * End: ;
      page ;

    The first three lines are SAS comments and will be ignored by SAS by
    used by emacs.  The \"page ;\" statement prevents emacs from getting fooled
    when the comments are echoed in the log file.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map sas-mode-map)
  (setq major-mode 'sas-mode)
  (setq mode-name "SAS")
  (sas-mode-variables)
  (set-sas-file-name)
  (run-hooks 'sas-mode-hook)
  (message "SAS mode updated 2/1/95.  See C-hm for documentation.")
  )

(defun sasl-mode ()
  "Major mode for editing sas log and lst files.
\\{sasl-mode-map}
Entry into this mode calls the value of `sasl-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sasl-mode-map)
  (setq major-mode 'sasl-mode)
  (setq mode-name "SAS")
  (if (null sas-file-name) (set-sas-file-name))
  (run-hooks 'sasl-mode-hook))

(defun sasd-mode ()
  "Major mode for viewing sas datasets.
\\{sasd-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map sasd-mode-map)
  (setq major-mode 'sasd-mode)
  (setq mode-name "SAS")
  (run-hooks 'sasd-mode-hook)
  )



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
          (indent-region (point) end (+ (current-column) (1- sas-indent-width)))
          (search-forward ";" (point-max) 1 1)
          (setq arg (1- arg)))))))

(defun sas-indent ()
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
    (replace-regexp "$" "")
    (goto-char (point-min))
    (replace-regexp "\\([^\\$]+\\)" "\n\\1")
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
 
;; (defun switch-to-sas-file (suff &optional revert silent)
;;   "Switches to sas \"SUFF\" file associated with the current file"
;;   (let* ((sfile sas-file-name)
;;          (buf (get-file-buffer (concat sfile "." suff)))
;;          (sas-require-confirmation
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
;; (defun switch-to-sas-file-other-window (suff)
;;   "Switches to sas \"SUFF\" file associated with the current file"
;;   (let* ((sfile sas-file-name)
;;          (buf (get-file-buffer (concat sfile "." suff))))
;;     (if (or sas-require-confirmation (string-equal suff "sas") (not buf))
;;         (find-file-other-window (concat sfile "." suff))
;;       (progn (switch-to-buffer-other-window buf)
;;              (if (not (verify-visited-file-modtime (current-buffer)))
;;                  (progn (revert-buffer t t)
;;                         (message "File has changed on disk.  Buffer automatically updated.")))))
;;     (setq sas-file-name sfile))
;;   (if (string-equal suff "sas")
;;       (if (not (string-equal major-mode "sas-mode"))
;;           (sas-mode))
;;     (if (not (string-equal major-mode "sasl-mode"))
;;         (sasl-mode))))

(defun switch-to-sas-file (suff)
  "Switches to sas \"SUFF\" file associated with the current file"
  (switch-to-buffer (set-sas-file-buffer suff))
  )

(defun switch-to-sas-file-other-window (suff)
  "Switches to sas \"SUFF\" file associated with the current file"
  (switch-to-buffer-other-window (set-sas-file-buffer suff))
  )

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
          (sas-mode))
    (if (not (string-equal major-mode "sasl-mode"))
        (sasl-mode)))
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
	      (sas-buf  sas-buffer-name)
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
    (if (and sas-notify-popup window-system)
        (x-popup-dialog
         t
         (list "SAS Menu" (cons msg  nil) )))
    ;;(if (not (minibuffer-window-active-p)) (princ msg))
    (princ msg)
    )))



;; 5/2/94 Modified run-sas-on-region to separate log and output buffers.
;; 
(defun run-sas-on-region (start end append &optional buffer)
  "Submit region to SAS"
  (interactive "r\nP")
  (message "----  Running SAS  ----")
  (let ((sfile sas-file-name)
        (shell-file-name "/bin/sh")
        serror buff)
    (setq buffer (or buffer "*SAS output*"))
    (save-excursion
      (shell-command-on-region
       start end;; added sas-program 
       (concat sas-program " -nonews -stdio 2> /tmp/_temp_.log" nil))
      (get-buffer-create "*SAS Log*")
      (save-window-excursion
        (switch-to-buffer "*SAS Log*")
        (erase-buffer)
        (insert-file-contents "/tmp/_temp_.log")
        (delete-file "/tmp/_temp_.log")
        (setq serror (re-search-forward "^ERROR" nil t))
        (if serror () (bury-buffer)))
      (setq buff (get-buffer-create buffer))
      (save-window-excursion
        (switch-to-buffer buff)
        (setq sas-file-name sfile)
        (if append
            (progn
              (end-of-buffer)
              (insert "\f\n"))
          (erase-buffer))
        (if (get-buffer "*Shell Command Output*")
            (progn (insert-buffer "*Shell Command Output*")
                   (kill-buffer "*Shell Command Output*"))
          (insert "SAS completed with no output."))
        (if append () (sasl-mode))
        (message "----  SAS Complete ----")))
    (if (not serror)
        (switch-to-buffer-other-window  buff)
      (switch-to-buffer-other-window "*SAS Log*")
      (goto-char serror)
      (beep)
      (message "Error found in log file.")
      )))
  
(defun switch-to-dataset-log-buffer ()
  "Switch to log buffer for run-sas-on-region."
  (interactive)
  (switch-to-buffer-other-window "*SAS Log*"))

(defun switch-to-dataset-source-buffer ()
  "Switch to source buffer for run-sas-on-region."
  (interactive)
  (switch-to-buffer-other-window (format " *sas-tmp-%s*" sas-dataset)))

(defun sas-get-dataset (filename &optional arg opts-p append buffer vars)
  "Run proc contents and proc print on SAS dataset.  Automatically prompts 
for SAS options to use.  Default options are defined by the variable
`sas-get-options'.  Output may be updated from within output buffer with
C-cr if dataset changes.  Also, the source code which generates the output
may be edited with C-cs.  Typing C-cr within the output buffer reexecutes
the (modified) source code."
  (interactive "fName of SAS dataset (file name):")
  (let ((file (file-name-nondirectory filename))
        (dir (file-name-directory filename))
        (opts sas-get-options)
        (minibuffer-history sas-get-options-history)
        buf fsize)
    (setq buffer (or buffer (concat "*" file "*")))
    (setq opts (if opts-p opts (read-string "SAS options: " opts)))
    (setq sas-get-options-history minibuffer-history)
    (cond ((string-match (substring file -6 nil) "\\.ssd01")
      (setq file (substring file 0 (- (length file) 6))))
    (t (error "This file is not a SAS dataset.")))
    (setq buf (format " *sas-tmp-%s*" file))
    (get-buffer-create buf)
    (save-window-excursion
      (switch-to-buffer buf)
      (erase-buffer)
      (setq default-directory dir)
      (if opts 
          (insert (format "options  %s ;\n" opts)))
      (insert (format "title \"Contents of SAS dataset `%s'\" ;\n" file))
      (insert (format "libname %s '%s' ;\n" sas-tmp-libname dir))
      (if (not (equal arg 1))
               (insert (format "proc contents data = %s.%s ;\n" sas-tmp-libname file)))
      (if (equal arg 2) ()
        (insert (format "proc print data = %s.%s ;\n" sas-tmp-libname file))
        (if vars (insert (format "  var %s ;\n" vars))))
      (run-sas-on-region (point-min) (point-max) append
                         buffer)
      (get-buffer buffer)
      (if append () (sasd-mode))  ;; added 5/5/94 
      (setq sas-dataset file))
    (if (get-buffer-window buffer t)
        (raise-frame (window-frame (get-buffer-window buffer t)))
    (display-buffer buffer (not append)))
    ))
    
(defun revert-sas-dataset ()
  "Revert current sas dataset from disk version"
  (interactive)
  (let* ((file sas-dataset)
        (buf (format " *sas-tmp-%s*" file))
        (pos (point)))
      (save-window-excursion
        (switch-to-buffer buf)
        (run-sas-on-region (point-min) (point-max) nil
                           (concat "*" file ".ssd01*"))
        )
      (goto-char pos)  ;; added 6/9/94
    (sasd-mode)  ;; added 5/5/94 
    (setq sas-dataset file)))

(if (file-exists-p sas-custom-file-name) (load sas-custom-file-name))

(provide 'sas)

(message "SAS mode updated 2/1/95 See C-hm for changes.")

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

  

;;  Change Log:
;;  2/1/95  Modified indentation to work better with /* */ comments.
;;
;;  1/30/95  Added changes contributed by arossini@biostats.hmc.psu.edu -
;;            startup for auto-mode-alist
;;            XEmacs/Lucid emacs
;; 
;;  8/30/94  Added new optional arguments to sas-get-dataset in order to implement 
;;           some features in sas.data.el
;;  
;;  8/3/94   Fixed some bugs in indentation w.r.t comments introduced 6/27.
;;
;;  6/27/94  Fixed up sas-mode documentation.  
;;           Fixed indentation to work with macros.
;;           Fixed indentation to handle comments better.
;;  
;;  6/7/94 fixed some bugs with menus and buffers and such.  
;;  
;;  Modified 5/3/94 to add function sas-get-dataset and ancillary functions.
;;    (see documentation to sas-get-dataset for details.)
;;  
;;  Modified 5/2/94 to:
;;     1) Split log output and lst output into separate buffers in command
;;        `run-sas-on-region'.
;;     2) Add error check when sas-notify is set to alert the user of errors in
;;        the log file.
;;  
;;  Modified 4/29/94 to add variable `sas-program' whose value is the command which
;;    invokes SAS.  Defaults to \"sas\" but may be set to \"sas609\" for example,
;;    to use sas version 6.09.
;;  
;;  Modified 4/7/94 to add sas-notify.  If variable sas-notify is set, beep and
;;  display message that SAS run is done.
;;  
;;  Modified 1/26/94 to fix a bug in the indentation code.
;;  
;;  Modified 1/19/94 to add variable `sas-require-confirmation'.  If `t', you
;;  will be prompted for confirmation if you switch to a .lst or .log file which
;;  has changed on disk.  If `nil', emacs will automatically visit the disk
;;  version no questions asked and throw away any edits you have made in the
;;  buffer.
;;  
;;  Modified 1/13/94 to list key bindings in menu.
;;  
;;  Modified 1/12/94 to completely remove Riggle's code.  
;;  
;;  Modified 1/10/94 to add sas menu to menu bar.  It contains roughly the
;;  same set of functions available through the C-c bindings.
;;  
;;  The default is 4.


;;  To Do:
;;  
;;  1)  Add feature to automatically check log file for errors.
;;           Done 5/2/94 (at least in part)
;;
;;  2)  Fix indendation to work better with macros.
;;           Done 6/27/94
;;
;;  3)  Spiff up feature to print sas data sets.
;;
;;  4)  Add facility to generate templates for various procs.
;;
;;  5)  Add facility for completion/abbreviations for options, variable names,
;;      etc.
;;
;;  6)  Make point-and-click interface to sas datasets to run sas-get-dataset.
;;
;;  7)  Move documentation from sas-mode defun doc string to info page.(where it
;;      ought to be)
;;   



















;;-*-emacs-lisp-*-
;;;  file name: sas-fontlock.el
;;;
;;;  Version 0.1 (goes with version 1.4 of sas.el, sas-hilit.el, see below)
;;; 
;;;    sas-fontlock:  fontlock SAS programs.
;;;    Copyright (C) 1995 Anthony Rossini
;;;    (used ideas found in sas-hilit.el, Copyright (C) 1994 Tom Cook)
;;;
;;;    This program is free software; you can redistribute it and/or modify
;;;    it under the terms of the GNU General Public License as published by
;;;    the Free Software Foundation; either version 2 of the License, or
;;;    (at your option) any later version.
;;;
;;;    This program is distributed in the hope that it will be useful,
;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;    GNU General Public License for more details.
;;;
;;;    You should have received a copy of the GNU General Public License
;;;    along with this program; if not, write to the Free Software
;;;    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;  Author:   Anthony Rossini
;;;            Center for Biostatistics and Epidemiology
;;;            Penn State College of Medicine
;;;            P.O. Box 850 
;;;            Hershey, PA 17033-0850
;;;            arossini@biostats.hmc.psu.edu
;;   Last change: 2/2/95
;;
;; a line like  (if window-system (require 'sas-hilit))
;;

(require 'font-lock)

;; from Splus
(defvar sas-mode-font-lock-keywords
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
    ("%[a-z0-9_]*\\>" . font-lock-preprocessor-face)))


;; Font-lock support
(make-local-variable 'font-lock-defaults)
(setq font-lock-defaults '(sas-mode-font-lock-keywords))
(add-hook 'sas-mode-hook 'turn-on-font-lock)

;;
;; Font Lock is based on this:
;; (basic patterns by Tom Cook).

;;(hilit-set-mode-patterns
;; 'sas-mode
;; '(("/\\*" "\\*/" comment)
;;   ("^ *\\*[^/]" ";" comment)
;;   ("; *\\*[^/]" ";" comment)
;;   ("%include [^;]*;" nil include)
;;   ("&+[a-z0-9_]*\\>" nil label)
;;   ("^[ \t]*%let[ \t]+\\([a-z0-9_]*\\)" 1 label)
;;   ("\\<\\(array\\|length\\|var\\|class\\)\\>" nil decl)
;;   ("^[ \t]*\\(proc\\|data\\|%macro\\|run\\|%mend\\|endsas\\)[ \t;]" nil defun)
;;   ("\\<\\(retain\\|format\\|input\\|infile\\|by\\|set\\|merge\\|label\\|options\\|where\\|%?if\\|%?then\\|%?else\\|%?while\\|%?do\\|%?until\\|%?end\\|%let\\|%str\\)\\>" nil keyword)
;;   ("^[ \t]*\\(infile\\|proc\\|%macro\\|data\\)\\>[ \t]+\\([a-z0-9_.]*\\)" 2 define)
;;   ("\\b\\(data\\|out\\)\\>[ \t]*=[ \t]*\\([a-z0-9_.]*\\)" 2 define)
;;   ("^[ \t]*\\(set\\|merge\\)[ \t]+\\([^();]*\\)" 2 define)
;;   ("^[ \t]*\\(set\\|merge\\)[ \t]+[a-z0-9_.]*[ \t]*([^)]*)[ \t]*\\([^();]*\\)" 2 define)
;;   ("^[ \t]*\\(set\\|merge\\)[ \t]+[a-z0-9_.]*[ \t]*([^)]*)[ \t]*[a-z0-9_.]*[ \t]*([^)]*)[ \t]*\\([^();]*\\)" 2 define)
;;    
;;   ;;("^[ \t]*\\(set\\|merge\\)\\>\\([^;]*\\);" 2 define)
;;   ;;("\\b\\(set\\|merge\\)\\>[^()]*\\((.*)\\)" 2 default)
;;   ("%[a-z0-9_]*\\>" nil define)
;;   )
;; nil 'case-insensitive)

(provide 'sas-fontlock)
