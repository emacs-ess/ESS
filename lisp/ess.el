;;; ess.el --- Emacs Speaks Statistics: statistical programming within Emacs

;; Copyright (C) 1989--1996 Bates, Kademan, Ritter and Smith
;; Copyright (C) 1997--2010 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.
;; Copyright (C) 2011--2015 A.J. Rossini, Richard M. Heiberger, Martin Maechler,
;;      Kurt Hornik, Rodney Sparapani, Stephen Eglen and Vitalie Spinu.

;; Author: Doug Bates
;;     Ed Kademan
;;     Frank Ritter
;;     David Smith
;; Created: October 14, 1991
;; Maintainer: ESS-core <ESS-core@r-project.org>
;; Keywords: statistics, languages

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
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; PURPOSE
;;
;; Interface to the S, SAS, and XLisp dialects of statistical
;; programming languages, with potential extensions to other
;; languages.   Designed to be extendable to most other interactive
;; statistical programming situations.

;; BRIEF OVERVIEW
;;
;; Supports structured editing of S, SAS, and XLisp (statistics
;; programming languages) functions that are integrated with a
;; running process in a buffer.

;; THE ESS MAILING LIST
;;
;; There is an informal mailing list for discussions of ESS. Alpha
;; and beta releases of ESS are also announced here. Send mail
;; to ess-help-request@r-project.org to join.

;; OVERVIEW OF ESS
;;
;; S is a statistics programming language developed at Bell Labs
;; particularly suited for descriptive and exploratory statistics.
;; s-mode is built on top of comint (the general command interpreter
;; mode written by Olin Shivers), and so comint.el (or comint.elc)
;; should be either loaded or in your load path when you invoke it.
;;
;; Aside from the general features offered by comint such as
;; command history editing and job control, inferior S mode
;; allows you to dump and load S objects into and from external
;; files, and to display help on functions.  It also provides
;; name completion while you do these.  For more detailed
;; information see the documentation strings for inferior-ess,
;; inferior-ess-mode, ess-mode, and comint-mode.  There are also
;; many variables and hooks available for customizing (see
;; the variables below that have document strings that start
;; with an "*").

;; INSTALLATION
;; See README and S-site for details.

;; GETTING RELEASES OF ESS
;; ===> http://ess.r-project.org
;;

;; CREDITS.
;; Thanks to shiba@shun.isac.co.jp (Ken'ichi "Modal" Shibayama) for
;;   the indenting code.
;; Thanks also to maechler@stat.math.ethz.ch (Martin Maechler) for
;;   suggestions and bug fixes.
;; ess-eval-line-and-step is based on a function by Rod Ball
;;   (rod@marcam.dsir.govt.nz)
;; Also thanks from David Smith to the previous authors for all their
;; help and suggestions.
;; And thanks from Richard M. Heiberger, Kurt Hornik, Martin
;; Maechler, and A.J. Rossini to David Smith.

;; BUG REPORTS
;; Please report bugs to ess-bugs@r-project.org
;; Comments, suggestions, words of praise and large cash donations
;; are also more than welcome, but should generally be split between
;; all authors :-).

;;; Code:

;;*;; Requires and autoloads
;;;=====================================================
;;;

(defun ess-write-to-dribble-buffer (text)
  "Write TEXT to dribble ('*ESS*') buffer."
  (unless (buffer-live-p ess-dribble-buffer)
    ;; ESS dribble buffer must be re-created.
    (setq ess-dribble-buffer (get-buffer-create "*ESS*")))
  (let (deactivate-mark)
    (with-current-buffer ess-dribble-buffer
      (goto-char (point-max))
      (insert-before-markers text))))

;; Shortcut to render "dribbling" statements less cluttering:
(defun ess-if-verbose-write (text)
  "Write TEXT to dribble buffer ('*ESS*') only *if* `ess-verbose'."
  (if ess-verbose (ess-write-to-dribble-buffer text)))


(require 'easymenu)
(if (or window-system
        noninteractive ; compilation!
        )
    (require 'font-lock))

(require 'ess-compat)
(require 'ess-custom)
(require 'ess-mode)
(require 'ess-inf)
;; We can't use cl-lib whilst supporting Emacs <= 24.2 users
(with-no-warnings (require 'cl))



 ; ess-mode: editing S/R/XLS/SAS source

(autoload 'inferior-ess "ess-inf"
  "Run [inferior-ess-program], an ess process, in an Emacs buffer" t)

(autoload 'ess-dump-object-into-edit-buffer "ess-mode"
  "Edit an S object." t)

(autoload 'ess-parse-errors "ess-mode"
  "Jump to the last error generated from a sourced file." t)

(autoload 'ess-load-file "ess-inf" "Source a file into S.")

(autoload 'ess-inside-string-or-comment-p "ess-utils"
  "Non-nil, if inside string or comment." t)
(autoload 'ess-inside-string-p "ess-utils" "Non-nil if inside string." t)

(autoload 'ess-do-auto-fill "ess-utils" "substitute for \\[do-auto-fill]." t)

(autoload 'ess-rep-regexp "ess-utils" "Replace, but not in string/comment." t)

(autoload 'ess-time-string "ess-utils" "Return time-stamp string." t)

(autoload 'ess-nuke-trailing-whitespace "ess-utils"
  "Maybe get rid of trailing blanks." t)

 ; ess-transcript-mode: editing ``outputs'

(autoload 'ess-transcript-mode "ess-trns"
  "Major mode for editing S transcript files." t)

(autoload 'ess-display-help-on-object "ess-help"
  "Display help on an S object." t)

(defalias 'ess-help 'ess-display-help-on-object)

(autoload 'ess-goto-info "ess-help"
  "Jump to the relevant section in the `ess-mode' manual." t)

(autoload 'ess-submit-bug-report "ess-help"
  "Submit a bug report on the `ess-mode' package." t)

;;==> ess-inf.el  has its OWN autoload's !


(run-hooks 'ess-mode-load-hook)

(provide 'ess)

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

;;; ess.el ends here
