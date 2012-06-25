;;; ess-sp5-d.el --- S-plus 5  customization

;; Copyright (C) 1998 A.J. Rossini
;; Copyright (C) 1999--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 9 Nov 1998
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: languages

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

;; AJR copied S4 to be S+5.
;; DB contributed the changes from ess-sp3-d.el to
;; ess-s4-d.el. (removed the old ugly approach).
;; This file defines Sp5 customizations for ess-mode.  Lots of thanks
;; to RMH and JMC for code and suggestions
;; Thanks to MM for making this sensible.

;;; Code:

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(require 'ess-s-l)

;; You now need to make sure you've defined if you are running 5.0 or 5.1.
;; Lots of things are broken between them, GRR...

(defvar S+5-dialect-name "S+5"
  "Name of 'dialect' for S-PLUS 5.");easily changeable in a user's .emacs

(defvar S+5-customize-alist
  (append
   '((ess-local-customize-alist         . 'S+5-customize-alist)
     (ess-dialect                       . S+5-dialect-name)
     (ess-loop-timeout                  . ess-S-loop-timeout);fixme: dialect spec.
     (ess-object-name-db-file           . "ess-sp5-namedb.el")
     (inferior-ess-program              . inferior-S+5-program-name)
     ;;(inferior-ess-objects-pattern    . ".*") ; for new s4 stuff
     (inferior-ess-help-command   . "help(\"%s\",pager=\"slynx -dump\",window=F)\n")
     (inferior-ess-help-filetype . nil)
     (inferior-ess-search-list-command  . "searchPaths()\n")
     (inferior-ess-start-args      . inferior-Splus-args)
     (ess-STERM  . "iESS")
     )
   S+common-cust-alist)

  "Variables to customize for S+5.")


;; For loading up the S code required for the above.
;;(add-hook 'ess-post-run-hook
;;         (lambda ()
;;           (ess-command
;;            (concat
;;             "if(exists(\"Sversion\")) library(emacs) else source(\""
;;             ess-mode-run-file
;;             "\")\n"))
;;           (if ess-mode-run-file2
;;               (ess-command
;;                (concat "source(\"" ess-mode-run-file2 "\")\n")))))


(defun S+5 (&optional proc-name)
  "Call 'Splus5', based on S version 4, from Bell Labs.
New way to do it."
  (interactive)
  (setq ess-customize-alist S+5-customize-alist)
  (ess-write-to-dribble-buffer
   (format "\n(S+5): ess-dialect=%s, buf=%s\n" ess-dialect (current-buffer)))
  (inferior-ess)
  (if inferior-ess-language-start
      (ess-eval-linewise inferior-ess-language-start)))

(defun S+5-mode (&optional proc-name)
  "Major mode for editing S+5 source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist S+5-customize-alist)
  (ess-mode S+5-customize-alist proc-name)
  (if ess-imenu-use-S (ess-imenu-S)))

(defun S+5-transcript-mode ()
  "S-PLUS 5 transcript mode."
  (interactive)
  (ess-transcript-mode S+5-customize-alist))

 ; Provide package

(provide 'ess-sp5-d)

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

;;; ess-sp5-d.el ends here
