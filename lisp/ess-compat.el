;;; ess-compat.el --- simple determination of Emacs and version #.

;; Copyright (C) 2000--2017 A.J. Rossini, Richard M. Heiberger, Martin
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
;; version of Emacs that we are using.

;;; Code:

;; FIXME:  When Emacs is started from Cygwin shell in Windows,
;;         we have (equal window-system 'x) -and should use "--ess" in *d-r.el
(defvar ess-microsoft-p (memq system-type '(ms-dos windows-nt))
  "Value is t if the OS is one of Microsoft's, nil otherwise.")


(defun ess-sleep ()
  ;; FIXME: Not a "compatibility" thing any more, so move to ess-utils.el.
  "Put emacs to sleep for `ess-sleep-for-shell' seconds (floats work)."
  (sleep-for ess-sleep-for-shell))

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

;;; xemacs process-put and process-get workarounds:
;;; !!!! remove this when xemacs starts supporting them!!!
(when (featurep 'xemacs)
  (defvar process-plist-map (make-hash-table :test 'eq :weakness 'key)
    "Property list information for process, when XEmacs doesn't provide this.
See `process-plist' and `set-process-plist'.")

  (defun-when-void process-plist (process)
    "Return the property list of PROCESS."
    (check-argument-type #'processp process)
    (gethash process process-plist-map))

  (defun-when-void set-process-plist (process plist)
    "Set the property list of PROCESS to PLIST."
    (check-argument-type #'processp process)
    (check-argument-type #'valid-plist-p plist)
    (puthash process plist process-plist-map))


  (defun-when-void process-get (process propname)
    "Return the value of PROCESS' PROPNAME property.
This is the last value stored with `(process-put PROCESS PROPNAME VALUE)'."
    (plist-get (process-plist process) propname))

  (defun-when-void process-put (process propname value)
    "Change PROCESS' PROPNAME property to VALUE.
It can be retrieved with `(process-get PROCESS PROPNAME)'."
    (set-process-plist process
                       (plist-put (process-plist process) propname value)))
  )

(defun ess-mode-xemacs-menu ()
  "Hook to install `ess-mode' menu for XEmacs (w/ easymenu)."
  (if 'ess-mode
      (easy-menu-add ess-mode-menu)
    (easy-menu-remove ess-mode-menu)))

(when (featurep 'xemacs)
  (add-hook 'ess-mode-hook 'ess-mode-xemacs-menu))

(when (featurep 'xemacs) ;; work around Xemacs bug (\C-\M-h redefines M-BS):
  (eval-after-load "ess-mode"
    '(define-key ess-mode-map [(meta backspace)] 'backward-kill-word)))

(provide 'ess-compat)

 ; Local variables section

;; This file is automatically placed in Outline minor mode.
;; The file is structured as follows:
;; Chapters:     ^L ;
;; Sections:    ;;*;;
;; Subsections: ;;;*;;;
;; Components:  defuns, defvars, defconsts
;;              Random code beginning with a ;;;;* comment
;; Local variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;; End:

;;; ess-compat.el ends here
