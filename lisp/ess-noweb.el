;;; ess-noweb.el --- support for Literate Data Analysis

;; Copyright (C) 1999 Mark Lunt
;; Copyright (C) 1999--2004 A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Mark Lunt <mark.lunt@mrc-bsu.cam.ac.uk>
;;          A.J. Rossini <rossini@u.washington.edu>
;; Created: April 18, 1999
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; Keywords: statistics, languages
;; Summary: Noweb support for ESS

;; This file is part of ESS

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

;; Code for ESS and Literate Data Analysis.

;;; Code:

 ; Requires and autoloads

(require 'noweb-mode)
;; still needed when user turns font-lock-mode *on* (from initial off):
(autoload 'noweb-font-lock-mode "noweb-font-lock-mode")

 ; Variables

(defvar ess-noweb-use-font-lock font-lock-mode
  "Set to t if you want to use font-locking in ESS noweb buffers.")

;; this helps with XEmacs barfing, sigh...
;; but is *NOT* okay to do *globally*: (setq global-font-lock-mode t)

(if ess-noweb-use-font-lock
    (require 'noweb-font-lock-mode))

 ; Functions

;;*;; Code Chunk evaluation.

(defun ess-eval-chunk (vis)
  "Tangle the current chunk and send it to the inferior ESS process.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (let ( (process-name ess-local-process-name)
         new-process-name
         (cbuf (current-buffer))
         (temp-buffer (ess-create-temp-buffer "Tangle Buffer")))
    (noweb-tangle-chunk temp-buffer)
    (set-buffer temp-buffer)
    ;; When the temp buffer is created, it does not inherit any value
    ;; of ess-local-process-name from the .Rnw buffer, so we have to set it
    ;; here.  If ess-local-process-name is not set in the .Rnw buffer,
    ;; it will inherit the value that is chosen here.
    (set (make-local-variable 'ess-local-process-name) process-name)
    (ess-eval-region (point-min) (point-max) vis "Eval buffer")
    (if process-name
        (kill-buffer temp-buffer)
      ;; if process-name was nil, source buffer did not have a local process
      ;; so keep value from temp buffer before killing it.
      (setq new-process-name ess-local-process-name)
      (kill-buffer temp-buffer)
      (set-buffer cbuf)
      (set (make-local-variable 'ess-local-process-name) new-process-name))))

(defun ess-eval-chunk-and-go (vis)
  "Tangle the current chunk, send to the ESS process, and go there.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-chunk vis)
  (ess-switch-to-ESS t))

;;*;; Thread code chunk evaluation

;;;
;;; Threads are code chunks which fit into the same "buffer" (I'm (AJR)
;;; abusing terminology, but what I mean is things like:
;;; <<thing1>>=
;;;   code for thing1
;;; @
;;; Documentation
;;; <<thing1>>=
;;;   continuing code for thing1
;;; @
;;;

(defun ess-eval-thread (vis)
  "Tangle all chunks in the current chunk-thread and send to the ESS process.
Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (let ((temp-buffer (ess-create-temp-buffer "Tangle Buffer")))
    (noweb-tangle-current-thread temp-buffer)
    (set-buffer temp-buffer)
    (ess-eval-region (point-min) (point-max) vis "Eval buffer")
    (kill-buffer temp-buffer)))

(defun ess-eval-thread-and-go (vis)
  "Tangle all chunks in the current chunk-thread, send to ESS process,
and go there.  Arg has same meaning as for `ess-eval-region'."
  (interactive "P")
  (ess-eval-thread vis)
  (ess-switch-to-ESS t))

 ; Provide package

(provide 'ess-noweb)

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

;;; ess-noweb.el ends here
