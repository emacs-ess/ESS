;;; essd-r32.el --- R customization for Windows 9x/NT
;;; Richard M. Heiberger, February 1999

;; Copyright (C) 1999 Richard M. Heiberger <rmh@fisher.stat.temple.edu>

;; Author: Richard M. Heiberger <rmh@fisher.stat.temple.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: February 1999
;; Modified: $Date: 
;; Version: $Revision: 
;; RCS: $Id: essd-r32.el
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

;;; Requires and Autoloads:

(require 'essd-r)

;;; ess-site.el
;;;
;;; Windows 95/98/NT
;;; If your PATH includes the R bin directory, then no changes are needed here.
;;; If not, then something like this is needed:
;;(setq-default inferior-R-program-name "c:/Progra~1/R/rw0632/bin/Rterm.exe")
(setq-default inferior-R-program-name "c:/Progra~1/R/rw0632/bin/Rterm.exe")
;;;
;;; The graphics window is a secondary window and does not
;;; automatically get focus unless locator is called.  Even then the
;;; window sometimes cannot be moved or resized.  It is necessary to
;;; move and resize both the emacs frame and the R Graphics window.


;;; ess-vars.el
(defvar inferior-R-program-name
  (if (or (equal window-system 'w32) (equal window-system 'win32))
      "Rterm.exe"
    "R")
  "*Program name for invoking an inferior ESS with R().")
;;;



;;; essd-r.el
(defun R (&optional start-args)
  "Call 'R', the GNU 'S clone' from Robert & Ross (Auckland, NZ)."
  (interactive "P")
  (setq ess-customize-alist R-customize-alist)
  ;; for debugging only
  (ess-write-to-dribble-buffer
   (format 
    "\n(R): ess-dialect=%s, buf=%s, start-arg=%s\n\t current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args
    current-prefix-arg))
  (let* ((r-always-arg
	  (if (or (equal window-system 'w32) (equal window-system 'win32))
	      "--ess "  "--no-readline "))
	 (r-start-args 
	  (concat r-always-arg
		  (if start-args
		   (read-string
		   (concat "Starting Args [other than `" r-always-arg "'] ? "))
		    nil))))
    (inferior-ess r-start-args))
    (if (or (equal window-system 'w32) (equal window-system 'win32))
	(progn
	  (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)
	  (comint-strip-ctrl-m) ;;; Timing problem in bash.
                                ;;; Can't make startup ^M go away.
	  (goto-char (point-max))
	  (beginning-of-line)
	  (insert
"The interaction of ESS 5.1.2 and R 0.63.2 Beta is slightly rough:\n
To start the graphics window, you must explicitly use the `x11()' command.\n
To see the graphics window, you must use the `locator()' command and
then click on the graphics window.\n
The `system(\"command\")' doesn't work when bash is the emacs shell.\n
You must quit R with `q()' or you take the risk of not being able
to shut down the computer cleanly.\n\n")
	  (goto-char (point-max)))
))
