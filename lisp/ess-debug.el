;;; ess-debug.el --- debugging start up for ESS

;; Copyright (C) 1993 David M. Smith

;; Author: David Smith <D.M.Smith@lancaster.ac.uk>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 12 Nov 1993
;; Modified: $Date: 1997/10/22 19:39:34 $
;; Version: $Revision: 1.3 $
;; RCS: $Id: ess-debug.el,v 1.3 1997/10/22 19:39:34 rossini Exp $
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

;;;
;;; Commentary:  Strictly for debugging and development.
;;;

(defun ess-add-path (path &rest options)
  "Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `defaul-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'.

For ESS, ONLY use load-path, since Emacs doesn't have
default-load-path."

  (let ((rest load-path)
	p)
    (if (and (catch 'tag
	       (while rest
		 (setq p (expand-file-name path (car rest)))
		 (if (file-directory-p p)
		     (throw 'tag p))
		 (setq rest (cdr rest))))
	     (not (member p load-path)))
	(setq load-path
	      (if (memq 'append options)
		  (append load-path (list p))
		(cons p load-path))))))

(setq-default debug-on-error t)
(ess-add-path "/p1/apps/X11R6.3/lib/xemacs/site-lisp/ESS/")
(require 'ess-site)
