;;; ess-batch.el --- Emacs functions used by ESS[SAS] and ESS[BUGS]

;; Copyright (C) 2002 R. Sparapani.

;; Author: Rodney Sparapani <rsparapa@mcw.edu>
;; Maintainer: Rodney Sparapani <rsparapa@mcw.edu>
;; Created: 23 Jan 2002
;; Modified: $Date: 2002/01/23 16:41:26 $
;; Version: $Revision: 1.1 $
;; RCS: $Id: ess-batch.el,v 1.1 2002/01/23 16:41:26 rsparapa Exp $

;; This file is part of ESS (Emacs Speaks Statistics).

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

(defun ess-set-file-path ()
 "*Set `ess-sas-file-path' or `ess-bugs-file-path' depending on suffix."
  (interactive)

  (save-match-data (let ((ess-temp-file (expand-file-name (buffer-name))))
    (if (string-match ess-sas-suffix-regexp ess-temp-file) 
	(setq ess-sas-file-path (nth 0 (split-string ess-temp-file "[<]")))
    ;;else
    (if (string-match ess-bugs-suffix-regexp ess-temp-file) 
	(setq ess-bugs-file-path (nth 0 (split-string ess-temp-file "[<]"))))))))

(provide 'ess-batch)
