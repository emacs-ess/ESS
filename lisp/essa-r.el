;;; essa-r.el -- Possible local customizations for R with ESS.

;; Copyright (C) 1997--2000	A.J. Rossini, Martin Maechler,
;;				Kurt Hornik, and Richard M. Heiberger.

;; Author: A.J. Rossini <rossini@biostat.washington.edu>
;; Maintainer: A.J. Rossini <rossini@biostat.washington.edu>
;; Created: 17 November 1999
;; Modified: $Date: 2000/10/17 18:19:32 $
;; Version: $Revision: 1.4 $
;; RCS: $Id: essa-r.el,v 1.4 2000/10/17 18:19:32 rossini Exp $

;; Keywords: editing and process modes.

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
;;
;; In short: you may use this code any way you like, as long as you
;; don't charge money for it, remove this notice, or hold anyone liable
;; for its results.

;;; Code:

;; you can invoke ESS/R from emacs by typing
;;      C-u M-x essr
;; with vsize set to (for example) 40M, and nsize set to 600000.
(defalias 'essr
  (read-kbd-macro
     "C-u M-x R RET - - vsize SPC 40M SPC - - nsize SPC 600000 2*RET"))
;; "SPC" must be "=" in future versions of R (works from 0.99 on)


 ; Provide package

(provide 'essa-r)

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

;;; ess-site.el ends here
