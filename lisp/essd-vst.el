;;; essd-vst.el --- ViSta customization

;; Copyright (C) 1997 A. J. Rossini

;; Author: A.J. Rossini <rossini@stat.sc.edu>
;; Maintainer: A.J. Rossini <rossini@stat.sc.edu>
;; Created: 26 Aug 1997
;; Modified: $Date: 1997/12/01 16:01:45 $
;; Version: $Revision: 5.0 $
;; RCS: $Id: essd-vst.el,v 5.0 1997/12/01 16:01:45 rossini Exp $
;;
;; Keywords: start up, configuration.

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
;;; This file extends the XLispStat configuration for ViSta.


;;; Autoloads:

(autoload 'inferior-ess "ess-inf" "Run an ESS process")

;;; Code:

(defvar VST-customize-alist
  '((ess-customize-alist           .  VST-customize-alist )
    (ess-language                  .  "XLS"               )
    (ess-dialect                   .  "ViSta"             )
    (ess-loop-timeout              .  10000               )
    (ess-object-name-db-file       .  "ess-xls-namedb.el" )
    (ess-help-sec-regex            .  " ")
    (ess-help-sec-keys-alist       .  " ")
    (inferior-ess-primary-prompt   .  "> ?"               )
    (inferior-ess-program          .  inferior-VST-program-name)
    (inferior-ess-help-command     .  "(help '%s)\n"      )
    (inferior-ess-objects-command  .  "(variables)\n"     )
    (inferior-ess-exit-command     .  "(exit)\n"          )
    (inferior-ess-start-file       . "~/.ess-VST")
    (inferior-ess-start-args       . nil)
    )
  "Variables to customize for XLS")


(defun VST-mode (&optional proc-name)
  "Major mode for editing ViSta source.  NOT EVEN STARTED."
  (interactive)
  (setq ess-customize-alist VST-customize-alist)
  (lisp-mode))


(defun ViSta ()
  "Call 'ViSta', the extend XLispStat statistical system, from Forrest Young."

  (interactive)
  (setq ess-customize-alist VST-customize-alist)
  (ess-write-to-dribble-buffer
   (format "(ViSta): ess-dialect=%s , buf=%s\n"
  	   ess-dialect (current-buffer)))
  (inferior-ess))

 ; Provide package

(provide 'essd-vst)

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
