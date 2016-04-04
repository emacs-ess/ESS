;;; ess-old-contrib.el --- Various utils contributed over the years

;; Copyright (C) 1997--2012 A.J. Rossini, Richard M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Created: 1 April 2016
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

;;; Code:

;; This is thanks to  Ed L Cashin <ecashin@uga.edu>, 03 Mar 2004 :
(defun ess-restore-asm-extns ()
  "Remove the S-Plus mode association for .s and .S files added by ESS.
Putting the following in ~/.emacs restores emacs' default association
between .s or .S files and assembly mode.

  (add-hook 'ess-mode-hook 'ess-restore-asm-extns)
  (add-hook 'inferior-ess-mode-hook 'ess-restore-asm-extns)"
  (interactive)
  (when (assoc "\\.[qsS]\\'" auto-mode-alist)
    (setq auto-mode-alist
          (remassoc "\\.[qsS]\\'" auto-mode-alist))
    ;; put .q extention back
    ;; (add-to-list is in xemacs and GNU emacs)
    ;; R-mode when in a R/ subdirectory, otherwise S-mode:
    (add-to-list 'auto-mode-alist '("/R/.*\\.q\\'" . R-mode))
    (add-to-list 'auto-mode-alist '("\\.q\\'" . S-mode))
    ))


;; From: Sebastian Luque <spluque@gmail.com>
;; To: ess-help@stat.math.ethz.ch
;; Date: Mon, 01 May 2006 19:17:49 -0500

;; Without knowing how to tell R to use w3m from within Emacs, and after
;; switching to Konqueror's window for the millionth time, I wrote the
;; following function:

;; This emulates some of the functionality of RSiteSearch() and tests ok in
;; my system GNU Emacs 22.0.50.1 (i486-pc-linux-gnu, X toolkit, Xaw3d scroll
;; bars) of 2006-04-27 on pacem, modified by Debian.  This has the benefit of
;; displaying results with whatever you've told browse-url to use; in my
;; case, w3m with the emacs-w3m package.

;; My elisp skills are rather poor, so comments and suggestions for
;; improvement are welcome.
;; --
;; Seb

;; MM _FIXME_: This only works correctly for  Emacs 22.0.50 (alpha)
;;             for 21.x it has problems in the (completing-read-multiple .)
;;             at the end
(defun R-site-search (string)
  "Search the R archives for STRING, using default criteria.  If
called with a prefix, options are available for
  1) matches per page,
  2) sections of the archives to search (separated by value of `crm-default-separator'),
  3) for displaying results in long or short formats, and
  4) for sorting by any given field.
Completion is available for supplying options."
  (interactive "sSearch string: ")
  (let ((site "http://search.r-project.org/cgi-bin/namazu.cgi?query=")
        (okstring (replace-regexp-in-string " +" "+" string)))
    (if current-prefix-arg
        (let ((mpp (concat
                    "&max="
                    (completing-read
                     "Matches per page: "
                     '(("20" 1) ("30" 2) ("40" 3) ("50" 4) ("100" 5)))))
              (format (concat
                       "&result="
                       (completing-read
                        "Format: " '("normal" "short")
                        nil t "normal" nil "normal")))
              (sortby (concat
                       "&sort="
                       (completing-read
                        "Sort by: "
                        '(("score" 1) ("date:late" 2) ("date:early" 3)
                          ("field:subject:ascending" 4)
                          ("field:subject:decending" 5)
                          ("field:from:ascending" 6) ("field:from:decending" 7)
                          ("field:size:ascending" 8) ("field:size:decending" 9))
                        nil t "score" nil "score")))
              (restrict (concat
                         "&idxname="
                         (mapconcat
                          'identity
                          (completing-read-multiple
                           "Limit search to: "
                           '(("Rhelp02a" 1) ("functions" 2) ("docs" 3)
                             ("Rhelp01" 4))
                           nil t "Rhelp02a,functions,docs" nil
                           "Rhelp02a,functions,docs") "&idxname="))))
          (browse-url (concat site okstring mpp format sortby restrict)))
      ;; else: without prefix use defaults:
      (browse-url (concat site okstring "&max=20&result=normal&sort=score"
                          "&idxname=Rhelp02a&idxname=functions&idxname=docs")))))

(provide 'ess-old-contrib)

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

;;; ess-old-contrib.el ends here
