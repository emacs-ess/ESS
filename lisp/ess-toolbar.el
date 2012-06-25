;;; ess-toolbar.el --- Support for a toolbar in ESS.

;; Copyright (C) 1997--2009 A.J. Rossini, Rich M. Heiberger, Martin
;;      Maechler, Kurt Hornik, Rodney Sparapani, and Stephen Eglen.

;; Author: Stephen Eglen
;; Created: 2004-05-06
;; Revised: 2009-03-16
;; Maintainer: ESS-core <ESS-core@r-project.org>

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

;; This code adds a toolbar to ESS modes for editing R and S code.
;; Support can be added for other modes (e.g. STATA), just ask!
;;
;; This code is experimental, and runs best on Emacs 21 and XEmacs
;; 21.  It has been tested only on Linux machines.  All feedback
;; appreciated.
;;
;; If your emacs can support images, the ESS toolbar should be loaded.
;;
;; If you see a toolbar, but no icons, check out the value of
;; ess-icon-directory.
;;
;; The toolbar can be customized in several ways.  To see options, do:
;; M-x customize-group RET ess-toolbar RET
;; If you change any of the variables, you _may_ need to restart Emacs
;; to see any effect.  See also the documentation for ess-toolbar-items
;; if you wish to change its value.

;;; Technical issues.

;; Emacs vs XEmacs.
;; Of course, Emacs and XEmacs have different interfaces and handle
;; the toolbars in different ways.  The code here is rough, but
;; hopefully soon a compatibility toolbar library will be released
;; that will make the toolbar code more portable.  So, for now the
;; code should be regarded as proof of concept.

;; 2009-03-16: toolbar code in Emacs 23 has changed slightly to 22,
;; and presumably once Emacs 22 is no longer supported, this code can
;; be cleaned up a bit (i.e. no need to set load-path.)

;;; Code:

(defgroup ess-toolbar nil
  "ESS: toolbar support."
  :group 'ess
  :link '(emacs-commentary-link :tag "Commentary" "ess-toolbar.el")
  :prefix "ess-")

(defcustom ess-use-toolbar
  (if (featurep 'xemacs)
      (memq (device-type) '(x gtk mswindows))
    (and (fboundp 'display-images-p) (display-images-p)))
  "*Non-nil means ESS should support the toolbar.
Currently works only under Emacs 21 and maybe XEmacs 21.4."
  :group 'ess-toolbar
  :type 'boolean)


(defcustom ess-toolbar-own-icons nil
  "*Non-nil means that we only put our toolbar entries in ESS.
Otherwise we get standard toolbar as well as ESS entries.
Under Emacs, the standard toolbar items are copied from the default toolbar.
Under XEmacs, the items stored in `ess-toolbar-xemacs-general' are added."
  :group 'ess-toolbar
  :type 'boolean)

(defcustom ess-toolbar-global nil
  "*Non-nil means that the ESS toolbar is available in all emacs buffers.
Otherwise, the ESS toolbar is present only in R/S mode buffers.
For beginners, this is probably better set to a non-nil value."
  :group 'ess-toolbar
  :type 'boolean)

(defcustom ess-toolbar-items
  '( (R   "startr"  "Start R process")
     ;;(S   "spluslogo" "Start S process")
     (S   "splus_letter_small" "Start S process")
     (ess-eval-line-and-step   "rline" "Eval line & step")
     (ess-eval-region   "rregion" "Eval region")
     (ess-eval-function-or-paragraph-and-step "rregion"
                                              "Eval function or paragraph and step")
     (ess-load-file   "rbuffer" "Load file")
     (ess-eval-function   "rfunction" "Eval function")
     (ess-switch-to-ESS   "switch_ess" "Switch to ESS buffer"))
  "Items to be added to the ESS toolbar.
Each list element has three items:
1. the name of the function to run
2. the icon to be used (without .xpm extension)
3. the tooltip doc string (XEmacs only; Emacs gets doc string from menu items.

General toolbar items are also added to the ESS toolbar
iff `ess-toolbar-own-icons' is nil.

Setting this variable with setq doesn't take effect once you have
loaded ess-site, unless you follow it by a call to
`ess-make-toolbar' afterwards.  Instead, change its value using
Custom, and then on all new ESS buffers you should see the
toolbar has changed."
  :group 'ess-toolbar
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (fboundp 'ess-make-toolbar)
             (ess-make-toolbar)))
  :type '(repeat (list (function :tag "Function to run")
                       (string  :tag "Icon")
                       (string  :tag "Tooltip"))))

(defvar ess-icon-directory
  (expand-file-name (concat (file-name-as-directory ess-etc-directory) "icons"))
  "*Location for ESS icons.
This variable should be set automatically by the ESS install process.
Icons should be found in ESS/etc/icons/ directory.
If `ess-icon-directory' is invalid, please report a bug.")

(unless (file-directory-p ess-icon-directory)
  (ess-write-to-dribble-buffer
   "`ess-icon-directory' does not exist; using `ess-etc-directory'.\n")
  (setq ess-icon-directory ess-etc-directory))

(defvar ess-toolbar nil
  "Toolbar items to be added to ESS editing buffers.")

(defun ess-make-toolbar ()
  "Make the ESS toolbar."
  (if (featurep 'xemacs)
      (ess-make-toolbar-xemacs)
    ;; Under Emacs, only worth building the toolbar if tool-bar-map is
    ;; available.  e.g. when running Emacs within a terminal, tool-bar-map
    ;; is not available, so no need to make the tool-bar.
    (if (boundp 'tool-bar-map)
        (ess-make-toolbar-emacs))))

(defun ess-make-toolbar-emacs ()
  "Make the ESS toolbar under Emacs."
  (setq ess-toolbar
        (if (or ess-toolbar-own-icons (null tool-bar-map))
            (make-sparse-keymap)
          (copy-keymap tool-bar-map)))
  (let ((tool-bar-map ess-toolbar)
        (load-path (list ess-icon-directory)))
    ;; in Emacs 22, icons are found by examining load-path, bound here
    ;; whereas Emacs 23 seems to want them in image-load-path, set at the
    ;; bottom of this file.
    (mapc 'ess-add-icon-emacs ess-toolbar-items)))

(defun ess-add-icon-emacs (x)
  "Add an ESS item to the Emacs toolbar."
  ;; By using tool-bar-add-item-from-menu instead of tool-bar-add-item
  ;; we get the tooltips "for free" from ess-mode-map.
  (tool-bar-add-item-from-menu (car x) (cadr x) ess-mode-map))

(defun ess-add-icon-xemacs (x)
  "Return a 4-vector containing the spec for an ESS toolbar entry in XEmacs."
  (vector
   (toolbar-make-button-list
    (expand-file-name (concat (cadr x) ".xpm") ess-icon-directory))
   (car x)                              ;function
   t
   (nth 2 x)                            ;doc string
   ))

(defvar ess-toolbar-xemacs-general
  (list
   [toolbar-file-icon toolbar-open t "Open a file"]
   [toolbar-disk-icon toolbar-save t "Save buffer"]
   [toolbar-printer-icon generic-print-buffer t "Print buffer"]
   [toolbar-cut-icon toolbar-cut t "Kill region"]
   [toolbar-copy-icon toolbar-copy t "Copy region"]
   [toolbar-paste-icon toolbar-paste t "Paste from clipboard"]
   [toolbar-undo-icon toolbar-undo t "Undo edit"]
   [toolbar-replace-icon toolbar-replace t "Search & Replace"]
   [:style 3d]
   )
  "General Xemacs icons to be added iff `ess-toolbar-own-icons' is non-nil.
These toolbar items were taken from the list that John Fox's code provided.
Each vector is of length four specifying: 1 - icon; 2 - function to call;
3 - whether to activate; 4 - doc string.")

(defun ess-make-toolbar-xemacs ()
  "Set up the ESS toolbar for XEmacs."
  (setq ess-toolbar
        (append (if ess-toolbar-own-icons nil ess-toolbar-xemacs-general)
                (mapcar 'ess-add-icon-xemacs ess-toolbar-items)))
  )

(defun ess-add-toolbar ()
  "Add the ESS toolbar to a particular mode.
The toolbar is added iff `ess-toolbar-global' is nil, else the toolbar
is added globally when ess-toolbar.el is loaded."
  (if (and ess-toolbar (not ess-toolbar-global))
      (if (featurep 'xemacs)
          (set-specifier  default-toolbar ess-toolbar (current-buffer))
        ;; Support for Emacs
        (set (make-local-variable 'tool-bar-map) ess-toolbar))))

;; Make the toolbars.  Each toolbar is hopefully made only when this file
;; is loaded; we don't need it to be remade every time.
(if ess-use-toolbar
    (progn
      (ess-make-toolbar)
      ;; After making the toolbar, if ESS toolbar is needed globally,
      ;; add it here.
      (if ess-toolbar-global
          (if (featurep 'xemacs)
              ;; Xemacs
              (progn
                (set-specifier  default-toolbar ess-toolbar)
                (ess-write-to-dribble-buffer "Creating global XEmacs toolbar"))
            ;; Emacs
            (setq tool-bar-map ess-toolbar)
            (ess-write-to-dribble-buffer "Creating global Emacs toolbar"))
        )

      ;; Check for toolbar support - needed iff ess-use-toolbar is non-nil.
      (or
       ;; XEmacs test for image support, adapted from vm-version.el:
       (and (featurep 'xemacs) (memq (device-type) '(x gtk mswindows)))
       ;;
       ;; Emacs support for images:
       (and (fboundp 'display-images-p) (display-images-p))
       ;; if above tests failed, give a warning.
       (progn
         (message "Toolbar support for ESS not available in this emacs.")
         ;; Not sure if we want to delay startup of ESS.
         ;;(sit-for 2)
         ))
      ))

;; Following needed for Emacs 23, not Emacs 22 (nor XEmacs).
(when (boundp 'image-load-path)
  (add-to-list 'image-load-path ess-icon-directory))

(provide 'ess-toolbar)

;;; ess-toolbar.el ends here
