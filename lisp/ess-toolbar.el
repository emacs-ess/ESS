;;; ess-toolbar.el --- initial support for an ESS toolbar.
;;; Thu 06 May 2004
;;; Stephen Eglen
;;; GPL.

;;; Notes.


;;; Emacs vs XEmacs.

;; Of course, Emacs and XEmacs have different interfaces and handle
;; the toolbars in different ways.  The code here is rough, but
;; hopefully soon a compatibility toolbar library will be released
;; that will make the toolbar code more portable.  So, for now the
;; code should be regarded as proof of concept.


;;; Creating pixmaps:
;; Need to add backgrounToolBarColor for Xemacs to show okay.
;;/usr/share/xemacs-21.4.12/etc/toolbar/folder-cap-up.xpm
;; has header:
;;"X	c Gray75 s backgroundToolBarColor",
;; whereas I have set "c None" to indicate the background pixel; this line 
;; seems to work for both toolbars.
;;;". c None s backgroundToolBarColor",



(defvar ess-use-toolbar (fboundp 'tool-bar-add-item)
  "*Non-nil means we should support the toolbar.
Currently works only under Emacs 21 and maybe XEmacs 21.4.")

(defvar ess-own-toolbar nil
  "*Non-nil means that we only put our toolbar entries in ESS.
Otherwise we get standard toolbar as well as ESS entries.
It might be better for the user if we have standard toolbar entries too,
e.g. for saving/loading files.")

(defvar ess-icon-directory nil
  "Location for ESS icons.")

;; Emacs has the ability to clear the cache.
;; (clear-image-cache)


(defvar ess-toolbar nil
  "Emacs toolbar for ESS.")

(defun ess-make-toolbar-r ()
  "Make the toolbar for R."
  (if (featurep 'xemacs)
      (ess-make-toolbar-r-xemacs)
    (ess-make-toolbar-r-emacs)))

(defun ess-make-toolbar-r-emacs ()
  "Make the toolbar for R under Emacs."
  (setq ess-toolbar 
	(if ess-own-toolbar
	    (make-sparse-keymap)
	  (copy-keymap tool-bar-map)))
  (let ((tool-bar-map ess-toolbar)
	(load-path (list ess-icon-directory)))

    ;; icons are found by examining load-path; hence by temporarily setting
    ;; load-path to our icons directory, they will be found.
    (tool-bar-add-item "startr" 'R 'Start_R_process
		       :help "Start R process" )
    (tool-bar-add-item-from-menu 'ess-eval-line-and-step "rline" ess-mode-map)
    (tool-bar-add-item-from-menu 'ess-eval-region "rregion" ess-mode-map)
    (tool-bar-add-item-from-menu 'ess-eval-buffer "rbuffer" ess-mode-map)
    (tool-bar-add-item-from-menu 'ess-eval-function "rfunction" ess-mode-map)
    (tool-bar-add-item-from-menu 'ess-switch-to-ESS "switchr" ess-mode-map)
    ))


;; Suggest adding "Start R" from the menu.  "Start process" to be
;; generic.
;; (ess-start-process) could then call the relevant function to start R/S etc.
;; easy for R, but not sure about other modes.  Could have a completion list.


(defun ess-make-toolbar-r-xemacs ()
  "Set up the R toolbar for XEmacs."

  (defvar toolbar-send-line-icon
    (toolbar-make-button-list
     (expand-file-name "rline.xpm" ess-icon-directory))
    "Send line to R")

  (defvar toolbar-r-icon
    (toolbar-make-button-list
     (expand-file-name "startr.xpm" ess-icon-directory))
    "Start R")
  
  (defvar toolbar-switch-ess-icon
    (toolbar-make-button-list
     (expand-file-name "switchr.xpm" ess-icon-directory))
    "Switch to ESS")
  
  (defvar toolbar-send-para-icon
    (toolbar-make-button-list
     (expand-file-name "para.xpm" ess-icon-directory))
    "Send paragraph to R")
  
  (defvar toolbar-send-function-icon
    (toolbar-make-button-list
     (expand-file-name "rfunction.xpm" ess-icon-directory))
    "Send function to R")
  
  (defvar toolbar-send-reg-icon
    (toolbar-make-button-list
     (expand-file-name "rregion.xpm" ess-icon-directory))
    "Send region to R")
  
  (defvar toolbar-source-icon
    (toolbar-make-button-list
     (expand-file-name "rbuffer.xpm" ess-icon-directory)))
  
  ;; the following is a modified version of ess-eval-line-and-go
  
  ;; make the toolbar
  
  (if (or t ess-use-toolbar)
      (set-specifier 
       default-toolbar 
       '(
	 [toolbar-file-icon toolbar-open t "Open a file"] 
	 [toolbar-disk-icon toolbar-save t "Save buffer"] 
	 [toolbar-printer-icon generic-print-buffer t "Print buffer"] ; toolbar-print doesn't work properly    
	 [toolbar-cut-icon toolbar-cut t "Kill region"] 
	 [toolbar-copy-icon toolbar-copy t "Copy region"] 
	 [toolbar-paste-icon toolbar-paste t "Paste from clipboard"] 
	 [toolbar-undo-icon toolbar-undo t "Undo edit"] 
	 [toolbar-replace-icon toolbar-replace t "Search & Replace"] 
	 [:style 3d]
	 [toolbar-r-icon R t "Start R"]
	 [toolbar-send-line-icon ess-eval-line-and-step t "Send line to R"]
	 [toolbar-send-reg-icon ess-eval-region t "Send region to R"]
	 [toolbar-source-icon ess-eval-buffer t "Source buffer to R"]
	 [toolbar-send-function-icon ess-eval-function t "Send function to R"]
	 [toolbar-switch-ess-icon ess-switch-to-ESS t "Switch to ESS"]
		       
	 [toolbar-info-icon toolbar-info t "Info documentation"])))

  )

(if ess-use-toolbar 
    (ess-make-toolbar-r))

(defun R-mode  (&optional proc-name)
  "Major mode for editing R source.  See `ess-mode' for more help."
  (interactive)
  (setq ess-customize-alist R-customize-alist)
  ;;(setq imenu-generic-expression R-imenu-generic-expression)
  (ess-mode R-customize-alist proc-name)
  ;; SJE: hook on Emacs for setting up tool-bar.
  (if (and ess-toolbar (not (featurep 'xemacs)))
      (set (make-local-variable 'tool-bar-map) ess-toolbar))
  ;; ECB needs seminatic stuff.
  ;;  (if (featurep 'semantic)
  ;;      (setq semantic-toplevel-bovine-table r-toplevel-bovine-table))
  ;; AJR: Need to condition on this...!
  ;; MM: and you probably should really use ess-imenu-mode-function from the
  ;;     alist above!
  (if ess-imenu-use-S (ess-imenu-R)))

(provide 'ess-toolbar)