;;; ess-rlib.el --- browse contents of R packages.
;; Stephen Eglen, GPL applies.
;;
;; Primitive browswing facilities for the list of packages, and
;; the functions within a package.  Use RET or mouse-2 to click on a
;; link and see either contents of a package or the function.
;;
;; To use, simply start a R session, eval this buffer, 
;; then, in the R buffer, do 
;; source("ess-rlib.R")
;; to load the changes to the data() and print.libraryIQR() functions.
;;
;; Then do M-x
;; ess-rpackage or use C-x C-e at end of one of following lines:
;;
;; (ess-rpackage "ctest")
;; okay
;;
;; (ess-rpackage "foreign")
;; okay, but fails on "S3 read functions"
;;
;; (ess-rpackage "eda")
;; shows help for lib, but won't show individual files unless package 
;; has been loaded.  How to show help without loading the package first?

;; (ess-rpackage "ts")
;; not so good, due to regexp failure!

;; (ess-rpackage "base")
;; (ess-rpackage "mva")
;; one problem, near end of buffer.

;; Presumably, adding link markup to the output from library() command
;; would help here so that only functions will get converted into 
;; links.  This would be more robust than using regexps.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (ess-rdata)
;; This command will browse the output of the data() command, using
;; the extra markup of data names.  Output goes to the *ess-rdata*
;; buffer.  It currently just converts the data names into links
;; (highlight with mouse; underline), but no action is made on the
;; links.  Kurt suggested before making e.g. ? bring up help on that
;; data topic, and RET to load the package.
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ess-rpackage (package)
  "View functions within the package named PACKAGE.  The package must
have already been loaded; if the package is not loaded, the functions
in it can still be viewed, but the help for those functions cannot be
viewed.  This function is therefore currently restricted to show
contents for loaded packages only.  This calls an extra R funcition named
funs.for.package()."
  (interactive (let ()
		 (list 
		  (completing-read "Package name: "  
				   (mapcar 'list
					   (ess-rpackage-list))))))
  (let (beg str (all t))
    (setq str (concat "funs.for.package('"
		      package
		      "')\n"))
    (ess-execute str nil "ess-rlib")
    (pop-to-buffer "*ess-rlib*")
    (goto-char (point-min))
    (while (re-search-forward "^[^ ]" nil t)
      (beginning-of-line)
      (setq beg (point))
      (re-search-forward "\\s-")
      (add-text-properties beg (1- (point)) 
			   '(face underline
				  mouse-face highlight
				  help-xref function))
      ))
  
  ;; End of mark up, so add some text to the top of buffer.
  (goto-char (point-min))
  (insert "Functions in package " package ":\n\n")
  (goto-char (point-min))
  (ess-rpackage-mode)
  )

(defun ess-rpackage-list ()
  "Return list of packages currently loaded."
  (save-window-excursion
    (ess-execute "cat(.packages(), '\\n')")
    (pop-to-buffer "*ess-output*")
    (cdr (reverse
	  (split-string (buffer-substring (point-min) (point-max)) " ")))))


(defun ess-rpackage-1 (lib)
  "View help available for a topic.  Lib should either be blank or
name of a package.  It currently uses regexps to guess which parts of
the output are function and package names.  This mostly works, but for
a classic failure, see output from viewing the base package.
Old version."
  (interactive "sPackage name (leave blank for lib list): ")
  ;; todo: would it be worth getting completion for package list?
  (let (beg str (all t))
    (setq str
	  (if (equal lib "")
	      "library()"
	    (setq all nil)
	    (concat "library(help="
		    lib
		    ")\n")))
    (ess-execute str nil "ess-rlib")
    (pop-to-buffer "*ess-rlib*")
    (if all 
	(ess-markup-libnames)
      (re-search-forward "^Index:")
      (while (re-search-forward "^[^ ]+ " nil t)
	(beginning-of-line)
	(setq beg (point))
	(re-search-forward "[ \t]")
	(add-text-properties beg (1- (point)) 
			     '(     face underline
				       mouse-face highlight
				       help-xref function))
      (end-of-line)
      ))

    ;; end of mark up
    (goto-char (point-min))
    (ess-rpackage-mode)
    )
  )

(defun ess-markup-libnames ()
  "Markup the output from library() command."
  (goto-char (point-min))
  (while (re-search-forward "^\\\\package{" nil t)
    (delete-region (point) (progn (beginning-of-line) (point)))
    (setq beg (point))
    (search-forward "}")
    (delete-backward-char 1)
    (add-text-properties beg (point)
			 '(face underline
				mouse-face highlight
				help-xref library))
    (end-of-line)))


(defun ess-markup-libnames-old ()
  "Markup the output from library() command."
  (goto-char (point-min))
  (while (re-search-forward "^[^ ]+ " nil t)
    (beginning-of-line)
    (if (not (looking-at "Packages in library"))
	(progn
	  (setq beg (point))
	  (re-search-forward "[ \t]")
	  (add-text-properties beg (1- (point)) 
			       '(face underline
				      mouse-face highlight
				      help-xref library))))
	  (end-of-line)
	  ))


;;; Set up the major mode for viewing.

(define-derived-mode ess-rpackage-mode
  text-mode "Rlib"
  "Major mode for browsing package contents.
\\{ess-rpackage-mode-map}"
  (setq case-fold-search nil))


;; define the keys.
(if (featurep 'xemacs)
    (define-key ess-rpackage-mode-map [button2] 'ess-rpackage-mouse-view)
  (define-key ess-rpackage-mode-map [mouse-2] 'ess-rpackage-mouse-view)
  )
(define-key ess-rpackage-mode-map  [return] 'ess-rpackage-show-help)
(define-key ess-rpackage-mode-map "\t" 'help-next-ref)
(if (featurep 'xemacs)
    (define-key ess-rpackage-mode-map [iso-left-tab] 'help-previous-ref)
  (define-key ess-rpackage-mode-map [<S-iso-lefttab>] 'help-previous-ref))


(defun ess-rpackage-show-help ()
  "Show ESS help  for item on current line."
  (interactive)
  (let 
      (beg fn type)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (if (looking-at "[ \t\n]")
	  (message "No function on this line.")
	(setq type (get-text-property (point) 'help-xref))
	(re-search-forward "\\s-")
	(setq fn (buffer-substring-no-properties beg (1- (point))))
	(if (equal type 'function)
	    (ess-display-help-on-object fn)
	  (ess-rpackage fn))))))

(defun ess-rpackage-mouse-view (event)
  "In rdired, visit the object on the line you click on."
  ;; copied from ess-rdired.
  (interactive "e")
  (let (window pos)
    (save-excursion
      (if (featurep 'xemacs)
	  ;; XEmacs
	  (setq window (event-window event)
		pos (event-point event))
	;; Emacs
	(setq window (posn-window (event-end event))
	      pos (posn-point (event-end event))))
      (if (not (windowp window))
	  (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (ess-rpackage-show-help))))

(defun ess-rdata ()
  "Show the data currently available.  Currently the links are not
active -- nothing is set up to handle either pressing RET or mouse-2
over a data name.  Kurt suggested maybe `?' on a data item lists its
help, and RET to load the data set?"
  (interactive)
  (let (beg str (all t))
    (setq str "data()")
    (ess-execute str nil "ess-rdata")
    (pop-to-buffer "*ess-rdata*")

    ;; Now mark up the buffer.
    (goto-char (point-min))
    (while (re-search-forward "^\\\\data{" nil t)
      (delete-region (point) (progn (beginning-of-line) (point)))
      (setq beg (point))
      (search-forward "}")
      (delete-backward-char 1)
      (add-text-properties beg (point)
			   '(face underline
				  mouse-face highlight
				  help-xref library))
      (end-of-line)))
  (goto-char (point-min))
  )
