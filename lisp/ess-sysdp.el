;;; sysdep.el --- consolidate Emacs-version dependencies in one file.

;; Copyright (c) 1995 - 1997 Ben Wing.

;; Author: Ben Wing <ben@xemacs.org>, William Perry <wmperry@cs.indiana.edu>
;; Keywords: lisp, tools
;; Version: 0.003

;; Modified 05June2000 to reflect use for ESS
;; A.J. Rossini (rossini@biostat.washington.edu). 

;; The purpose of this file is to eliminate the cruftiness that
;; would otherwise be required of packages that want to run on multiple
;; versions of Emacs.  The idea is that we make it look like we're running
;; the latest version of XEmacs (currently 19.12) by emulating all the
;; missing functions.

;; #### This file does not currently do any advising but should.
;; Unfortunately, advice.el is a hugely big package.  Is any such
;; thing as `advice-lite' possible?

;; #### - This package is great, but its role needs to be thought out a bit
;; more.  Sysdep will not permit programs written for the old XEmacs API to
;; run on new versions of XEmacs.  Sysdep is a backward-compatibility
;; package for the latest and greatest XEmacs API.  It permits programmers
;; to use the latest XEmacs functionality and still have their programs run
;; on older versions of XEmacs...perhaps even on FSF Emacs.  It should NEVER
;; ever need to be loaded in the newest XEmacs.  It doesn't even make sense
;; to put it in the lisp/utils part of the XEmacs distribution because it's
;; real purpose is to be distributed with packages like w3 which take
;; advantage of the latest and greatest features of XEmacs but still need to
;; be run on older versions.  --Stig

;; Any packages that wish to use this file should load it using
;; `load-library'.  It will not load itself if a version of sysdep.el
;; that is at least as recent has already been loaded, but will
;; load over an older version of sysdep.el.  It will attempt to
;; not redefine functions that have already been custom-redefined,
;; but will redefine a function if the supplied definition came from
;; an older version of sysdep.el.

;; Packages such as w3 that wish to include this file with the package
;; should rename it to something unique, such as `w3-sysdep.el', and
;; load it with `load-library'.  That will ensure that no conflicts
;; arise if more than one package in the load path provides a version
;; of sysdep.el.  If multiple packages load sysdep.el, the most recent
;; version will end up loaded; as long as I'm careful not to
;; introduce bugs in previously working definitions, this should work
;; fine.

;; You may well discover deficiencies in this file as you use it.
;; The preferable way of dealing with this is to send me a patch
;; to sysdep.el; that way, the collective body of knowledge gets
;; increased.

;; IMPORTANT: leave the version string in the format X.XXX (e.g. 1.001)
;; so that string comparisons to other versions work properly.

(defconst sysdep-potential-version "0.003")

;; this macro means: define the function, but only if either it
;; wasn't bound before, or the supplied binding comes from an older
;; version of sysdep.el.  That way, user-supplied bindings don't
;; get overridden.

;; note: sysdep-defalias is often more useful than this function,
;; esp. since you can do load-time conditionalizing and can
;; optionally leave the function undefined. (e.g. frame functions
;; in v18.)

(defmacro sysdep-defun (function &rest everything-else)
  (` (cond ((and (not (fboundp (quote (, function))))
		 (or
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this
		 sysdep-potential-version)
	    (defun (, function) (,@ everything-else))))))

(defmacro sysdep-defvar (function &rest everything-else)
  (` (cond ((and (not (boundp (quote (, function))))
		 (or 
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defvar (, function) (,@ everything-else))))))

(defmacro sysdep-defconst (function &rest everything-else)
  (` (cond ((and (not (boundp (quote (, function))))
		 (or
		  (not
		   (stringp (get (quote (, function)) 'sysdep-defined-this)))
		  (and (get (quote (, function)) 'sysdep-defined-this)
		       (string-lessp
			(get (quote (, function)) 'sysdep-defined-this)
			sysdep-potential-version))))
	    (put (quote (, function)) 'sysdep-defined-this t)
	    (defconst (, function) (,@ everything-else))))))

;; similar for fset and defalias.  No need to quote as the argument
;; is already quoted.

(defmacro sysdep-fset (function def)
  (` (cond ((and (not (fboundp (, function)))
		 (or (not (stringp
			   (get (, function) 'sysdep-defined-this)))
		     (and (get (, function) 'sysdep-defined-this)
			  (string-lessp
			   (get (, function) 'sysdep-defined-this)
			   sysdep-potential-version)))
		 (, def))
	    (put (, function) 'sysdep-defined-this t)
	    (fset (, function) (, def))))))

(defmacro sysdep-defalias (function def)
  (` (cond ((and (not (fboundp (, function)))
		 (or (not (stringp
			   (get (, function) 'sysdep-defined-this)))
		     (and (get (, function) 'sysdep-defined-this)
			  (string-lessp
			   (get (, function) 'sysdep-defined-this)
			   sysdep-potential-version)))
		 (, def)
		 (or (listp (, def))
		     (and (symbolp (, def))
			  (fboundp (, def)))))
	    (put (, function) 'sysdep-defined-this t)
	    (defalias (, function) (, def))))))

;; bootstrapping: defalias and define-function don't exist
;; in older versions of lemacs

(sysdep-fset 'defalias 'fset)
(sysdep-defalias 'define-function 'defalias)

;; useful ways of determining what version is running
;; emacs-major-version and emacs-minor-version are
;; already defined in recent versions of FSF Emacs and XEmacs

(sysdep-defconst emacs-major-version
		 ;; will string-match ever fail?  If so, assume 19.0.
		 ;; (should we assume 18.something?)
		 (if (string-match "^[0-9]+" emacs-version)
		     (string-to-int
		      (substring emacs-version
				 (match-beginning 0) (match-end 0)))
		   19))

(sysdep-defconst emacs-minor-version
		 (if (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
		     (string-to-int
		      (substring emacs-version
				 (match-beginning 1) (match-end 1)))
		   0))

(sysdep-defconst sysdep-running-xemacs
		 (or (string-match "Lucid" emacs-version)
		     (string-match "XEmacs" emacs-version)))

(sysdep-defconst window-system nil)
(sysdep-defconst window-system-version 0)

(sysdep-defvar list-buffers-directory nil)
(sysdep-defvar x-library-search-path (`
				      ("/usr/X11R6/lib/X11/"
				       "/usr/X11R5/lib/X11/"
				       "/usr/lib/X11R6/X11/"
				       "/usr/lib/X11R5/X11/"
				       "/usr/local/X11R6/lib/X11/"
				       "/usr/local/X11R5/lib/X11/"
				       "/usr/local/lib/X11R6/X11/"
				       "/usr/local/lib/X11R5/X11/"
				       "/usr/X11/lib/X11/"
				       "/usr/lib/X11/"
				       "/usr/local/lib/X11/"
				       "/usr/X386/lib/X11/"
				       "/usr/x386/lib/X11/"
				       "/usr/XFree86/lib/X11/"
				       "/usr/unsupported/lib/X11/"
				       "/usr/athena/lib/X11/"
				       "/usr/local/x11r5/lib/X11/"
				       "/usr/lpp/Xamples/lib/X11/"
				       "/usr/openwin/lib/X11/"
				       "/usr/openwin/share/lib/X11/"
				       (, data-directory)
				       )
				      )
  "Search path used for X11 libraries.")

;; frame-related stuff.

(sysdep-defalias 'buffer-dedicated-frame 'buffer-dedicated-screen)
(sysdep-defalias 'deiconify-frame
  (cond ((fboundp 'deiconify-screen) 'deiconify-screen)
	;; make-frame-visible will be defined as necessary
	(t 'make-frame-visible)))
(sysdep-defalias 'delete-frame 'delete-screen)
(sysdep-defalias 'event-frame 'event-screen)
(sysdep-defalias 'event-glyph-extent 'event-glyph)
(sysdep-defalias 'find-file-other-frame 'find-file-other-screen)
(sysdep-defalias 'find-file-read-only-other-frame
  'find-file-read-only-other-screen)
(sysdep-defalias 'frame-height 'screen-height)
(sysdep-defalias 'frame-iconified-p 'screen-iconified-p)
(sysdep-defalias 'frame-left-margin-width 'screen-left-margin-width)
(sysdep-defalias 'frame-list 'screen-list)
(sysdep-defalias 'frame-live-p
  (cond ((fboundp 'screen-live-p) 'screen-live-p)
	((fboundp 'live-screen-p) 'live-screen-p)
	;; #### not sure if this is correct (this is for Epoch)
	;; but gnuserv.el uses it this way
	((fboundp 'screenp) 'screenp)))
(sysdep-defalias 'frame-name 'screen-name)
(sysdep-defalias 'frame-parameters 'screen-parameters)
(sysdep-defalias 'frame-pixel-height 'screen-pixel-height)
(sysdep-defalias 'frame-pixel-width 'screen-pixel-width)
(sysdep-defalias 'frame-right-margin-width 'screen-right-margin-width)
(sysdep-defalias 'frame-root-window 'screen-root-window)
(sysdep-defalias 'frame-selected-window 'screen-selected-window)
(sysdep-defalias 'frame-totally-visible-p 'screen-totally-visible-p)
(sysdep-defalias 'frame-visible-p 'screen-visible-p)
(sysdep-defalias 'frame-width 'screen-width)
(sysdep-defalias 'framep 'screenp)
(sysdep-defalias 'get-frame-for-buffer 'get-screen-for-buffer)
(sysdep-defalias 'get-frame-for-buffer-noselect 'get-screen-for-buffer-noselect)
(sysdep-defalias 'get-other-frame 'get-other-screen)
(sysdep-defalias 'iconify-frame 'iconify-screen)
(sysdep-defalias 'lower-frame 'lower-screen)
(sysdep-defalias 'mail-other-frame 'mail-other-screen)

(sysdep-defun frame-parameter (frame parameter)
  "Return FRAME's value for parameter PARAMETER.
  If FRAME is omitted, describe the currently selected frame."
  (cdr (assq parameter (frame-parameters frame))))

(sysdep-defalias 'make-frame
  (cond ((fboundp 'make-screen)
	 (function (lambda (&optional parameters device)
		     (make-screen parameters))))
	((fboundp 'x-create-screen)
	 (function (lambda (&optional parameters device)
		     (x-create-screen parameters))))))

(sysdep-defalias 'make-frame-invisible 'make-screen-invisible)
(sysdep-defalias 'make-frame-visible
  (cond ((fboundp 'make-screen-visible) 'make-screen-visible)
	((fboundp 'mapraised-screen) 'mapraised-screen)
	((fboundp 'x-remap-window)
	 (lambda (&optional x)
	   (x-remap-window)
	   (accept-process-output)))))
(sysdep-defalias 'modify-frame-parameters 'modify-screen-parameters)
(sysdep-defalias 'new-frame 'new-screen)
(sysdep-defalias 'next-frame 'next-screen)
(sysdep-defalias 'next-multiframe-window 'next-multiscreen-window)
(sysdep-defalias 'other-frame 'other-screen)
(sysdep-defalias 'previous-frame 'previous-screen)
(sysdep-defalias 'previous-multiframe-window 'previous-multiscreen-window)
(sysdep-defalias 'raise-frame
  (cond ((fboundp 'raise-screen) 'raise-screen)
	((fboundp 'mapraise-screen) 'mapraise-screen)))
(sysdep-defalias 'redraw-frame 'redraw-screen)
(sysdep-defalias 'select-frame 'select-screen)
(sysdep-defalias 'selected-frame 'selected-screen)
(sysdep-defalias 'set-buffer-dedicated-frame 'set-buffer-dedicated-screen)
(sysdep-defalias 'set-frame-height 'set-screen-height)
(sysdep-defalias 'set-frame-left-margin-width 'set-screen-left-margin-width)
(sysdep-defalias 'set-frame-position 'set-screen-position)
(sysdep-defalias 'set-frame-right-margin-width 'set-screen-right-margin-width)
(sysdep-defalias 'set-frame-size 'set-screen-size)
(sysdep-defalias 'set-frame-width 'set-screen-width)
(sysdep-defalias 'show-temp-buffer-in-current-frame 'show-temp-buffer-in-current-screen)
(sysdep-defalias 'switch-to-buffer-other-frame 'switch-to-buffer-other-screen)
(sysdep-defalias 'visible-frame-list 'visible-screen-list)
(sysdep-defalias 'window-frame 'window-screen)
(sysdep-defalias 'x-create-frame 'x-create-screen)
(sysdep-defalias 'x-set-frame-icon-pixmap 'x-set-screen-icon-pixmap)
(sysdep-defalias 'x-set-frame-pointer 'x-set-screen-pointer)
(sysdep-defalias 'x-display-color-p 'x-color-display-p)
(sysdep-defalias 'x-display-grayscale-p 'x-grayscale-display-p)
(sysdep-defalias 'menu-event-p 'misc-user-event-p)

(sysdep-defun event-point (event)
  (let ((posn (event-end event)))
    (if posn 
 	(posn-point posn))))

;; WMP - commenting these out so that Emacs 19 doesn't get screwed by them.
;; In particular, this makes the 'custom' package blow up quite well.
;;(sysdep-defun add-submenu (menu-path submenu &optional before)
;;  "Add a menu to the menubar or one of its submenus.
;;If the named menu exists already, it is changed.
;;MENU-PATH identifies the menu under which the new menu should be inserted.
;; It is a list of strings; for example, (\"File\") names the top-level \"File\"
;; menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
;; If MENU-PATH is nil, then the menu will be added to the menubar itself.
;;SUBMENU is the new menu to add.
;; See the documentation of `current-menubar' for the syntax.
;;BEFORE, if provided, is the name of a menu before which this menu should
;; be added, if this menu is not on its parent already.  If the menu is already
;; present, it will not be moved."
;;  (add-menu menu-path (car submenu) (cdr submenu) before))

;;(sysdep-defun add-menu-button (menu-path menu-leaf &optional before)
;;  "Add a menu item to some menu, creating the menu first if necessary.
;;If the named item exists already, it is changed.
;;MENU-PATH identifies the menu under which the new menu item should be inserted.
;; It is a list of strings; for example, (\"File\") names the top-level \"File\"
;; menu.  (\"File\" \"Foo\") names a hypothetical submenu of \"File\".
;;MENU-LEAF is a menubar leaf node.  See the documentation of `current-menubar'.
;;BEFORE, if provided, is the name of a menu item before which this item should
;; be added, if this item is not on the menu already.  If the item is already
;; present, it will not be moved."
;; (add-menu-item menu-path (aref menu-leaf 0) (aref menu-leaf 1)
;;		(aref menu-leaf 2) before))

(sysdep-defun make-glyph (&optional spec-list)
  (if (and spec-list (cdr-safe (assq 'x spec-list)))
      (make-pixmap (cdr-safe (assq 'x spec-list)))))

(sysdep-defalias 'face-list 'list-faces)

(sysdep-defun set-keymap-parent (keymap new-parent)
  (let ((tail keymap))
    (while (and tail (cdr tail) (not (eq (car (cdr tail)) 'keymap)))
      (setq tail (cdr tail)))
    (if tail
	(setcdr tail new-parent))))

;; Property list functions
;;
(sysdep-defun plist-put (plist prop val)
  "Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2 ...).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `(setq x (plist-put x prop val))' to be sure to use the new value.
The PLIST is modified by side effects."
  (let ((node (memq prop plist)))
    (if node
	(setcar (cdr node) val)
      (setq plist (cons prop (cons val plist))))
    plist))

(sysdep-defun plist-get (plist prop)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
  (while (and plist (not (eq (car plist) prop)))
    (setq plist (cdr (cdr plist))))
  (and plist (car (cdr plist))))

;; Extent stuff
(sysdep-fset 'delete-extent 'delete-overlay)
(sysdep-fset 'extent-end-position 'overlay-end)
(sysdep-fset 'extent-start-position 'overlay-start)
(sysdep-fset 'set-extent-endpoints 'move-overlay)
(sysdep-fset 'set-extent-property 'overlay-put)
(sysdep-fset 'make-extent 'make-overlay)

(sysdep-defun extent-property (extent property &optional default)
  (or (overlay-get extent property) default))

(sysdep-defun extent-at (pos &optional object property before at-flag)
  (let ((tmp (overlays-at (point)))
	ovls)
    (if property
	(while tmp
	  (if (extent-property (car tmp) property)
	      (setq ovls (cons (car tmp) ovls)))
	  (setq tmp (cdr tmp)))
      (setq ovls tmp
	    tmp nil))
    (car-safe
     (sort ovls
	   (function
	    (lambda (a b)
	      (< (- (extent-end-position a) (extent-start-position a))
		 (- (extent-end-position b) (extent-start-position b)))))))))

(sysdep-defun overlays-in (beg end)
  "Return a list of the overlays that overlap the region BEG ... END.
Overlap means that at least one character is contained within the overlay
and also contained within the specified region.
Empty overlays are included in the result if they are located at BEG
or between BEG and END."
  (let ((ovls (overlay-lists))
	tmp retval)
    (if (< end beg)
	(setq tmp end
	      end beg
	      beg tmp))
    (setq ovls (nconc (car ovls) (cdr ovls)))
    (while ovls
      (setq tmp (car ovls)
	    ovls (cdr ovls))
      (if (or (and (<= (overlay-start tmp) end)
		   (>= (overlay-start tmp) beg))
	      (and (<= (overlay-end tmp) end)
		   (>= (overlay-end tmp) beg)))
	  (setq retval (cons tmp retval))))
    retval))

(sysdep-defun map-extents (function &optional object from to
				    maparg flags property value)
  (let ((tmp (overlays-in (or from (point-min))
			  (or to (point-max))))
	ovls)
    (if property
	(while tmp
	  (if (extent-property (car tmp) property)
	      (setq ovls (cons (car tmp) ovls)))
	  (setq tmp (cdr tmp)))
      (setq ovls tmp
	    tmp nil))
    (catch 'done
      (while ovls
	(setq tmp (funcall function (car ovls) maparg)
	      ovls (cdr ovls))
	(if tmp
	    (throw 'done tmp))))))

;; misc
(sysdep-fset 'make-local-hook 'make-local-variable)

(sysdep-defun buffer-substring-no-properties (beg end)
  "Return the text from BEG to END, without text properties, as a string."
  (format "%s" (buffer-substring beg end)))
  
(sysdep-defun symbol-value-in-buffer (symbol buffer &optional unbound-value)
  "Return the value of SYMBOL in BUFFER, or UNBOUND-VALUE if it is unbound."
  (save-excursion
    (set-buffer buffer)
    (if (not (boundp symbol))
	unbound-value
      (symbol-value symbol))))

(sysdep-defun insert-file-contents-literally
  (file &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((file-name-handler-alist nil)
	(find-file-hooks nil))
    (insert-file-contents file visit beg end replace)))

(sysdep-defun alist-to-plist (alist)
  "Convert association list ALIST into the equivalent property-list form.
The plist is returned.  This converts from

\((a . 1) (b . 2) (c . 3))

into

\(a 1 b 2 c 3)

The original alist is not modified.  See also `destructive-alist-to-plist'."
  (let (plist)
    (while alist
      (let ((el (car alist)))
	(setq plist (cons (cdr el) (cons (car el) plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

(sysdep-defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Add a minor mode to `minor-mode-alist' and `minor-mode-map-alist'.
TOGGLE is a symbol which is used as the variable which toggle the minor mode,
NAME is the name that should appear in the modeline (it should be a string
beginning with a space), KEYMAP is a keymap to make active when the minor
mode is active, and AFTER is the toggling symbol used for another minor
mode.  If AFTER is non-nil, then it is used to position the new mode in the
minor-mode alists.  TOGGLE-FUN specifies an interactive function that
is called to toggle the mode on and off; this affects what appens when
button2 is pressed on the mode, and when button3 is pressed somewhere
in the list of modes.  If TOGGLE-FUN is nil and TOGGLE names an
interactive function, TOGGLE is used as the toggle function.

Example:  (add-minor-mode 'view-minor-mode \" View\" view-mode-map)"
  (if (not (assq toggle minor-mode-alist))
      (setq minor-mode-alist (cons (list toggle name) minor-mode-alist)))
  (if (and keymap (not (assq toggle minor-mode-map-alist)))
      (setq minor-mode-map-alist (cons (cons toggle keymap)
				       minor-mode-map-alist))))

(sysdep-defvar x-font-regexp-foundry-and-family
  (let ((- 		"[-?]")
	(foundry		"[^-]+")
	(family 		"[^-]+")
	)
    (concat "\\`[-?*]" foundry - "\\(" family "\\)" -)))

(sysdep-defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(sysdep-defun add-hook (hook-var function &optional at-end)
  "Add a function to a hook.
First argument HOOK-VAR (a symbol) is the name of a hook, second
 argument FUNCTION is the function to add.
Third (optional) argument AT-END means to add the function at the end
 of the hook list instead of the beginning.  If the function is already
 present, this has no effect.
Returns nil if FUNCTION was already present in HOOK-VAR, else new
 value of HOOK-VAR."
      (if (not (boundp hook-var)) (set hook-var nil))
      (let ((old (symbol-value hook-var)))
	(if (or (not (listp old)) (eq (car old) 'lambda))
	    (setq old (list old)))
	(if (member function old)
	    nil
	  (set hook-var
	       (if at-end
		   (append old (list function)) ; don't nconc
		 (cons function old))))))

(sysdep-defalias 'valid-color-name-p
  (cond
   ((fboundp 'x-valid-color-name-p)	; XEmacs/Lucid
    'x-valid-color-name-p)
   ((and window-system
	 (fboundp 'color-defined-p))	; NS/Emacs 19
    'color-defined-p)
   ((and window-system
	 (fboundp 'pm-color-defined-p))
    'pm-color-defined-p)
   ((and window-system
	 (fboundp 'x-color-defined-p))	; Emacs 19
    'x-color-defined-p)
   ((fboundp 'get-color)		; Epoch
    (function (lambda (color)
		(let ((x (get-color color)))
		  (if x
		      (setq x (progn
				(free-color x)
				t)))
		  x))))
   (t 'identity)))			; All others

;; Misc.
;; NT doesn't have make-symbolic-link
(sysdep-defalias 'make-symbolic-link 'copy-file)
(sysdep-defalias 'insert-and-inherit 'insert)

(sysdep-defun run-hook-with-args-until-success (hook &rest args)
  "Run HOOK with the specified arguments ARGS.
HOOK should be a symbol, a hook variable.  Its value should
be a list of functions.  We call those functions, one by one,
passing arguments ARGS to each of them, until one of them
returns a non-nil value.  Then we return that value.
If all the functions return nil, we return nil."
  (let ((rval nil)
	(todo (and (boundp hook) (symbol-value hook)))
	(global (and (boundp hook) (default-value hook)))
	(cur nil))
    (while (and (setq cur (car todo)) (not rval))
      (setq todo (cdr todo))
      (if (eq cur t)
	  (if global
	      (setq todo (append global todo)))
	(setq rval (apply cur args))))))

(sysdep-defun split-string (string pattern)
  "Return a list of substrings of STRING which are separated by PATTERN."
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))
    ))

(sysdep-defun member (elt list)
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(sysdep-defun rassoc (key list)
  (let ((found nil))
    (while (and list (not found))
      (if (equal (cdr (car list)) key) (setq found (car list)))
      (setq list (cdr list)))
    found))

(sysdep-defun display-error (error-object stream)
  "Display `error-object' on `stream' in a user-friendly way."
  (funcall (or (let ((type (car-safe error-object)))
		 (catch 'error
		   (and (consp error-object)
			(symbolp type)
			;;(stringp (get type 'error-message))
			(consp (get type 'error-conditions))
			(let ((tail (cdr error-object)))
			  (while (not (null tail))
			    (if (consp tail)
				(setq tail (cdr tail))
			      (throw 'error nil)))
			  t)
			;; (check-type condition condition)
			(get type 'error-conditions)
			;; Search class hierarchy
			(let ((tail (get type 'error-conditions)))
			  (while (not (null tail))
			    (cond ((not (and (consp tail)
					     (symbolp (car tail))))
				   (throw 'error nil))
				  ((get (car tail) 'display-error)
				   (throw 'error (get (car tail)
						      'display-error)))
				  (t
				   (setq tail (cdr tail)))))
			  ;; Default method
			  (function
			   (lambda (error-object stream)
			     (let ((type (car error-object))
				   (tail (cdr error-object))
				   (first t))
			       (if (eq type 'error)
				   (progn (princ (car tail) stream)
					  (setq tail (cdr tail)))
				 (princ (or (get type 'error-message) type)
					stream))
			       (while tail
				 (princ (if first ": " ", ") stream)
				 (prin1 (car tail) stream)
				 (setq tail (cdr tail)
				       first nil)))))))))
	       (function
		(lambda (error-object stream)
		  (princ "Peculiar error " stream)
		  (prin1 error-object stream))))
	   error-object stream))

(sysdep-defun decode-time (&optional specified-time)
  (let* ((date (current-time-string specified-time))
	 (dateinfo (and date (timezone-parse-date date)))
	 (timeinfo (and dateinfo (timezone-parse-time (aref dateinfo 3)))))
    (list (aref timeinfo 2) (aref timeinfo 1)
	  (aref timeinfo 0) (aref dateinfo 2)
	  (aref dateinfo 1) (aref dateinfo 0)
	  "unknown" nil 0)))

(sysdep-defun find-face (face)
  (car-safe (memq face (face-list))))

(sysdep-defun set-marker-insertion-type (marker type)
  "Set the insertion-type of MARKER to TYPE.
If TYPE is t, it means the marker advances when you insert text at it.
If TYPE is nil, it means the marker stays behind when you insert text at it."
  nil)

;; window functions

;; not defined in v18
(sysdep-defun eval-buffer (bufname &optional printflag)
  (interactive)
  (save-excursion
    (set-buffer bufname)
    (eval-current-buffer)))

(sysdep-defun window-minibuffer-p (window)
  "Returns non-nil if WINDOW is a minibuffer window."
  (eq window (minibuffer-window)))

(sysdep-defun window-live-p (window)
  "Returns t if OBJ is a window which is currently visible."
  (and (windowp window)
       (window-point window)))

(provide 'ess-sysdp)
;;; sysdep.el ends here

;;;(sysdep.el) Local Variables:
;;;(sysdep.el) eval: (put 'sysdep-defun 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defalias 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defconst 'lisp-indent-function 'defun)
;;;(sysdep.el) eval: (put 'sysdep-defvar 'lisp-indent-function 'defun)
;;;(sysdep.el) End:
