;;;; -*- Mode: Emacs-Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : S-tek.el
;;;; Authors         : David Smith
;;;; Created On      : June 25, 1992
;;;; Last Modified By: David Smith
;;;; Last Modified On: Mon Jun 29 14:51:26 CST 1992
;;;; Version         : 1.1
;;;; 
;;;; PURPOSE
;;;;    Tek graphics support of S-mode.
;;;; 	Component of the S-mode distribution -- see file S.el for info
;;;; 
;;;; Copyright 1992  David Smith   dsmith@stats.adelaide.edu.au
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tek support for S-mode   25-Jun-92
;;; Copyright 1992 David Smith  (dsmith@stats.adelaide.edu.au)
;;;
;;; This file forms part of the S-mode package defined in the file S.el
;;; and is autoloaded as required.

;;; GENERAL DISCLAIMER
;;; 
;;; This program is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public
;;; License as published by the Free Software Foundation; either
;;; version 1, or (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;; PURPOSE.  See the GNU General Public License for more
;;; details.
;;; 
;;; You should have received a copy of the GNU General Public
;;; License along with this program; if not, write to the Free
;;; Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.
;;; 
;;; In short: you may use this code any way you like, as long as you
;;; don't charge money for it, remove this notice, or hold anyone liable
;;; for its results.

;;;
;;; OVERVIEW
;;;

;;; If S-tek-mode is non-nil, then whenever output from an S
;;; command looks like Tek graphics (i.e. starts with S-tek-graphics-re,
;;; such as sent by the tek4014() driver) it is sent straight to the
;;; terminal. This mode may be toggled with \\[S-tek-mode-toggle].
;;; 
;;; This mode depends on being able to work out where the graphics finish
;;; and normal (text) output starts. In the easiest case, it finishes with
;;; your prompt and S-mode has no trouble detecting that. Sometimes
;;; plotting functions also display some text afterwards, and provided the
;;; function finishes and your prompt is displayed *at the start* of a
;;; line this is no problem either, but make sure any such function you
;;; write finishes any text with a newline. Functions like
;;; 
;;; 	badfun <- function() { plot(1:10) cat(\"Hello\") invisible() }
;;; 
;;; will break the graphics detector. Other functions, such as 
;;; `gam(obj,ask=T)' present a menu after the plot and wait for input (and
;;; so your prompt isn't displayed). The variable S-tek-possible-graph-prompts 
;;; is a regular expression used to detect any alternative prompt used in
;;; this case.
;;; 
;;; When the graphics display has completed, press any key to return to
;;; your Emacs display. This mode also works with the \"ask=T\" option to
;;; tek4014(), however any single key is now the appropriate response to
;;; the \"GO?\" prompt.
;;; 
;;; Unexpected redisplays of the Emacs screen (such as caused by
;;; display-time or garbage collection) can possibly send garbage to your
;;; graphics display, but unfortunately there seems to way to prevent this.
;;; 
;;; If you have a very simple prompt, it may by chance appear in the
;;; graphics output which could possibly cause problems; if this occurs
;;; you will be given a warning. It is advisable to choose a prompt with
;;; at least two characters. Tek mode relies on your prompt not changing
;;; while it is active. If your prompt changes while S-tek-mode is t be
;;; sure to tell the Tek graphics detector with
;;; \\[S-tek-get-simple-prompt], or toggle S-tek-mode twice with
;;; \\[S-tek-mode-toggle].
;;; 
;;; When this mode is enabled, S-mode will make your Emacs process
;;; unusable while waiting for the first output from a function (so it can
;;; determine whether or not it's graphics). You may be stuck for a long
;;; time when executing a time-consuming function that produces no output.

;;;
;;; Internal variables to S tek mode.
;;;

(defvar S-tek-graphics-re "\035\\|\033"
  "Regexp signalling start of Tek graphics.")

(defvar S-tek-go-string "\ v @\GO? "
  "String which, if found in TEK graphics, signals that S is waiting
for another plot.")

(defvar S-tek-graphics-end-string "\C-_\033:"
  "Regexp which, along with S-tek-simple-prompt, signals end of graphics.")

(defvar S-tek-prompt-warning-given t
  "Flag, t if warning given about prompt found in graphics.")

(defvar S-tek-enter-tek-mode-code "\e[?38h"
  "Control codes to enter Tek mode.")

(defvar S-tek-leave-tek-mode-code 
  (if (string= (getenv "TERM") "xterm")
      "\e\C-c" "\C-x") 
  "Control codes to leave Tek mode.")

;;;
;;; The Tek driver itself
;;;

(defun S-tek-looking-at-string (str)
  "Like looking-at, but does a simple string search"
  (string= str (buffer-substring (point) (+ (point) (length str)))))

(defun S-tek-looking-after-string (str)
  "Like S-tek-looking-at-string, but considers text before point"
  (string= str (buffer-substring (point) (- (point) (length str)))))

(defun S-tek-wait-for-eog ()
;;; Waits until the Tek graphics output is completed, and puts point just
;;; before the prompt
  (let* ((cbuffer (current-buffer))
         (sprocess (get-process "S"))
         (sbuffer (process-buffer sprocess))
         (limit comint-last-input-end)
	 result)
    (set-buffer sbuffer)
    (if (not S-tek-simple-prompt)
	(S-tek-get-simple-prompt))
    (while (progn
	     (accept-process-output sprocess)
	     (goto-char (point-max))
	     (not (if (or (search-backward S-tek-simple-prompt limit t)
			  (re-search-backward S-tek-possible-graph-prompts limit t))
		      (cond
		       ((S-tek-looking-after-string S-tek-graphics-end-string) 
			(setq result 'done))
		       ((S-tek-looking-after-string "\n")
			;; Found prompt at bol. This means that graphics finished
			;; some time ago; go back and get it
			(search-backward S-tek-graphics-end-string nil t) 
			(search-forward S-tek-graphics-end-string nil t)
					; put point at eog
			(setq result 'done))
		       (t
			;; Other alternatives mean prompt found in middle of graphics
			(if S-tek-prompt-warning-given nil
			  (message "Prompt found in Tek graphics. Maybe you should change it.")
			  (sleep-for 5)
			  (setq S-tek-prompt-warning-given t))
			  nil))
		    ;; Prompt not found, maybe a GO? is waiting?
		    (if (not (search-backward S-tek-go-string limit t)) nil
		      ;; GO? found
		      (search-forward S-tek-go-string) ; Put point at end of GO
		      (setq result 'go))))))
    (set-buffer cbuffer)
    result))

;;; (fset 'real-sit-for (symbol-function 'sit-for))
;;; (fset 'real-set-buffer-modofied-p (symbol-function 'set-buffer-modified-p))

(defun S-tek-snarf-graphics nil
  ;; We're in the S process buffer, and the output
  ;; from an S command is about to appear. If it's Tek graphics,
  ;; pull it out and plot it, else leave it alone.
  (accept-process-output (get-process "S"))
  (let* ((pmark (process-mark (get-buffer-process (current-buffer))))
	 (output (buffer-substring comint-last-input-end pmark))
	 graphics
	 graph-type)
    (if (string-match S-tek-graphics-re output)
	(progn
	  (garbage-collect)
	  ;; Hopefully sentinels will not cause another collection, causing
	  ;; a message which looks ugly in the graphics.
	  (message 
	   (concat "Grabbing graphics ... wait  "
		   (if S-tek-pause-for-graphics 
		       "[Press any key to clear graphics]")))
	  (setq graph-type (S-tek-wait-for-eog))
	  (save-excursion
	    (setq graphics
		  (buffer-substring comint-last-input-end 
				    (point)))
	    (delete-region comint-last-input-end (point))
	    (send-string-to-terminal S-tek-enter-tek-mode-code)
	    (send-string-to-terminal graphics)
	    (send-string-to-terminal S-tek-leave-tek-mode-code)
	    (if (eq graph-type 'go)
		(progn
		  (if S-tek-pause-for-graphics nil
		    (message "Press a key for next plot."))
		  (read-char)
		  (inferior-S-send-input)
		  (forward-line -1)
		  (delete-blank-lines))
	      (if S-tek-pause-for-graphics
		  (read-char))
	    (redraw-display)))
	    (goto-char (point-max))))))

;;; Provide package

(provide 'S-tek)