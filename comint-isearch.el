;;; -*-Emacs-Lisp-*- Incremental command search for comint
;;; Terry Glanfield (tg.southern@rxuk.xerox.com)
;;; Version 1.0
;;; 
;;; This file is not part of GNU Emacs but the same permissions apply.
;;; 
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to
;;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;;
;;; This is an incremental command search mode for comint.  Previous
;;; commands that match are inserted on the command line as you type.
;;; Anyone familiar with the emacs isearch or bash should find this easy
;;; to adapt to.
;;; 
;;; It is based on ideas from bash and borrows heavily from isearch.el.
;;; 
;;; Differences from bash:
;;;   * C-s searches forwards
;;;   * DEL cancels characters or moves to previous search
;;;   * C-g also moves to previous successful search
;;;   * Inserts previous search string
;;; 
;;; Differences from isearch:
;;;   * No C-w or C-y
;;;   * No wrap around, yet
;;; 
;;; It uses the variables search-repeat-char, search-exit-char etc.
;;; 
;;;  recommended usage:
;;; (setq cmushell-load-hook
;;;       '((lambda ()
;;; 	  (require 'comint-isearch)
;;; 	  (define-key cmushell-mode-map "\C-r" 'comint-isearch))))

;; LCD Archive Entry:
;; comint-isearch|Terry Glanfield|tg.southern@rxuk.xerox.com
;; |Command line incremental searching for comint.
;; |92-04-10|Version 1.0|~/packages/comint-isearch.el.Z

(require 'comint)
(provide 'comint-isearch)

(defvar comint-last-isearch-string nil
  "Last string searched for in comint-isearch.")

(defvar comint-isearch-buffer " *Ring Buffer*"
  "Buffer used in comint-isearch")

(defun comint-isearch ()
  "Do incremental searching for commands in comint shells.
As you type characters, they add to the search string and the
matching command line from the history ring is inserted.
Type Delete to cancel characters from end of search string.
Type ESC to exit, leaving point at location found.
Type C-r to search again, C-s to search again forwards.
Type C-q to quote control character to search for it.
Type RET to send this command to the shell.
Other control and meta characters terminate the search
 and are then executed normally.
The above special characters are mostly controlled by parameters;
 do M-x apropos on search-.*-char to find them.
C-g while searching or when search has failed
 cancels input back to what has been found successfully.
C-g when search is successful aborts and moves point to starting point."
  (interactive)
  (let ((ring-len (ring-length input-ring)))
    (cond ((not (comint-after-pmark-p))
	   (isearch-backward))
	  ((<= ring-len 0)
	   (message "Empty input ring")
	   (ding))
	  (t
	   (comint-isearch-internal)))))

(defun comint-isearch-internal ()
  (let* ((search-string "")
	 (search-message "")
	 (success t)
	 (cmds nil)
	 (forward nil)
	 (pmark (marker-position
		 (process-mark (get-buffer-process (current-buffer)))))
	 (saved-prompt (buffer-substring
			(save-excursion (beginning-of-line) (point))
			pmark))
	 (saved-command (buffer-substring
			 pmark
			 (save-excursion (end-of-line) (point))))
	 (saved-point (- (point-max) (point)))
	 (ring input-ring)
	 (ring-buf (get-buffer-create comint-isearch-buffer))
	 (ring-point nil)
	 (line-point 0)
	 (abort-flag nil)
	 (inhibit-quit t))  ;Prevent ^G from quitting.
    (save-excursion
      (set-buffer ring-buf)
      ;; fill temporary buffer with history ring
      (erase-buffer)
      (let ((n ring-len))
	(while (> n 0)
	  (setq n (1- n))
	  (insert (ring-ref ring n) 10)))
      (insert saved-command)
      (end-of-buffer)
      (backward-char saved-point)
      (setq ring-point (point))
      (setq line-point (- (point-max) (point))))
    (comint-isearch-push)
    (catch 'search-done
      (while t
	(or (>= unread-command-char 0)
	    (progn
	      (or (input-pending-p)
		  (comint-isearch-prompt))))
	(let ((char (if quit-flag
			?\C-g
		      (read-char))))
	  (setq quit-flag nil)
	  (cond ((and (>= char 128)
		      search-exit-option)
		 (setq unread-command-char char)
		 ;; Meta character means exit search.
		 (setq unread-command-char char)
		 (throw 'search-done t))
		((eq char search-exit-char)
		 ;; Esc means exit search normally.
		 (throw 'search-done t))
		((= char ?\C-g)
		 ;; ^G means the user tried to quit.
		 ;; needs to be more clever
		 (ding)
		 (discard-input)
		 (if success
		     ;; really do quit.
		     (progn (setq abort-flag t)
			    (throw 'search-done t))
		   ;; If search is failing, rub out until it is once more
		   ;;  successful.
		   (while (not success) (comint-isearch-pop))
		   ;; If it is now at the start, exit anyway
		   (if (equal search-string "")
		       (progn (setq abort-flag t)
			      (throw 'search-done t)))))
		((or (eq char search-repeat-char)
		     (eq char search-reverse-char))
		 (if (eq forward (eq char search-repeat-char))
		     ;; C-s in forward or C-r in reverse.
		     (if (equal search-string "")
			 ;; If search string is empty, use last one.
			 (setq search-string comint-last-isearch-string
			       search-message (mapconcat 'text-char-description
							 search-string ""))
		       ;; If already have what to search for, repeat it.
		       (or success (ding)))
		   ;; C-s in reverse or C-r in forward, change direction.
		   (setq forward (not forward)))
		 (setq success t)
		 (or (equal search-string "")
		     (comint-isearch-search t))
		 (comint-isearch-push))
		((= char search-delete-char)
		 ;; Rubout means discard last input item and move point
		 ;; back.  If buffer is empty, just beep.
		  (if (null (cdr cmds))
		      (ding)
		    (comint-isearch-pop)))
		((or (= char ?\r)
		     (= char ?\n))
		 ;; Accept this line
		 (setq unread-command-char char)
		 (throw 'search-done t))
		(t
		 ;; could add search-yank-word-char
		 ;; and search-yank-line-char in here
		 (cond ((and
			 search-exit-option
			 (/= char search-quote-char)
			 (or (= char ?\177)
			     (and (< char ? ) (/= char ?\t) (/= char ?\r))))
			;; Any other control char =>
			;;  unread it and exit the search normally.
			(setq unread-command-char char)
			(throw 'search-done t))
		       (t
			;; Any other character => add it to the
			;;  search string and search.
			(and (= char search-quote-char)
			     (setq char (read-quoted-char
					 (comint-isearch-prompt t))))
			(setq search-string (concat
					     search-string
					     (char-to-string char))
			      search-message (concat
					      search-message
					      (text-char-description char)))))
		 (if success
		     (comint-isearch-search))
		 (comint-isearch-push))))))
    (message "")
    (delete-region
     (progn (beginning-of-line) (point))
     (progn (end-of-line) (point)))
    (insert-string saved-prompt)
    (if (or abort-flag
	    (equal search-string ""))
	(progn (insert saved-command)
	       (backward-char saved-point))
      (progn (insert (comint-isearch-selected-line))
	     (backward-char line-point)
	     (if (> (length search-string) 0)
		 (setq comint-last-isearch-string search-string))))
    (set-marker (process-mark (get-buffer-process (current-buffer))) pmark)))

(defun comint-isearch-selected-line ()
  (save-excursion
    (set-buffer ring-buf)
    (goto-char ring-point)
    (beginning-of-line)
    (buffer-substring
     (point)
     (progn (end-of-line) (point)))))

(defun comint-isearch-prompt (&optional c-q-hack)
  (let ((m (concat "(I-search: '"
		   search-message
		   (if c-q-hack "^Q" "")
		   "'): "))
	(c (if ring-point
	       (comint-isearch-selected-line)
	     "")))
    (beginning-of-line)
    (delete-region (point) (save-excursion (end-of-line) (point)))
    (insert-string m c)
    (backward-char line-point)
    ;; ditch garbage from mini-buffer
    (message " ")))

(defun comint-isearch-pop ()
  (setq cmds (cdr cmds))
  (let ((cmd (car cmds)))
    (setq search-string (car cmd)
	  search-message (car (cdr cmd))
	  success (nth 2 cmd)
	  forward (nth 3 cmd)
	  ring-point (nth 4 cmd)
	  line-point (nth 5 cmd))
    (save-excursion
      (set-buffer ring-buf)
      (goto-char ring-point))))

(defun comint-isearch-push ()
  (setq cmds (cons (list search-string search-message success
			 forward ring-point line-point)
		   cmds)))

(defun comint-isearch-search (&optional repeat)
  (save-excursion
    (set-buffer ring-buf)
    (if forward
	(goto-char ring-point)
      (goto-char (+ ring-point (if (null repeat) (length search-string) 0))))
    (condition-case lossage
	(let ((inhibit-quit nil))
	  (setq success
		(funcall
		 (if forward 'search-forward 'search-backward)
		 search-string nil t))
	  (if success
	      (progn (setq ring-point (point))
		     (end-of-line)
		     (setq line-point (- (point) ring-point)))))
      (quit (setq unread-command-char ?\C-g)
	    (setq success nil)))
    (if success
	nil
      ;; Ding if failed this time after succeeding last time.
      (and (nth 2 (car cmds))
	   (ding)))))
