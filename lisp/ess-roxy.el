;;; ess-roxy.el --- convenient editing of in-code roxygen documentation
;;
;; Copyright (C) 2009 Henning Redestig
;;
;; Author: Henning Redestig <henning.red * go0glemail c-m>
;; Keywords: convenience tools
;;
;; This file is part of ESS
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;; Lots of inspiration from doc-mode,
;; http://nschum.de/src/emacs/doc-mode/
;;
;; Features::
;;
;; - basic highlighting
;; - generating and updating templates from function definition and customized default template
;;   - C-c C-e C-o :: update template
;; - navigating and filling roxygen fields
;;   - C-c TAB, M-q, C-a, ENTER, M-h :: advised tag completion, fill-paragraph,
;;        move-beginning-of-line, newline-and-indent, mark-paragraph
;;   - C-c C-e n,p :: next, previous roxygen entry
;;   - C-c C-e C-c :: Unroxygen region. Convenient for editing examples.
;; - folding visibility using hs-minor-mode
;;   - TAB :: advised ess-ident-command, hide entry if in roxygen doc.
;; - preview
;;   - C-c C-e C-r :: create a preview of the Rd file as generated
;;     using roxygen
;;
;; To enable it for ESS, put something like
;;
;; (add-to-list 'load-path "/path/to/dir/with/ess-roxy")
;; (require 'ess-roxy)
;; (add-hook 'ess-mode-hook
;; 	  (lambda () (ess-roxy-mode) ))

(require 'ess-custom)
(require 'hideshow)

;; ------------------
(defconst ess-roxy-version "0.2"
  "Current version of ess-roxy.el.")

(defvar ess-roxy-mode-map nil
  "Keymap for `ess-roxy' mode.")
(if ess-roxy-mode-map
    nil
  (setq ess-roxy-mode-map (make-sparse-keymap))
  (if ess-roxy-hide-show-p
      (define-key ess-roxy-mode-map (kbd "C-c C-e C-h") 'ess-roxy-hide-all))
  (define-key ess-roxy-mode-map (kbd "C-c C-e n")   'ess-roxy-next-entry)
  (define-key ess-roxy-mode-map (kbd "C-c C-e p")   'ess-roxy-previous-entry)
  (define-key ess-roxy-mode-map (kbd "C-c C-e C-r")   'ess-roxy-preview-Rd)
  (define-key ess-roxy-mode-map (kbd "C-c C-e C-c") 'ess-roxy-toggle-roxy-region)
  (define-key ess-roxy-mode-map (kbd "C-c C-o") 'ess-roxy-update-entry))

(defconst ess-roxy-font-lock-keywords
  (eval-when-compile
    `((,(concat ess-roxy-str " *\\([@\\]"
		(regexp-opt ess-roxy-tags-param t)
		"\\)\\>")
       (1 font-lock-keyword-face prepend))
      (,(concat ess-roxy-str " *\\([@\\]"
         (regexp-opt '("param") t)
         "\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
       (1 font-lock-keyword-face prepend)
       (3 font-lock-variable-name-face prepend))
      (,(concat "[@\\]" (regexp-opt ess-roxy-tags-noparam t) "\\>")
       (0 font-lock-variable-name-face prepend))
      (,(concat ess-roxy-str)
       (0 'bold prepend)))))

(define-minor-mode ess-roxy-mode
  "Minor mode for editing in-code documentation."
  :lighter " Rox"
  :keymap ess-roxy-mode-map
  (if ess-roxy-mode
      (progn
        (font-lock-add-keywords nil ess-roxy-font-lock-keywords)
	(if ess-roxy-hide-show-p
	    (progn
	      ;(setq hs-c-start-regexp "s")
	      (if (condition-case nil
		      (if (and (symbolp hs-minor-mode)
			       (symbol-value hs-minor-mode))
			  nil t) (error t) )
		  (progn
		    (hs-minor-mode)))
	      (if ess-roxy-start-hidden-p
		  (ess-roxy-hide-all)))))
    (if ess-roxy-hide-show-p
	(if hs-minor-mode
	    (progn
	      (hs-show-all)
	      (hs-minor-mode))))
    (font-lock-remove-keywords nil ess-roxy-font-lock-keywords))
  (when font-lock-mode
    (font-lock-fontify-buffer)))


;; (setq hs-c-start-regexp ess-roxy-str)
;; (make-variable-buffer-local 'hs-c-start-regexp)



;; Function definitions
(defun ess-roxy-beg-of-entry ()
  "Get point number at start of current entry, 0 if not in entry"
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg -1)
      (if (not (ess-roxy-entry-p))
	  (setq beg 0)
	(setq beg (point)))
      (while (and (= (forward-line -1) 0) (ess-roxy-entry-p))
	(setq beg (point)))
      beg)))

(defun ess-roxy-beg-of-field ()
  "Get point number at beginning of current field, 0 if not in entry"
  (save-excursion
    (let (cont beg)
      (beginning-of-line)
      (setq beg 0)
      (setq cont t)
      (while (and (ess-roxy-entry-p) cont)
	(setq beg (point))
	(if (looking-at (concat "^" ess-roxy-str " *[@].+"))
	    (setq cont nil))
	(if (looking-at (concat "^" ess-roxy-str " *$"))
	    (progn
	      (forward-line 1)
	      (setq beg (point))
	      (setq cont nil)))
	(if cont (setq cont (= (forward-line -1) 0))))
      beg)))

(defun ess-roxy-end-of-entry ()
  " get point number at end of current entry, 0 if not in entry"
  (save-excursion
    (let ((end))
      (end-of-line)
      (setq end -1)
      (if (not (ess-roxy-entry-p))
	  (setq end 0)
	(setq end (point)))
      (while (and (= (forward-line 1) 0) (ess-roxy-entry-p))
	(end-of-line)
	(setq end (point)))
      end)))

(defun ess-roxy-end-of-field ()
  "get point number at end of current field, 0 if not in entry"
  (save-excursion
    (let ((end nil)
	  (cont nil))
      (setq end 0)
      (if (ess-roxy-entry-p) (progn (end-of-line) (setq end (point))))
      (beginning-of-line)
      (forward-line 1)
      (setq cont t)
      (while (and (ess-roxy-entry-p) cont)
	(setq end (point))
	(if (or (looking-at (concat "^" ess-roxy-str " *$"))
		(looking-at (concat "^" ess-roxy-str " *[@].+")))
	    (progn
	      (forward-line -1)
	      (end-of-line)
	      (setq end (point))
	      (setq cont nil)))
	(if cont (setq cont (= (forward-line 1) 0))))
      end)))

(defun ess-roxy-entry-p ()
  "True if point is in a roxy entry"
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^" ess-roxy-str))))

(defun ess-roxy-narrow-to-field ()
  "Go to to the start of current field"
  (interactive)
  (let ((beg (ess-roxy-beg-of-field))
	(end (ess-roxy-end-of-field)))
    (narrow-to-region beg end)))

(defun ess-roxy-fill-field ()
  "Fill the current roxygen field."
  (interactive)
  (if (ess-roxy-entry-p)
      (save-excursion
	(let ((beg (ess-roxy-beg-of-field))
	      (end (ess-roxy-end-of-field))
	      (fill-prefix (concat ess-roxy-str " ")))
	  (fill-region beg end nil t)))))

(defun ess-roxy-goto-func-def ()
  "put point at start of function either that the point is in or
below the current roxygen entry, error otherwise"
  (if (ess-roxy-entry-p)
      (progn
	(ess-roxy-goto-end-of-entry)
	(forward-line 1)
	(beginning-of-line))
    (goto-char (car (ess-end-of-function)))))

(defun ess-roxy-get-args-list-from-def ()
  "get args list for current function"
  (save-excursion
    (ess-roxy-goto-func-def)
    (let* ((args (ess-roxy-get-function-args)))
      (mapcar (lambda (x) (cons x '(""))) args))))

(defun ess-roxy-insert-args (args &optional here)
  "Insert an args list to the end of the roxygen entry for the
function at point. if here is supplied start inputting
`here'. Finish at end of line."
  (let* ((arg-des nil))
    (if (or (not here) (< here 1))
	(progn
	  (ess-roxy-goto-end-of-entry)
	  (beginning-of-line)
	  (if (not (looking-at "\="))
	      (progn
		(end-of-line))))
      (goto-char here))
    (while (stringp (car (car args)))
      (setq arg-des (pop args))
      (insert (concat "\n"
		      ess-roxy-str " @param " (car arg-des) " "))
      (insert (concat (car (cdr arg-des))))
      (ess-roxy-fill-field))))

(defun ess-roxy-merge-args (fun ent)
  "Take two args lists (alists) and return their union. Result
holds all keys from both fun and ent but no duplicates and
association from ent are preferred over entries from fun. Also,
drop entries from ent that are not in fun and are associated with
the empty string."
  (let ((res-arg nil)
	(arg-des))
    (while (stringp (car (car fun)))
      (setq arg-des (pop fun))
      (if (assoc (car arg-des) ent)
	  (setq res-arg
		(cons (cons (car arg-des) (cdr (assoc (car arg-des) ent))) res-arg))
	(setq res-arg (cons (cons (car arg-des) '("")) res-arg))))
    (while (stringp (car (car ent)))
      (setq arg-des (pop ent))
      (if (and (not (assoc (car arg-des) res-arg)) (not (string= (car (cdr arg-des)) "")))
	  (setq res-arg (cons (cons (car arg-des) (cdr arg-des)) res-arg))))
    (nreverse res-arg)))

(defun ess-roxy-update-entry ()
  "Update the current entry or the entry above the function which
the point is in. Add basic roxygen documentation if no roxygen
entry is available."
  (interactive)
  (save-excursion
    (let* ((args-fun (ess-roxy-get-args-list-from-def))
	   (args-ent (ess-roxy-get-args-list-from-entry))
	   (args (ess-roxy-merge-args args-fun args-ent))
	   (line-break "")
	   here key template tag-def)
      (ess-roxy-goto-func-def)
      (if (not (= (forward-line -1) 0))
      	  (progn
	    (insert "\n")
	    (forward-line -1)))
      (if (and (not (looking-at "^\n")) (not (ess-roxy-entry-p)))
	  (progn
	    (end-of-line)
	    (insert "\n")))
      (if (ess-roxy-entry-p)
	  (progn
	    (setq here (1- (ess-roxy-delete-args)))
	    (ess-roxy-insert-args args here))
	(setq template (copy-sequence ess-roxy-template-alist))
	(while (stringp (car (car template)))
	  (setq tag-def (pop template))
	  (if (string= (car tag-def) "param")
	      (ess-roxy-insert-args args (point))
	    (if (string= (car tag-def) "description")
		(insert (concat line-break ess-roxy-str " " 
				(cdr tag-def) "\n" ess-roxy-str))
	      (if (string= (car tag-def) "details")
		  (insert (concat line-break ess-roxy-str " " (cdr tag-def)))
		(insert (concat line-break ess-roxy-str " @" 
				(car tag-def) " " (cdr tag-def))))
		))
	  (setq line-break "\n")
	  )))))

(defun ess-roxy-goto-end-of-entry ()
  "Put point at the top of the entry at point or above the
function at point. Return t if the point is left in a roxygen
entry, otherwise nil. Error if point is not in function or
roxygen entry."
  (if (not (ess-roxy-entry-p))
      (progn
	(goto-char (nth 0 (ess-end-of-function)))
	(forward-line -1)))
  (if (ess-roxy-entry-p)
      (progn
	(goto-char (ess-roxy-end-of-entry))
	t) (forward-line) nil))

(defun ess-roxy-goto-beg-of-entry ()
  "put point at the top of the entry at point or above the
function at point. Return t if the point is left in a roxygen
entry, otherwise nil. Error if point is not in function or
roxygen entry."
  (if (not (ess-roxy-entry-p))
      (progn
	(goto-char (nth 0 (ess-end-of-function)))
	(forward-line -1)))
  (if (ess-roxy-entry-p)
      (progn
	(goto-char (ess-roxy-beg-of-entry))
	t) (forward-line) nil))

(defun ess-roxy-delete-args ()
  "remove all args from the entry at point or above the function
at point. Return 0 if no deletions were made other wise the point
at where the last deletion ended"
  (save-excursion
    (let* ((args nil)
	   (cont t)
	   (field-beg 0)
	   entry-beg entry-end field-end)
      (ess-roxy-goto-end-of-entry)
      (setq entry-beg (ess-roxy-beg-of-entry))
      (setq entry-end (ess-roxy-end-of-entry))
      (goto-char entry-end)
      (beginning-of-line)
      (while (and (<= entry-beg (point)) (> entry-beg 0) cont)
	(if (looking-at
	     (concat "^" ess-roxy-str " *@param"))
	    (progn
	      (setq field-beg (ess-roxy-beg-of-field))
	      (setq field-end (ess-roxy-end-of-field))
	      (delete-region field-beg (+ field-end 1))))
	(setq cont nil)
	(if (= (forward-line -1) 0)
	    (setq cont t)))
      field-beg)))

(defun ess-roxy-get-args-list-from-entry ()
  "fill an args list from the entry above the function where the
point is"
  (save-excursion
    (let* (args entry-beg field-beg field-end args-text arg-name
	   desc)
      (if (ess-roxy-goto-end-of-entry)
	  (progn
	    (beginning-of-line)
	    (setq entry-beg (ess-roxy-beg-of-entry))
	    (while (and (< entry-beg (point)) (> entry-beg 0))
	      (if (looking-at
		   (concat "^" ess-roxy-str " *@param"))
		  (progn
		    (setq field-beg (ess-roxy-beg-of-field))
		    (setq field-end (ess-roxy-end-of-field))
		    (setq args-text (buffer-substring-no-properties
				     field-beg field-end))
		    (setq args-text
			  (ess-replace-in-string args-text
						 ess-roxy-str ""))
		    (setq args-text
			  (ess-replace-in-string
			   args-text "@param" ""))
		    (setq args-text
			  (ess-replace-in-string args-text "\n" ""))
		    (setq args-text (replace-regexp-in-string
				     "^ +" "" args-text))
		    (setq arg-name (replace-regexp-in-string
				    " .*" ""  args-text))
		    (setq desc (replace-regexp-in-string
				(concat "^" arg-name) "" args-text))
		    (setq desc (replace-regexp-in-string
				"^ +" "" desc))
		    (setq args (cons (list (concat arg-name)
					   (concat desc)) args))))
	      (forward-line -1))
	    args)
	nil))))

(defun ess-roxy-toggle-roxy-region (beg end)
  "Remove prefix roxy string in this region if point is in a roxy
region, otherwise prefix all lines with the roxy
string. Convenient for editing example fields."
  (interactive "r")
  (condition-case nil
      (if (not (ess-roxy-mark-active))
  	  (error "region is not active")))
  (save-excursion
    (let (RE to-string)
      (narrow-to-region beg end)
      (if (ess-roxy-entry-p)
	  (progn (setq RE (concat "^" ess-roxy-str " *"))
		 (setq to-string ""))
	(setq RE "^")
	(setq to-string (concat ess-roxy-str " ")))
      (goto-char beg)
      (while (re-search-forward RE (point-max) 'noerror)
	(replace-match to-string))
      (widen))))

(defun ess-roxy-preview-Rd (&optional arg)
  "Use the connected R session and the roxygen package to create
a preview of the Rd file of the entry at point. If called with
`arg' is non-nil (e.g. called with the universal argument), also
set the visited file name of the created buffer to allow for
saving (and using Rd-modes preview function) of the file."
  (interactive "P")
  (let ((beg (ess-roxy-beg-of-entry))
	(roxy-tmp (make-temp-file "ess-roxy"))
	(roxy-buf (get-buffer-create " *RoxygenPreview*"))
	beg-end)
    (if (= beg 0)
	(error "Point is not in a Roxygen entry"))
    (save-excursion
      (goto-char (ess-roxy-end-of-entry))
      (forward-line 1)
      (setq beg-end (ess-end-of-function))
      (append-to-file beg (car (cdr beg-end)) roxy-tmp)
      (ess-command "library(roxygen)\n" roxy-buf)
      (save-excursion
	(set-buffer roxy-buf)
	(goto-char 1)
	(if (search-forward-regexp "Error in library(roxygen)" nil t)
	    (error "Failed to load the roxygen package")))
      (ess-command ".ess_roxy_roclet <- make.Rd.roclet(NULL)\n")
      (ess-command (concat ".ess_roxy_roclet$parse(\"" roxy-tmp "\")\n") roxy-buf)
      (delete-file roxy-tmp))
    (pop-to-buffer roxy-buf)
    (if arg
	(save-excursion
	  (goto-char 1)
	  (search-forward-regexp "name{\\(.+\\)}")
	  (set-visited-file-name (concat (match-string 1) ".Rd"))))
    )(Rd-mode))

(defun ess-roxy-mark-active ()
  "True if region is active and transient mark mode activated"
  (if (fboundp 'region-active-p)
      (region-active-p)
    (and transient-mark-mode mark-active)))

(defun ess-roxy-hide-all ()
  "Hide all Roxygen entries in current buffer. "
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ess-roxy-str (point-max) t 1)
      (if (not (hs-already-hidden-p))
	  (hs-hide-block))
      (goto-char (ess-roxy-end-of-entry))
      (forward-line 1))))

(defun ess-roxy-previous-entry ()
  "Go to beginning of previous Roxygen entry. "
  (interactive)
  (if (ess-roxy-entry-p)
      (progn
	(goto-char (ess-roxy-beg-of-entry))
	(forward-line -1)))
  (search-backward ess-roxy-str (point-min) t 1)
  (goto-char (ess-roxy-beg-of-entry)))

(defun ess-roxy-next-entry ()
  "Go to beginning of next Roxygen entry. "
  (interactive)
  (if (ess-roxy-entry-p)
      (progn
	(goto-char (ess-roxy-end-of-entry))
	(forward-line 1)))
  (search-forward ess-roxy-str (point-max) t 1)
  (goto-char (ess-roxy-beg-of-entry)))

(defun ess-roxy-get-function-args ()
  "Return the arguments specified for the current function as a
list of strings."
  (save-excursion
    (let ((result)
	  (args-txt
	   (progn
	     (ess-beginning-of-function)
	     (buffer-substring-no-properties
	      (progn
		(search-forward-regexp "function *" nil nil 1)
		(+ (point) 1))
	      (progn
		(ess-roxy-match-paren)
		(point))))))
      (setq args-txt (replace-regexp-in-string "([^)]*)" "" args-txt))
      (setq args-txt (replace-regexp-in-string "=[^,]*" "" args-txt))
      (setq args-txt (replace-regexp-in-string "\n*" "" args-txt))
      (setq args-txt (replace-regexp-in-string " *" "" args-txt))
      (setq result (split-string args-txt ","))
       result)))

(defun ess-roxy-match-paren ()
  "Go to the matching parenthesis"
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

(defun ess-roxy-complete-tag ()
  "complete the tag at point"
  (let ((token-string (thing-at-point 'symbol)))
    (if (string-match "@.+" token-string)
	(progn 
	  (comint-dynamic-simple-complete 
	   (replace-regexp-in-string "^@" "" token-string)
	   (append ess-roxy-tags-noparam ess-roxy-tags-param))))))

;; advices
(defadvice ess-R-complete-object-name (around ess-roxy-complete-tag)
  (if (ess-roxy-entry-p)
      (ess-roxy-complete-tag)
    ad-do-it))
(defadvice ess-internal-complete-object-name (around ess-roxy-complete-tag)
  (if (ess-roxy-entry-p)
      (ess-roxy-complete-tag)
    ad-do-it))
(ad-activate 'ess-internal-complete-object-name)
(ad-activate 'ess-R-complete-object-name)

(defadvice mark-paragraph (around ess-roxy-mark-field)
  "mark this field"
  (if (and (ess-roxy-entry-p) (not mark-active))
      (progn
	(push-mark (point))
	(push-mark (1+ (ess-roxy-end-of-field)) nil t)
	(goto-char (ess-roxy-beg-of-field)))
    ad-do-it))
(ad-activate 'mark-paragraph)

(defadvice ess-indent-command (around ess-roxy-toggle-hiding)
  "hide this block if we are at the beginning of the line"
  (if (and (ess-roxy-entry-p) 'ess-roxy-hide-show-p)
      (progn (hs-toggle-hiding))
    ad-do-it))
(if ess-roxy-hide-show-p
    (ad-activate 'ess-indent-command))

(defadvice fill-paragraph (around ess-roxy-fill-advise)
  "Fill the current roxygen field."
  (if (ess-roxy-entry-p)
      (ess-roxy-fill-field)
    ad-do-it))
(ad-activate 'fill-paragraph)

(defadvice move-beginning-of-line (around ess-roxy-beginning-of-line)
  "move to start"
  (if (and (ess-roxy-entry-p)
	   (not (looking-back (concat ess-roxy-str " *\\="))))
      (progn
	(end-of-line)
	(re-search-backward (concat ess-roxy-str " *") (point-at-bol))
	(goto-char (match-end 0)))
    ad-do-it))
(ad-activate 'move-beginning-of-line)

(defadvice newline-and-indent (around ess-roxy-newline)
  "Insert a newline in a roxygen field."
  (if (ess-roxy-entry-p)
      (progn
	ad-do-it
	(insert (concat ess-roxy-str " ")))
    ad-do-it))
(ad-activate 'newline-and-indent)

(provide 'ess-roxy)
