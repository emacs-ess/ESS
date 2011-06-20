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
;;   - C-c C-e C-t :: create a preview of the Rd HTML file as generated
;;     using roxygen and the tools package
;;     
;; Known issues:
;;
;; - hideshow mode does not work very well. In particular, if ordinary
;;   comments precede a roxygen entry, then both will be hidden in the
;;   same overlay from start and not unfoldable using TAB since the
;;   roxygen prefix is not present. The planned solution is implement
;;   a replacement for hideshow.
;; - only limited functionality for S4 documentation. 

;; this *is* enabled now via ess-mode-hook in ./ess-site.el

(require 'ess-custom)
(require 'hideshow)

;; ------------------
(defvar ess-roxy-mode-map nil
  "Keymap for `ess-roxy' mode.")
(if ess-roxy-mode-map
    nil
  (setq ess-roxy-mode-map (make-sparse-keymap))
  (if ess-roxy-hide-show-p
      (define-key ess-roxy-mode-map (kbd "C-c C-e C-h") 'ess-roxy-hide-all))
  ;; short version (*first*: -> key binding shown in menu):
  (define-key ess-roxy-mode-map (kbd "C-c C-o") 'ess-roxy-update-entry)
  (define-key ess-roxy-mode-map (kbd "C-c C-e n")   'ess-roxy-next-entry)
  (define-key ess-roxy-mode-map (kbd "C-c C-e p")   'ess-roxy-previous-entry)
  ;; For consistency (e.g. C-c C-e C-h !): kept here *in* addition to above
  (define-key ess-roxy-mode-map (kbd "C-c C-e C-o") 'ess-roxy-update-entry)
  (define-key ess-roxy-mode-map (kbd "C-c C-e C-r")   'ess-roxy-preview-Rd)
  (define-key ess-roxy-mode-map (kbd "C-c C-e C-t")   'ess-roxy-preview-HTML)
  (define-key ess-roxy-mode-map (kbd "C-c C-e C-c") 'ess-roxy-toggle-roxy-region)
  )

(defconst ess-roxy-font-lock-keywords
  (eval-when-compile
    `((,(concat ess-roxy-str " *\\([@\\]"
		(regexp-opt ess-roxy-tags-param t)
		"\\)\\>")
       (1 'font-lock-keyword-face prepend))
      (,(concat ess-roxy-str " *\\([@\\]"
         (regexp-opt '("param") t)
         "\\)\\>\\(?:[ \t]+\\(\\sw+\\)\\)?")
       (1 'font-lock-keyword-face prepend)
       (3 'font-lock-variable-name-face prepend))
      (,(concat "[@\\]" (regexp-opt ess-roxy-tags-noparam t) "\\>")
       (0 'font-lock-variable-name-face prepend))
      (,(concat ess-roxy-str)
       (0 'bold prepend)))))

(define-minor-mode ess-roxy-mode
  "Minor mode for editing in-code documentation."
  :lighter " Rox"
  :keymap ess-roxy-mode-map
  (if ess-roxy-mode
      (progn
	(unless (featurep 'xemacs) ;; does not exist in xemacs:
	  (font-lock-add-keywords nil ess-roxy-font-lock-keywords))
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
    (unless (featurep 'xemacs)
      (font-lock-remove-keywords nil ess-roxy-font-lock-keywords)))
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
      (unless (string= (car arg-des) "")
	  (progn
	    (insert (concat "\n"
			    ess-roxy-str " @param " (car arg-des) " "))
	    (insert 
	     (ess-replace-in-string (concat (car (cdr arg-des))) "\n" 
				    (concat "\n" ess-roxy-str)))
	    (if ess-roxy-fill-param-p
		(ess-roxy-fill-field))
	    )))))

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
  "Update the entry at the point or the entry above the function
which the point is in. Add a template empty roxygen documentation
if no roxygen entry is available. The template can be customized
via the variable `ess-roxy-template-alist'. The parameter
descriptions can are filled if `ess-roxy-fill-param-p' is
non-nil."
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
			   args-text "[[:space:]]*@param *" ""))
		    ;; (setq args-text
		    ;; 	  (ess-replace-in-string args-text "\n" ""))
		    (string-match "[^[:space:]]*" args-text)
		    (setq arg-name (match-string 0 args-text))
		    (setq desc (replace-regexp-in-string
				(concat "^" arg-name " *") "" args-text))
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
      (narrow-to-region beg (- end 1))
      (if (ess-roxy-entry-p)
	  (progn (setq RE (concat "^" ess-roxy-str " *"))
		 (setq to-string ""))
	(setq RE "^")
	(setq to-string (concat ess-roxy-str " ")))
      (goto-char beg)
      (while (re-search-forward RE (point-max) 'noerror)
	(replace-match to-string))
      (widen))))

(defun ess-roxy-preview ()
  "Use the connected R session and the roxygen package to
generate the Rd code for entry at point, place it in a temporary
buffer and return that buffer."
  (let ((beg (ess-roxy-beg-of-entry))
	(roxy-tmp (make-temp-file "ess-roxy"))
	(roxy-buf (get-buffer-create " *RoxygenPreview*")))
    (if (= beg 0)
	(error "Point is not in a Roxygen entry"))
    (save-excursion
      (goto-char (ess-roxy-end-of-entry))
      (forward-line 1)
      (if (ess-end-of-function nil t)
	  (append-to-file beg (point) roxy-tmp)
	(while (and (forward-line 1) (not (looking-at "^$")) 
		    (not (looking-at ess-roxy-str))))
	(append-to-file beg (point) roxy-tmp))
      (ess-command "print(suppressWarnings(require(roxygen, quietly=TRUE)))\n"
		   roxy-buf)
      (with-current-buffer roxy-buf
	(goto-char 1)
	(if (search-forward-regexp "FALSE" nil t)
	    (error (concat "Failed to load the roxygen package; "
			   "in R, try  install.packages(\"roxygen\")"))))
      (ess-command (concat "make.Rd.roclet()$parse(\"" roxy-tmp "\")\n") roxy-buf))
    (delete-file roxy-tmp)
    roxy-buf))

(defun ess-roxy-preview-HTML (&optional visit-instead-of-open)
  "Use the connected R session and the roxygen package to
generate a HTML page for the roxygen entry at point and open that
buffer in a browser. Visit the HTML file instead of showing it in
a browser if `visit-instead-of-open' is non-nil"
  (interactive "P")
  (let ((roxy-buf (ess-roxy-preview))
	(rd-tmp-file (make-temp-file "ess-roxy-" nil ".Rd"))
	(html-tmp-file (make-temp-file "ess-roxy-" nil ".html")))
    (with-current-buffer roxy-buf
      (set-visited-file-name rd-tmp-file)
      (save-buffer)
      (kill-buffer roxy-buf))
    (ess-command "print(suppressWarnings(require(tools, quietly=TRUE)))\n")
    (if (not visit-instead-of-open)
	(ess-command 
	 (concat "browseURL(Rd2HTML(\"" rd-tmp-file "\",\"" 
		 html-tmp-file "\", stages=c(\"render\")))\n"))
      (ess-command 
       (concat "Rd2HTML(\"" rd-tmp-file "\",\"" 
	       html-tmp-file "\", stages=c(\"render\"))\n"))
      (find-file html-tmp-file))))

(defun ess-roxy-preview-Rd (&optional name-file)
  "Use the connected R session and the roxygen package to
generate the Rd code for the roxygen entry at point. If called
with a non-nil `name-file' (e.g. universal argument C-u),
also set the visited file name of the created buffer to
facilitate saving that file."
  (interactive "P")
  (let ((roxy-buf (ess-roxy-preview)))
    (pop-to-buffer roxy-buf)
    (if name-file
	(save-excursion
	  (goto-char 1)
	  (search-forward-regexp "name{\\(.+\\)}")
	  (set-visited-file-name (concat (match-string 1) ".Rd"))))
    (Rd-mode)))

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
    (let ((args-txt
	   (progn
	     (ess-beginning-of-function)
	     (buffer-substring-no-properties
	      (progn
		(search-forward-regexp "\\([=,-]+ *function *\\|^\s*function\\)" nil nil 1)
		(+ (point) 1))
	      (progn
		(ess-roxy-match-paren)
		(point))))))
      (setq args-txt (replace-regexp-in-string "#+.*\n" "" args-txt))
      (setq args-txt (replace-regexp-in-string "([^)]+)" "" args-txt))
      (setq args-txt (replace-regexp-in-string "=[^,]+" "" args-txt))
      (setq args-txt (replace-regexp-in-string "[ \t\n]+" "" args-txt))
      (split-string args-txt ","))))

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
