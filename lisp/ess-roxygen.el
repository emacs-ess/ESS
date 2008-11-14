;;; ess-roxygen.el --- Insert roxygen tags for function definitions.
;;; 2008-08-23
;;; Proof of concept, Stephen Eglen


;;; Commentary:

;; Within an R function, if you call M-x ess-roxygen-fn (bound to C-c
;; C-o) the roxygen function template is added just before the start
;; of the function, as shown below.  Note that two hashes are used for
;; each comment, to ensure that the ESS indendation mechanism keeps
;; the comments at the start of the line.

;; ##' @param trials
;; ##' @param verbose
;; ##' @param new.arg

;; ##' @return ...
;; new.argmcpi <- function(trials, verbose=FALSE,
;;                         new.arg=100) {
;;     hits <- 0                             #' Number of successfull trials

;;       for ( i in 1:trials ) {
;;             print(i)
;;           }
;;   }

;;; TODO:

;; perhaps the template could be added within the function definition,
;; so that by default the template is included within the range marked
;; by ess-mark-function.  (Or that function can be amended to include
;; the template.)

;;; Linking in to this existing elisp code would be great:
;; http://nschum.de/src/emacs/doc-mode/

(defun ess-roxygen-fn ()
  "Insert roxygen argument list template for the current function.
The template is currently inserted just before the function name."
  (interactive)
    (save-excursion
    (let* ((beg-end (ess-end-of-function))
	   (beg (nth 0 beg-end))
	   (end (nth 1 beg-end))
	   (fn-regex "^\\(.+\\)\\s-*<-[ \t\n]*function[ ]*(")
	   names
	   args-beg args-end args-text
	   args
	   buffer-start
	   name)

      ;; Go to start of function definition, read the name
      ;; and then skip over the function.
      (goto-char beg)
      (setq name (ess-read-object-name-default))
      (princ (concat "Roxygen found: " name) t)
      (re-search-forward fn-regex end)
      ;; handle case that end of regex not found?

      ;; args and now between point and the next end round paren.
      (setq args-beg (point))
      (search-forward ")")
      (setq args-end (1- (point)))

      ;; this is one long string of the arguments.
      (setq args-text (buffer-substring-no-properties args-beg args-end))

      ;; remove any whitespace from args.
      (setq args-text (ess-replace-in-string args-text "[ \t\n]" ""))

      ;; break string into list of arguments, and remove any
      ;; default value.
      (setq names (split-string args-text ","))
      (setq args
	    (mapcar
	     (lambda (x) (ess-replace-in-string x "=.*" "")) names))

      ;; Now insert the arguments ahead of the function.
      (goto-char (1- beg))
      (setq buffer-start (equal (point) (point-min)))
      (unless (or buffer-start (looking-at "^$"))
	;; newline needed if there is no blank line above the function.
	(insert "\n"))
      (mapc 'ess-roxygen-print-one-param args)
      (insert "\n##' @return ...")
      (when buffer-start 
	  (insert "\n"))
      )))

(defun ess-roxygen-print-one-param (p)
  "Insert parameter P to the roxygen comments."
  (insert (format "##' @param %s\n" p)))


;; Perhaps extract code that returns the args of the current function?
;; This would be a nice factorisation.
;; (defun ess-r-get-fn-args (keep-defaults)
;;   "Return the formal arguments of the current R function.
;; If KEEP-DEFAULTS is true, those are returned.")

(provide 'ess-roxygen)
