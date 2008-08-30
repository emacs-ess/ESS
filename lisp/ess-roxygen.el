;;; ess-roxygen.el --- Insert roxygen tags for function definitions.
;;; 2008-08-23
;;; Proof of concept, Stephen Eglen

(defun ess-roxygen-fn ()
  "Insert  argument list template for the current function.
The template is inserted just before "
  (interactive)
    (save-excursion
    (let* ((beg-end (ess-end-of-function))
	   (beg (nth 0 beg-end))
	   (end (nth 1 beg-end))
	   (fn-regex "^\\(.+\\)\\s-*<-[ \t\n]*function[ ]*(")
	   names
	   args-beg args-end args-text
	   args
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
      (mapc 'ess-roxygen-print-one-param args)
      (insert "\n##' @return ...\n")
      ;;(setq s args)
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
