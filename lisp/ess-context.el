
(defun ess-completion-context-region (&optional pos)
  "Get a context at POS that could be used for completion.
Return a cons (beg . end)."
  ;; fixme: could (or should) this ever return invalid bounds or nil?
  (save-excursion
	(let ((pos (or pos (point))))
	  (goto-char pos)
      ;; pre-climb { first
      ;; fixme: this is a bit ugly, cannot we ess-climb-outside-calls skip {s?
      (let ((cont-pos (ess-containing-sexp-position)))
        (and (eq (char-after cont-pos) ?{)
             (goto-char cont-pos)))
	  (ess-climb-outside-calls)
	  (let ((bounds (ess-continuations-bounds)))
		(cons
		 (car bounds)
		 (max pos (cadr bounds)))))))

(defun ess-completion-context (&optional pos)
  "Get a context at POS that could be used for completion.
Return a string with ._. indicating the current position."
  (let* ((pos (or pos (point)))
		 (bounds (ess-completion-context-region pos)))
	(setcdr bounds (max pos (cdr bounds)))
	(concat (buffer-substring-no-properties (car bounds) pos)
			"._."
			(buffer-substring-no-properties pos (cdr bounds))
			(ess-missing-delimiters (car bounds) (cdr bounds)))))

(defun ess-missing-delimiters (&optional beg pos)
  ;; adaptation of http://emacs.stackexchange.com/a/915/5497
  (let ((beg (or beg (point-min)))
		(pos (or pos (point)))
		(closing nil))
    (save-excursion
	  (goto-char pos)
      (while
		  (and (< beg (point))
			   (condition-case nil
				   (progn
					 (backward-up-list)
					 (let ((syntax (syntax-after (point))))
					   (case (car syntax)
						 ((4) (setq closing (cons (cdr syntax) closing)))
						 ((7 8) (setq closing (cons (char-after (point)) closing)))))
					 t)
				 ((scan-error) nil)))))
    (apply #'string (nreverse closing))))
