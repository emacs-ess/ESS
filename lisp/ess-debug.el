;; Strictly for debugging and development

(defun ess-add-path (path &rest options)
  "Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `defaul-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'.

For ESS, ONLY use load-path, since Emacs doesn't have
default-load-path."

  (let ((rest load-path)
	p)
    (if (and (catch 'tag
	       (while rest
		 (setq p (expand-file-name path (car rest)))
		 (if (file-directory-p p)
		     (throw 'tag p)
		   )
		 (setq rest (cdr rest))
		 ))
	     (not (member p load-path))
	     )
	(setq load-path
	      (if (memq 'append options)
		  (append load-path (list p))
		(cons p load-path)
		))
      )))

(setq-default debug-on-error t)
(ess-add-path "/p1/apps/X11R6.3/lib/xemacs/site-lisp/ESS/")
(require 'ess-site)
