;;; This function will be added to ess-iw32.el

;;; Alternate version of ess-load-file, required with S+4.
;;; This version sends the S-Plus command
;;;         source("filename")
;;; to S.  This version does not guarantee to save .Last.value
;;; This version does not offer alternate buffers or editing capability.

;; C-c C-l
;;; this works for Sqpe+4 and S+4
(defun ess-load-file-ddeclient (filename)
  "Load an S source file into an inferior ESS process."
  (require 'ess-inf)
  (ess-make-buffer-current)
  (let ((source-buffer (get-file-buffer filename)))
    (if (ess-check-source filename)
	(error "Buffer %s has not been saved" (buffer-name source-buffer))
      ;; Find the process to load into
      (if source-buffer
	  (save-excursion
	    (set-buffer source-buffer)
    (ess-force-buffer-current "Process to load into: ")
	    (ess-check-modifications))))
    (ess-eval-visibly (format inferior-ess-load-command filename))))

(fset 'ess-load-file-original
      (symbol-function  'ess-load-file))

(defun ess-load-file (filename)
"Alternate version of ess-load-file, required with S+4.
This version sends the S-Plus command
     source("filename")
to S.  This version does not guarantee to save .Last.value
This version does not offer alternate buffers or editing capability."
     (interactive (list
		   (or
		    (and (eq major-mode 'ess-mode)
			 (buffer-file-name))
		    (expand-file-name
		     (read-file-name "Load S file: " nil nil t)))))
     (if (equal (ess-get-process-variable
		 ess-current-process-name 'inferior-ess-ddeclient)
		(default-value 'inferior-ess-ddeclient))
	 (ess-load-file-original filename)
       (ess-load-file-ddeclient filename))
     (widen))
