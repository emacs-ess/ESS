;; ess-r-tests-utils.el --- Various utilities for ess R tests

(defmacro with-r-file (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  `(apply #'with-r-file- (list ,file '(,@body))))

(defun with-r-file- (file body)
  (let ((r-file-buffer (if file
                           (find-file-noselect file)
                         (generate-new-buffer " *with-r-file-temp*"))))
    (save-window-excursion
      (switch-to-buffer r-file-buffer)
      (R-mode)
      (mapcar #'eval body))))

;; Borrowed from org
(defmacro ess-r-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with `R-mode' as the active
mode holding TEXT.  If the string \"¶\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	 (org-mode-hook nil))
     (with-temp-buffer
       (R-mode)
       (let ((point (string-match "¶" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       ,@body)))
(def-edebug-spec org-test-with-temp-text (form body))

(defun ess-vanila-R ()
  "Start vanila R process and return the process object."
  (save-window-excursion
    (let ((ess-ask-for-ess-directory nil))
      (R "--vanilla"))
    (ess-get-process)))

(defmacro with-r-running (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  `(apply #'with-r-running- (list ,file '(,@body))))

(defvar ess-r-tests-current-output-buffer nil)

(defun with-r-running- (file body)
  (let ((r-file-buffer (cond ((bufferp file)
                              file)
                             ((stringp file)
                              (find-file-noselect file))
                             (t
                              (generate-new-buffer " *with-r-file-temp*")))))
    (save-window-excursion
      (switch-to-buffer r-file-buffer)
      (R-mode)
      (let* ((proc (ess-vanila-R))
             (ess-local-process-name (process-name proc))
             (output-buffer (process-buffer proc)))
        (unwind-protect
            (progn
              (setq ess-r-tests-current-output-buffer (process-buffer proc))
              (let ((inhibit-read-only t))
                (with-current-buffer ess-r-tests-current-output-buffer
                  (erase-buffer)))
              (set-process-filter proc 'inferior-ess-ordinary-filter)
              (prog1 (car (last (mapcar #'eval body)))
                (ess-wait-for-process proc)))
          (kill-process proc)
          (setq ess-r-tests-current-output-buffer nil))))))

;; The following retrieve the last output and clean the output
;; buffer. This is useful to continue testing without restarting a
;; fresh R session.

;; In case of other side effects than mere output, it's probably safer
;; to perform ulterior tests with a fresh R to avoid contaminating
;; them.

(defmacro output (&rest body)
  (declare (indent 1) (debug (&rest body)))
  `(progn
     ,@body
     (ess-wait-for-process proc)
     (with-current-buffer ess-r-tests-current-output-buffer
       (ess-kill-last-line)
       (prog1 (buffer-string)
         (erase-buffer)))))

(defmacro output= (body expected)
  (declare (indent 1) (debug (&rest body)))
  `(progn
     (let ((output (output ,body))
           (expected (eval ,expected)))
       (if (string= output expected)
           output
         ;; Probably a better way but this gets the job done
         (signal 'ert-test-failed (list (concat "Expected: \n" expected)
                                        (concat "Result: \n" output)))))))

(provide 'ess-r-tests-utils)
