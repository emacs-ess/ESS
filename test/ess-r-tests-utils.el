
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

(defmacro with-r-running (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  `(apply #'with-r-running- (list ,file '(,@body))))

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
      (let* ((proc (save-window-excursion
                     (let ((ess-ask-for-ess-directory nil))
                       (R "--vanilla"))
                     (ess-get-process)))
             (ess-local-process-name (process-name proc))
             (prev-buffer (process-buffer proc))
             (output-buffer (get-buffer-create " *ess-r-tests-output*")))
        (with-current-buffer output-buffer
          (erase-buffer))
        (set-process-buffer proc output-buffer)
        (set-process-filter proc 'inferior-ess-ordinary-filter)
        (unwind-protect (prog1
                            ;; Returning the last value
                            (car (last (mapcar #'eval body)))
                          (ess-wait-for-process proc)
                          ;; Avoid getting "Process killed" in
                          ;; output-buffer
                          (set-process-buffer proc prev-buffer)
                          (with-current-buffer output-buffer
                            (ess-kill-last-line)
                            (buffer-string)))
          (kill-process proc))))))

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
     (with-current-buffer output-buffer
       (ess-kill-last-line)
       (prog1 (buffer-string)
         (erase-buffer)))))

(defmacro output= (body expected)
  (declare (indent 1) (debug (&rest body)))
  `(progn
     (let ((output (output ,body)))
       (if (string= output (eval ,expected))
           output
         ;; Probably a better way but this gets the job done
         (signal 'ert-test-failed (list (concat "Expected: \n" expected)
                                        (concat "Result: \n" output)))))))

(provide 'ess-r-tests-utils)
