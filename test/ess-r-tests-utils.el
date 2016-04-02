
(defmacro with-r-file (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  (apply #'with-r-file- `(,file (,@body))))

(defun with-r-file- (file body)
  (let ((r-file-buffer (if file
                           (find-file-noselect file)
                         (generate-new-buffer " *with-r-file-temp*"))))
    (save-window-excursion
      (switch-to-buffer r-file-buffer)
      (R-mode)
      (mapc #'eval body)))
  nil)

(defmacro with-r-running (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  (apply #'with-r-running- `(,file (,@body))))

(defun with-r-running- (file body)
  (let ((r-file-buffer (if file
                           (find-file-noselect file)
                         (generate-new-buffer " *with-r-file-temp*"))))
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
        (unwind-protect (progn
                          (mapc #'eval body)
                          (ess-wait-for-process proc)
                          ;; Avoid getting "Process killed" in
                          ;; output-buffer
                          (set-process-buffer proc prev-buffer)
                          (with-current-buffer output-buffer
                            (ess-kill-last-line)
                            (buffer-string)))
          (kill-process proc)))
      nil)))

(defun ess-kill-last-line ()
  (goto-char (point-max))
  (forward-line -1)
  (delete-region (point-at-eol) (point-max)))

;; The following retrieve the last output and clean the output
;; buffer. This is useful to continue testing without restarting a
;; fresh R session.

;; In case of other side effects than mere output, it's probably safer
;; to perform ulterior tests with a fresh R to avoid contaminating
;; them.

(defun output ()
  (with-current-buffer output-buffer
    (ess-kill-last-line)
    (prog1 (buffer-string)
      (erase-buffer))))

(defmacro output= (code expected)
  (eval code)
  (ess-wait-for-process proc)
  (let ((output (output)))
    (string= output (eval expected))))

(provide 'ess-r-tests-utils)
