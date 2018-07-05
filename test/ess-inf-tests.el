
(require 'ert)

;; As we use the R inferior for the generic tests
(require 'ess-r-tests-utils)


;;; Startup

(defun ess-r-tests-startup-output ()
  (let* ((proc (ess-vanila-R))
         (output-buffer (process-buffer proc)))
    (unwind-protect
        (with-current-buffer output-buffer
          (buffer-string))
      (kill-process proc))))

(ert-deftest ess-startup-verbose-setwd ()
  (should (string-match "to quit R.\n\n> setwd(.*)$" (ess-r-tests-startup-output))))

(ert-deftest ess-startup-default-directory-preserved ()
  (let ((default-directory "foo")
        (ess-startup-directory temporary-file-directory)
        ess-ask-for-ess-directory)
    (with-r-running nil
      (should (string= default-directory "foo"))
      (should (string= (inferior-ess-default-directory) temporary-file-directory)))
    (should (string= default-directory "foo"))))


;;; Evaluation

(ert-deftest ess-evaluation ()
  ;; `ess-send-string'
  (with-r-running nil
    (should (output= (ess-send-string proc "TRUE") "[1] TRUE")))

  ;; `ess-command'
  (with-r-running nil
    (let ((output-buffer (get-buffer-create " *ess-test-command-output*")))
      (ess-command "identity(TRUE)\n" output-buffer)
      (should (string= (with-current-buffer output-buffer
                         (ess-kill-last-line)
                         (buffer-string))
                       "[1] TRUE")))))

(ert-deftest ess-run-presend-hooks ()
  (with-r-running nil
    (let ((ess-presend-filter-functions (list (lambda (string) "\"bar\""))))
      (should (output= (ess-send-string (ess-get-process) "\"foo\"")
                       "[1] \"bar\"")))))

(ert-deftest ess-load-file ()
  (with-r-running nil
    (should (string-match "^\\[1\\] \"foo\"\nSourced file"
                          (output nil (ess-load-file "fixtures/file.R"))))))



;;; Inferior interaction

(ert-deftest ess-verbose-setwd ()
  (with-r-running nil
    (should (output= (ess-set-working-directory temporary-file-directory)
              (format "setwd('%s')" temporary-file-directory)))))


;;; Sending input

(ert-deftest ess-inf-send-input-invisible ()
  (let ((ess-eval-visibly nil))
    (should (string= (ess-send-input-to-R "\n\n")
                     "> "))
    (should (string= (ess-send-input-to-R "invisible(0)")
                     "> "))
    (should (string= (ess-send-input-to-R "invisible(0)" t)
                     "invisible(0)\n> "))))

(ert-deftest ess-inf-send-complex-input ()
  (let ((ess-eval-visibly nil)
        (input "identity(
  identity(
    identity(
      head(mtcars[, c('wt', 'vs')], 3))))
'+ + + + > some-output >'
cat('+ + + + > cleaned-prompts >\n')
")
        (output "> 
                 wt vs
Mazda RX4     2.620  0
Mazda RX4 Wag 2.875  0
Datsun 710    2.320  1
> 
[1] \"+ + + + > some-output >\"
> 
cleaned-prompts >
> "))
    (let ((inferior-ess-replace-long+ t))
      (should (string=
               (ess-send-input-to-R input)
               output)))))



;;; Inferior utils

(ert-deftest ess-build-eval-command ()
  (should (not (ess-build-eval-command "command()")))
  (let ((ess-eval-command "%s - %f"))
    (should (string= (ess-build-eval-command "command(\"string\")" nil nil "file")
                     "command(\\\"string\\\") - file"))))

(ert-deftest ess-build-load-command ()
  (should (string= (ess-build-load-command "file")
                   "source('file')\n")))
