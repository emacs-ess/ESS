
(require 'ert)

;; As we use the R inferior for the generic tests
(require 'ess-r-tests-utils)


;;; Evaluation

(ert-deftest ess-evaluation ()
  (with-r-running nil

    ;; `ess-send-string'
    (should (output= (ess-send-string proc "TRUE") "[1] TRUE"))

    ;; `ess-command'
    (with-r-running nil
      (let ((output-buffer (get-buffer-create " *ess-test-command-output*")))
        (ess-command "identity(TRUE)\n" output-buffer)
        (should (string= (with-current-buffer output-buffer
                           (ess-kill-last-line)
                           (buffer-string))
                         "[1] TRUE"))))))


;;; Inferior utils

(ert-deftest ess-format-eval-command ()
  (should (not (ess-format-eval-command "command()")))
  (let ((ess-eval-command "%s - %f"))
    (should (string= (ess-format-eval-command "command(\"string\")" nil nil "file")
                     "command(\"string\") - file"))))

(ert-deftest ess-format-load-command ()
  (should (string= (ess-format-load-command "file")
                   "source('file')\n")))
