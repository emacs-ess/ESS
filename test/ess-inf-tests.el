
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

(ert-deftest ess-startup-verbose-setwd-test ()
  (should (string-match "to quit R.\n\n> setwd(.*)$" (ess-r-tests-startup-output))))

(ert-deftest ess-startup-default-directory-preserved-test ()
  (let ((default-directory "foo")
        (ess-startup-directory temporary-file-directory)
        ess-ask-for-ess-directory)
    (with-r-running nil
      (should (string= default-directory "foo"))
      (should (string= (inferior-ess-default-directory) temporary-file-directory)))
    (should (string= default-directory "foo"))))


;;; Evaluation

(ert-deftest ess-command-test ()
  (with-r-running nil
    (let ((output-buffer (get-buffer-create " *ess-test-command-output*")))
      (ess-command "identity(TRUE)\n" output-buffer)
      (should (string= (with-current-buffer output-buffer
                         (ess-kill-last-line)
                         (buffer-string))
                       "[1] TRUE")))))

(ert-deftest ess-run-presend-hooks-test ()
  (with-r-running nil
    (let ((ess-presend-filter-functions (list (lambda (string) "\"bar\""))))
      (should (output= (ess-send-string (ess-get-process) "\"foo\"")
                       "[1] \"bar\"")))))

(ert-deftest ess-load-file-test ()
  (with-r-running nil
    (should (string-match "^\\[1\\] \"foo\"\nSourced file"
                          (output nil (ess-load-file "fixtures/file.R"))))))



;;; Inferior interaction

(ert-deftest ess-verbose-setwd-test ()
  (with-r-running nil
    (should (output= (ess-set-working-directory temporary-file-directory)
                     (format "setwd('%s')" temporary-file-directory)))))


;;; Sending input

(ert-deftest ess-inf-send-input-invisible-test ()
  (let ((ess-eval-visibly nil))
    (should (string= "> "
                     (ess-send-input-to-R "\n\n")))
    (should (string= "> "
                     (ess-send-input-to-R "invisible(0)")))
    (should (string= "invisible(0)\n> "
                     (ess-send-input-to-R "invisible(0)" 'repl)))))

(ert-deftest ess-inf-send-complex-input-test ()
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
      (should (string= output (ess-send-input-to-R input))))))

(ert-deftest ess-inf-send-fn-test ()
  (let ((input "fn <- function() {
}
")
        (output "> ")
        (output-nowait "> fn <- function() {
+ }
> "))
    (let ((inferior-ess-replace-long+ t))
      (let ((ess-eval-visibly nil))
        (should (string= output
                         (ess-send-input-to-R input 'c-c))))
      ;;; this fails randomly in batch
      ;; (let ((ess-eval-visibly 'nowait))
      ;;   (should (string= output-nowait
      ;;                    (ess-send-input-to-R input 'c-c))))
      )))

(ert-deftest ess-inf-send-cat-some.text-test ()
  (let ((input "cat(\"some. text\\n\")
head(cars, 2)
")
        (output "
some. text
> 
  speed dist
1     4    2
2     4   10
> ")
        (output-nowait "cat(\"some. text\\n\")
+ head(cars, 2)
some. text
> 
  speed dist
1     4    2
2     4   10
> "))
    (let ((inferior-ess-replace-long+ t))
      (let ((ess-eval-visibly nil))
        (should (string= output
                         (ess-send-input-to-R input 'c-c))))
      ;; these test fails randomly in batch
      ;; (let ((ess-eval-visibly 'nowait))
      ;;   (should (string= output-nowait
      ;;                    (ess-send-input-to-R input 'c-c))))
      )))


;;; Inferior utils

(ert-deftest ess-build-eval-command-test ()
  (should (not (ess-build-eval-command "command()")))
  (let ((ess-eval-command "%s - %f"))
    (should (string= (ess-build-eval-command "command(\"string\")" nil nil "file")
                     "command(\\\"string\\\") - file"))))

(ert-deftest ess-build-load-command-test ()
  (should (string= (ess-build-load-command "file")
                   "source('file')\n")))

;; Test runners

;; Note that we add R-3.2.1 to continuous integration via a symlink to
;; the actual R binary. These tests will likely fail locally for you
;; unless you have R-3.2.1 somewhere on `exec-path' when ESS was first
;; loaded. They're skipped unless you've defined the environment
;; variable CONTINUOUS_INTEGRATION or R-3.2.1 is found by
;; `executable-find'.
(when (> emacs-major-version 24)        ; ERT lacks `skip-unless' in Emacs 24
  (ert-deftest runner-R-3.2.1-defined ()
    (skip-unless (or (executable-find "R-3.2.1")
                     (getenv "CONTINUOUS_INTEGRATION")))
    (should (fboundp 'R-3.2.1)))

  (ert-deftest runner-R-3.2.1-buffer-name ()
    (skip-unless (or (executable-find "R-3.2.1")
                     (getenv "CONTINUOUS_INTEGRATION")))
    (should
     (string= "*R-3.2.1*"
              (let ((ess-use-inferior-program-in-buffer-name t)
                    (ess-gen-proc-buffer-name-function #'ess-gen-proc-buffer-name:simple)
                    (ess-ask-for-ess-directory nil)
                    (name))
                (R-3.2.1)
                (setq name (buffer-name))
                (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
                (kill-buffer name)
                name)))
    (should
     (string= "*R-3.2.1*"
              (let ((ess-use-inferior-program-name-in-buffer-name t)
                    (ess-gen-proc-buffer-name-function #'ess-gen-proc-buffer-name:simple)
                    (ess-ask-for-ess-directory nil)
                    (name))
                (R-3.2.1)
                (setq name (buffer-name))
                (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
                (kill-buffer name)
                name)))))
