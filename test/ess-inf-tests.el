;; -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

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
  (let ((default-directory user-emacs-directory)
        (ess-startup-directory temporary-file-directory)
        ess-ask-for-ess-directory)
    (with-r-running nil
      (should (string= default-directory user-emacs-directory))
      (should (string= (inferior-ess-default-directory) temporary-file-directory)))
    (should (string= default-directory user-emacs-directory))))


;;; Evaluation

(ert-deftest ess-command-test ()
  (with-r-running nil
    (let ((output-buffer (get-buffer-create " *ess-test-command-output*")))
      (ess-command "identity(TRUE)\n" output-buffer)
      (should (string= (with-current-buffer output-buffer
                         (ess-kill-last-line)
                         (buffer-string))
                       "[1] TRUE")))))

(ert-deftest ess-async-command-test ()
  ;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
  ;;                    (lambda (proc) (message "done")))
  ;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
  ;;                    (lambda (proc) (message "done"))
  ;;                    t)
  ;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
  ;;                    (lambda (proc) (message "done"))
  (with-r-running nil
    (lexical-let (semaphore)                  
      (ess-async-command "{cat(1:5);Sys.sleep(0.5);cat(2:6)}\n"
                         (get-buffer-create " *ess-async-text-command-output*")
                         (get-process "R")
                         (lambda (&rest args) (setq semaphore t)))
      (should (process-get (get-process "R") 'callbacks))
      (loop repeat 3
            until (and semaphore (null (process-get (get-process "R") 'callbacks)))
            do (sleep-for 0 600)
            finally (should-not (process-get (get-process "R") 'callbacks))))))

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

(defmacro ess-test-interactive-eval (out-string &rest body)
  "Evaluate BODY in a temp buffer with R-mod eon.
OUT-STRING is the content of the region captured by
`ess-send-region' function."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'ess-force-buffer-current) #'ignore)
             ((symbol-function 'ess-get-process) #'ignore)
             ((symbol-function 'ess-send-region)
              (lambda (_ start end &rest _ignore)
                (should (string= (buffer-substring-no-properties start end)
                                 ,out-string)))))
     (with-temp-buffer
       (R-mode)
       ,@body)))

(ert-deftest ess-eval-and-step-test ()
  (let ((inhibit-message ess-inhibit-message-in-tests))

    (let ((output "\nreal <- code\n"))
      (ess-test-interactive-eval output
        (insert (format "## comment\n%s" output))
        (forward-line -1)
        (ess-eval-region-or-function-or-paragraph-and-step)))

    (let ((output "\nxyz <- function {\n}\n"))
      (ess-test-interactive-eval output
        (insert (format "## comment\n%s" output))
        (goto-char (point-min))
        (forward-line 1)
        (ess-eval-region-or-function-or-paragraph-and-step)))

    (let ((output "a <- 1\nb <- 2\n"))
      (ess-test-interactive-eval output
        (insert (format "%s\nmore_code()" output))
        (goto-char (point-min))
        (ess-eval-region-or-function-or-paragraph-and-step)
        (should (looking-at-p "more_code"))))))

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
        (output "some. text
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
  (let ((ess-dialect nil))
    (should (string= (ess-build-load-command "file")
                     "source('file')\n"))))

(ert-deftest ess-get-words-from-vector-test ()
  (with-r-running nil
    (should (cl-every #'string= (ess-get-words-from-vector "c('1')\n") '("1")))
    (should (cl-every #'string= (ess-get-words-from-vector "c('1', \"2\")\n") '("1" "2")))
    (should (cl-every #'string= (ess-get-words-from-vector "c('aaa','bbb\"ccc', 'dddd')\n")
                      '("aaa" "bbb\\\"ccc" "dddd")))))

;; Test runners

;; Note that we add R-3.2.1 to continuous integration via a symlink to
;; the actual R binary. These tests will likely fail locally for you
;; unless you have R-3.2.1 somewhere on `exec-path' when ESS was first
;; loaded. They're skipped unless you've defined the environment
;; variable CONTINUOUS_INTEGRATION or R-3.2.1 is found by
;; `executable-find'.
(ert-deftest runner-R-3.2.1-defined-test ()
  (skip-unless (or (executable-find "R-3.2.1")
                   (getenv "CONTINUOUS_INTEGRATION")))
  (should (fboundp 'R-3.2.1)))

(ert-deftest runner-R-3.2.1-buffer-name-test ()
  (skip-unless (and (or (executable-find "R-3.2.1")
                        (getenv "CONTINUOUS_INTEGRATION"))
                    (> emacs-major-version 25)))
  (should
   (string= "*R-3.2.1:1*"
            (let ((ess-use-inferior-program-in-buffer-name t)
                  (ess-plain-first-buffername nil)
                  (ess-gen-proc-buffer-name-function #'ess-gen-proc-buffer-name:simple)
                  (ess-ask-for-ess-directory nil)
                  (name))
              (R-3.2.1)
              (setq name (buffer-name))
              (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
              (kill-process (get-buffer-process name))
              ;; FIXME: Does not work in batch mode
              ;; (kill-buffer name)
              name)))
  (should
   (string= "*R-3.2.1:2*"
            (let ((ess-use-inferior-program-name-in-buffer-name t)
                  (ess-plain-first-buffername nil)
                  (ess-gen-proc-buffer-name-function #'ess-gen-proc-buffer-name:simple)
                  (ess-ask-for-ess-directory nil)
                  (name))
              (R-3.2.1)
              (setq name (buffer-name))
              (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
              (kill-process (get-buffer-process name))
              ;; (kill-buffer name)
              name))))
