;;; ess-test-inf.el --- ESS tests for inferiors  -*- lexical-binding: t; -*-
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/
;;
;;; Commentary:
;; Tests for inferior processes.

(require 'ert)
(require 'etest)
(require 'cl-lib)

;; As we use the R inferior for the generic tests
(require 'ess-test-r-utils)


;;*;; Startup

(defun ess-r-tests-startup-output ()
  (let ((inf-buf (run-ess-test-r-vanilla)))
    (ess-test-unwind-protect inf-buf
      (with-current-buffer inf-buf
        (buffer-string)))))

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

(ert-deftest ess-test-inferior-live-process-error ()
  (let* ((ess-gen-proc-buffer-name-function
          ;; Generate same inferior name each time
          (lambda (&rest args) "" "foo"))
         (error-msg "Can't start a new session in buffer `foo` because one already exists")
         (inf-buf (run-ess-test-r-vanilla)))
    (ess-test-unwind-protect inf-buf
      (should (string= (cadr (should-error (run-ess-test-r-vanilla)))
                       (format-message error-msg))))))

(ert-deftest ess-test-inferior-local-start-args ()
  (with-r-running nil
    (let ((inf-data (buffer-local-value 'inferior-ess--local-data *inf-buf*)))
      (should (equal (car inf-data) "R"))
      (should (equal (cdr inf-data) "--no-readline  --no-init-file --no-site-file")))))

(ert-deftest ess-test-inferior-reload-start-data ()
  (let* ((r-path (executable-find "R"))
         (inferior-ess-r-program r-path)
         (inf-buf (run-ess-test-r-vanilla)))
    (ess-test-unwind-protect inf-buf
      (let ((inf-data (buffer-local-value 'inferior-ess--local-data inf-buf)))
        (should (equal (car inf-data) r-path))
        (should (equal (cdr inf-data) "--no-readline  --no-init-file --no-site-file"))))))


;;*;; Evaluation

(etest-deftest ess-command-test ()
  "`ess-command' saves output in specified buffer."
  (let ((output-buffer (get-buffer-create " *ess-test-command-output*")))
    (ess-command "identity(TRUE)\n" output-buffer)
    (should (string= (with-current-buffer output-buffer
                       (ess-kill-last-line)
                       (buffer-string))
                     "[1] TRUE")))
  ;; No impact on inferior output
  :inf-result "")

(ert-deftest ess-async-command-test ()
  ;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
  ;;                    (lambda (proc) (message "done")))
  ;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
  ;;                    (lambda (proc) (message "done"))
  ;;                    t)
  ;; (ess-async-command "{cat(1:5);Sys.sleep(5);cat(2:6)}\n" nil (get-process "R")
  ;;                    (lambda (proc) (message "done"))
  (with-r-running nil
    (let ((inf-proc *proc*)
          semaphore)
      (ess-async-command "{cat(1:5);Sys.sleep(0.5);cat(2:6, '\n')}\n"
                         (get-buffer-create " *ess-async-text-command-output*")
                         inf-proc
                         (lambda (&rest args) (setq semaphore t)))
      (should (process-get inf-proc 'callbacks))
      (cl-loop repeat 3
               until (and semaphore (null (process-get inf-proc 'callbacks)))
               do (sleep-for 0 600)
               finally (should-not (process-get inf-proc 'callbacks))))))

(ert-deftest ess-run-presend-hooks-test ()
  (with-r-running nil
    (let ((ess-presend-filter-functions (list (lambda (string) "\"bar\""))))
      (should (output= (ess-send-string (ess-get-process) "\"foo\"")
                       "[1] \"bar\"")))))

(ert-deftest ess-load-file-test ()
  (with-r-running nil
    (should (string-match "^\\[1\\] \"foo\"\nSourced file"
                          (output nil (ess-load-file (expand-file-name "file.R" ess-test-fixtures-directory)))))))

(etest-deftest ess-command-incomplete-test ()
  "`ess-command' fails with incomplete input."
  (should-error (ess-command "list(" nil nil nil nil nil nil 0.01))

  ;; No impact on inferior output
  :inf-result ""

  ;; Process has been interrupted, is no longer busy, and we can run a
  ;; command again
  (should (inferior-ess-available-p)) 
  (should-error (ess-command "list(" nil nil nil nil nil nil 0.01))
  :inf-result "")

(etest-deftest ess-command-hanging-test ()
  "`ess-command' fails with hanging command."
  (should-error (ess-command "Sys.sleep(2)\n" nil nil nil nil nil nil 0.01))
  :inf-result ""
  (should (inferior-ess-available-p)))

(defun ess-test--browser ()
  (ess-send-string (ess-get-process) "{ browser(); NULL }\n")
  (ess-wait-for-process))

(defun ess-test--browser-cleanup ()
  (ess-debug-command-quit)
  (ess-wait-for-process)
  (etest-clear-inferior-buffer))

(etest-deftest ess--command-browser-timeout-test ()
  "`ess-command' fails with hanging command within browser (#1081)."
  :cleanup (ess-test--browser-cleanup)
  (ess-test--browser)
  :inf-result "Called from: top level 
Browse[1]> debug at #1: NULL
Browse[1]> "

  (should-error (ess-command "Sys.sleep(2)\n" nil nil nil nil nil nil 0.01))

  ;; No impact on inferior
  :inf-result ""
  (should (inferior-ess-available-p))

  ;; We're still in the browser
  (ess-send-string (ess-get-process) "NULL\n")
  (ess-wait-for-process)
  :inf-result "NULL
Browse[1]> ")

(etest-deftest ess-command-browser-curly-braces ()
  "`{` expressions when debugger is active do not interrupt command."
  :cleanup (ess-test--browser-cleanup)
  (ess-test--browser)
  :inf-result "Called from: top level 
Browse[1]> debug at #1: NULL
Browse[1]> "

  (should (string= (ess-string-command "{ 1; 2 }\n")
                   "[1] 2"))
  :inf-result ""
  (should (inferior-ess-available-p)))

(etest-deftest ess-command-environment ()
  "Can access current env with `.ess.environment()`"
  :cleanup (ess-test--browser-cleanup)
  (ess-send-string (ess-get-process)
                   "local({ foo <- 1; browser(); NULL })\n")
  (ess-wait-for-process)
  (should (string= (ess-string-command "ls(envir = .ess.environment())\n")
                   "[1] \"foo\"")))

(etest-deftest ess-command-quit-test ()
  "`ess-command' does not leak output on quit (#794, #842).
This is especially important within `while-no-input' used by
packages like eldoc and company-quickhelp. `throw-on-input' sets
`quit-flag'."
  ;; Simulate a quit by throwing from a timer
  (run-at-time 0.1 nil (lambda () (throw 'my-quit 'thrown)))
  (should (eq (catch 'my-quit
                (ess-command "{ cat('output\n'); Sys.sleep(10) }\n" nil nil nil nil nil nil 0.5))
              'thrown))
  ;; There should be no output after the early exit
  :inf-result ""
  (should (inferior-ess-available-p)))

(etest-deftest ess-command-quit-async-interrupt-test ()
  "`ess-command' interrupts asynchronously on quits (#1091, #1102).
Needed with slow-responding processes."
  (should (eq (identity (marker-buffer (process-mark (ess-get-process))))
              (ess-get-process-buffer)))
  (run-at-time 0.1 nil (lambda () (throw 'my-quit 'thrown)))
  (should (eq (catch 'my-quit
                (ess-command "{
                                       cat('output\n')
                                       withCallingHandlers(
                                         interrupt = function(...) Sys.sleep(0.2),
                                         Sys.sleep(10)
                                       )
                                     }
                                     "
                             nil nil nil nil nil nil 0.5))
              'thrown))
  ;; Wait for the async interrupt
  (should (ess-wait-for-process (ess-get-process) nil nil 0.5))
  ;; Check that marker buffer was properly restored
  (should (eq (marker-buffer (process-mark (ess-get-process)))
              (ess-get-process-buffer)))

  ;; There should be no output after the early exit or async restoration
  :inf-result "")

(etest-deftest ess-command-newlines-test ()
  "`ess-command' doesn't garble new lines (#1110)."
  (should (string= (ess--strip-final-newlines "1\n2")
                   "1\n2"))
  (should (equal (ess-get-words-from-vector "{ 'foo'\n'bar'\n }\n")
                 (list "bar"))))

(etest-deftest ess-command-multiline-test ()
  "`ess-command' output doesn't include continuation prompts (#1116)."
  (let ((buf (generate-new-buffer "ess-command-multiline-test")))
    (ess-command "{ 1\n 2 }\n" buf)
    (should (string= (with-current-buffer buf
                       (buffer-string))
                     "[1] 2"))))

(ert-deftest ess--command-output-info-test ()
  ;; No output
  (with-temp-buffer
    (insert "baz> ")
    (should (equal (ess--command-output-info (current-buffer))
                   (list 1 1 nil))))
  ;; Command output and no new output
  (with-temp-buffer
    (insert "
foo
bar
baz> ")
    (should (equal (ess--command-output-info (current-buffer))
                   (list 1 9 nil))))
  ;; Command output and new output
  (with-temp-buffer
    (insert "
foo
bar
baz> 

new output")
    (should (equal (ess--command-output-info (current-buffer))
                   (list 1 9 16)))))

(ert-deftest ess--command-delimited-output-info-test ()
  ;; No output
  (with-temp-buffer
    (insert "
my-sentinel-START
my-sentinel-END
baz> ")
    (should (equal (ess--command-delimited-output-info (current-buffer) "my-sentinel")
                   (list 20 20 nil))))
  ;; Command output and no new output
  (with-temp-buffer
    (insert "
my-sentinel-START
foo
bar
my-sentinel-END
baz> ")
    (should (equal (ess--command-delimited-output-info (current-buffer) "my-sentinel")
                   (list 20 27 nil))))
  ;; Command output and new output
  (with-temp-buffer
    (insert "
my-sentinel-START
foo
bar
my-sentinel-END
baz> 

new output")
    (should (equal (ess--command-delimited-output-info (current-buffer) "my-sentinel")
                   (list 20 27 49)))))

(etest-deftest command-without-trailing-newline-test ()
  "It is a bug when a command doesn't output a trailing newline.
With delimiters it might be possible to figure out the output.
However if they are not available then the output is
indistinguishable from the prompt."
  (should-error (ess-command "cat(1)\n"))
  (ess-wait-for-process)
  ;; Leaks output after the error but that seems fine since errors in
  ;; filters are bugs
  :inf-result "
> ")

(etest-deftest ess-command-intervening-input-test ()
  "Test that user can send input while command is interrupting (#1119)."
  (run-at-time 0.1 nil (lambda () (throw 'my-quit 'thrown)))
  (should (eq (catch 'my-quit
                (ess-command "{
                                cat('output\n')
                                withCallingHandlers(
                                  interrupt = function(...) Sys.sleep(0.2),
                                  Sys.sleep(10)
                                )
                              }
                              "
                             nil nil nil nil nil nil 0.5))
              'thrown))
  ;; Send intervening input right away. Since we wait on the R side
  ;; on interrupt, the process hasn't been restored yet
  (ess-send-string (ess-get-process) "cat('foobar\\n')\n")
  ;; Wait for the async interrupt
  (should (ess-wait-for-process nil nil nil 0.5))
  ;; The output for the intervening input should be shown in the
  ;; process buffer
  :inf-result "foobar
> ")

;;*;; Inferior interaction

(defmacro ess-test-interactive-eval (out-string &rest body)
  "Evaluate BODY in a temp buffer with `R-mode' on.
OUT-STRING is the content of the region captured by
`ess-send-region' function."
  (declare (indent 1) (debug (form body)))
  `(cl-letf (((symbol-function 'ess-force-buffer-current) #'ignore)
             ((symbol-function 'ess-get-process) #'ignore)
             ((symbol-function 'ess-process-get) #'ignore)
             ((symbol-function 'ess-send-region)
              (lambda (_ start end &rest _ignore)
                (should (string= (buffer-substring-no-properties start end)
                                 ,out-string)))))
     (with-temp-buffer
       (R-mode)
       ,@body)))

(ert-deftest ess-eval-paragraph-test ()
  (let ((inhibit-message ess-inhibit-message-in-tests))
    (let ((output "library(bbb)"))
      (ess-test-interactive-eval output
        (insert (format "## a comment\n\n%s\n\nmore_code4" output))
        (goto-char (point-min))
        (search-forward "lib")
        (ess-eval-paragraph)
        (should (looking-at-p "rary"))
        (ess-eval-region-or-function-or-paragraph)
        (should (looking-at-p "rary"))))))

(ert-deftest ess-eval-and-step-test ()
  (let ((inhibit-message ess-inhibit-message-in-tests))

    (let ((output "real <- code"))
      (ess-test-interactive-eval output
        (insert (format "## comment\n\n%s" output))
        (forward-line -1)
        (ess-eval-region-or-function-or-paragraph-and-step)))

    (let ((output "xyz <- function () {\n}"))
      (ess-test-interactive-eval output
        (insert (format "## comment\n\n%s" output))
        (goto-char (point-min))
        (forward-line 1)
        (ess-eval-region-or-function-or-paragraph-and-step)))

    (let ((output "xyz <- function () {\n}"))
      (ess-test-interactive-eval output
        (insert (format "## comment\n%s\nsome_code\n\nmore_code" output))
        (goto-char (point-min))
        (forward-line 1)
        (ess-eval-region-or-function-or-paragraph-and-step)
        (should (looking-at-p "some_code"))))

    (let ((output "a <- 1\nb <- 2"))
      (ess-test-interactive-eval output
        (insert (format "%s\n\nmore_code1()" output))
        (goto-char (point-min))
        (ess-eval-region-or-function-or-paragraph-and-step)
        (should (looking-at-p "more_code1"))))

    (let ((output "a <- 1\nb <- 2"))
      (ess-test-interactive-eval output
        (insert (format "%s\n\nmore_code2()" output))
        (goto-char (point-min))
        (ess-eval-region-or-function-or-paragraph-and-step)
        (should (looking-at-p "more_code2"))))

    (let ((output "library(aaaa)"))
      (ess-test-interactive-eval output
        (insert (format "%s\n\nmore_code3\n" output))
        (goto-char (point-min))
        (ess-eval-region-or-function-or-paragraph-and-step)
        (should (looking-at-p "more_code3"))))

    (let ((output "library(bbb)"))
      (ess-test-interactive-eval output
        (insert (format "## a comment\n\n%s\n\nmore_code4" output))
        (goto-char (point-min))
        (ess-eval-region-or-function-or-paragraph-and-step)
        (should (looking-at-p "more_code4"))))
    ))

(ert-deftest ess-setwd-test ()
  (with-r-running nil
    ;; Working directory is set verbosely
    (should (output= (ess-set-working-directory temporary-file-directory)
                     (format "setwd('%s')" temporary-file-directory)))
    ;; Update process default-directory but not caller's buffer
    (let* ((cur-dir default-directory)
           (temp-dir (file-name-as-directory (make-temp-file "setwd-dir" t))))
      (unwind-protect
          (progn
            (setq default-directory temp-dir)
            (ess-set-working-directory temporary-file-directory)
            (should (equal default-directory temp-dir))
            (should (equal (ess-get-process-variable 'default-directory)
                           temporary-file-directory))
            (ess-set-working-directory temp-dir)
            (should (equal (ess-get-process-variable 'default-directory)
                           temp-dir)))
        (setq default-directory cur-dir)))))


;;*;; Sending input

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
        (inferior-ess-fix-misaligned-output t)
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
  (let ((inferior-ess-fix-misaligned-output t)
        (input "cat(\"some. text\\n\")
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


;;*;; Help

(etest-deftest ess-help-aliases-test ()
  (let ((aliases (ess-get-help-aliases-list)))
    (should (member "list" aliases)))
  :inf-result ""

  (ess-help--reset-cache)
  (let ((aliases (ess-get-help-aliases-list)))
    (should (member "list" aliases))))


;;*;; Inferior utils

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

(ert-deftest ess--derive-connection-path ()
  (let* ((old-localname "/path/to/file")
         (new-localname "/home/username/projects")
         (connection-basic "/ssh:melancholia.danann.net:")
         (connection-ip4 "/ssh:10.0.1.213:")
         (connection-ip6 "/ssh:[::1]:")
         (connection-user "/scp:user@host:")
         (connection-port "/ssh:daniel@melancholia#42:")
         (connection-domain "/smb:daniel%BIZARRE@melancholia:")
         (connection-hop "/ssh:bird@bastion|ssh:news.my.domain:")
         (make-check-replacement
          (lambda (new-connection)
            (lambda (old-connection)
              (let ((old-path (concat old-connection old-localname))
                    (new-path (concat new-connection new-localname)))
                (string= (ess--derive-connection-path old-path new-path)
                         (concat old-connection new-localname))))))
         (check-new-localpath (funcall make-check-replacement ""))
         (check-new-remotepath (funcall make-check-replacement connection-basic)))
    (should (funcall check-new-localpath ""))
    (should (funcall check-new-localpath connection-basic))
    (should (funcall check-new-localpath connection-ip4))
    (should (funcall check-new-localpath connection-ip6))
    (should (funcall check-new-localpath connection-user))
    (should (funcall check-new-localpath connection-port))
    (should (funcall check-new-localpath connection-domain))
    (should (funcall check-new-localpath connection-hop))
    (should (funcall check-new-remotepath ""))
    (should (funcall check-new-remotepath connection-basic))))

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
  (let ((ess-use-inferior-program-in-buffer-name t)
        (ess-plain-first-buffername nil)
        (ess-gen-proc-buffer-name-function #'ess-gen-proc-buffer-name:simple)
        (ess-ask-for-ess-directory nil)
        (inhibit-message ess-inhibit-message-in-tests))
    (let ((inf-buf (R-3.2.1)))
      (ess-test-unwind-protect inf-buf
        (with-current-buffer inf-buf
          (should (string= (buffer-name) "*R-3.2.1:1*")))
        (let ((other-inf-buf (R-3.2.1)))
          (ess-test-unwind-protect other-inf-buf
            (with-current-buffer other-inf-buf
              (should (string= (buffer-name) "*R-3.2.1:2*")))))))))

(ert-deftest ess-switch-to-inferior-or-script-buffer-test ()
  (with-r-running nil
    (should (derived-mode-p 'ess-mode))
    (ess-switch-to-inferior-or-script-buffer nil)
    (should (derived-mode-p 'inferior-ess-mode))))


(provide 'ess-test-inf)

;; Local Variables:
;; etest-local-config: etest-r-config
;; End:

;;; ess-test-inf.el ends here
