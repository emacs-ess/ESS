;;; ess-test-r-package.el --- ESS tests for R package functionality  -*- lexical-binding: t; -*-
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
;;

(require 'ert)
(require 'ess-r-mode)
(require 'ess-r-package)
(require 'ess-test-r-utils)

;;; Code:

(ert-deftest ess-r-package-auto-activation-test ()
  (let ((inhibit-message ess-inhibit-message-in-tests))
    (with-temp-buffer
      (text-mode)
      (hack-local-variables)
      (should (not ess-r-package-mode)))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (hack-local-variables)
      (should ess-r-package-mode))))

(ert-deftest ess-r-package-auto-activation-in-shell-test ()
  (skip-unless (executable-find "bash"))
  (let ((inhibit-message ess-inhibit-message-in-tests)
        (kill-buffer-query-functions nil)
        (explicit-shell-file-name "bash")
        (explicit-bash-args '("--noediting" "-i" "--norc" "--noprofile")))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (shell)
      (should ess-r-package-mode)
      (kill-buffer))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (let ((ess-r-package-auto-activate t))
        (shell)
        (should ess-r-package-mode))
      (kill-buffer))))

(ert-deftest ess-r-package-auto-no-activation-in-shell-test ()
  ;; FIXME: This test fails in batch in Emacs 27.
  (skip-unless (and (>= 27 emacs-major-version)
                    (not noninteractive)
                    (executable-find "bash")))
  (let ((kill-buffer-query-functions nil)
        (explicit-shell-file-name "bash")
        (explicit-bash-args '("--noediting" "-i" "--norc" "--noprofile")))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (let ((ess-r-package-exclude-modes '(shell-mode)))
        (shell)
        (should (not ess-r-package-mode))
        (kill-buffer)))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (let ((ess-r-package-auto-activate nil))
        (shell)
        (should (not ess-r-package-mode))
        (kill-buffer)))))

(ert-deftest ess-r-package-vars-test ()
  (with-ess-test-c-file "dummy-pkg/src/test.c"
    (let* ((inf-buf (run-ess-test-r-vanilla))
           (inf-proc (get-buffer-process inf-buf))
           (ess-local-process-name (process-name inf-proc)))
      (ess-test-unwind-protect inf-buf
        (let ((r-setwd-cmd (cdr (assq 'ess-setwd-command ess-r-customize-alist)))
              (r-getwd-cmd (cdr (assq 'ess-getwd-command ess-r-customize-alist))))
          (should (string= ess-setwd-command r-setwd-cmd))
          (should (string= ess-getwd-command r-getwd-cmd)))
        (let ((pkg-dir (file-truename (cdr (ess-r-package-project))))
              ;; Not sure why this is needed:
              ess-ask-for-ess-directory)
          (ess-set-working-directory (expand-file-name "src" pkg-dir))
          (ess-r-package-use-dir)
          (should (string= pkg-dir (file-truename
                                    (directory-file-name
                                     (ess-get-process-variable 'default-directory)))))
          (ess-wait-for-process)
          (should (string= pkg-dir (file-truename (ess-get-working-directory))))
          (ess-wait-for-process)
          (inferior-ess-reload)
          (should (string-match "Process R\\(:.\\)? \\(finished\\|killed\\)"
                                (with-current-buffer inf-buf
                                  (buffer-string)))))))))

(ert-deftest ess-r-package-package-info-test ()
  (let ((kill-buffer-query-functions nil)
        (ess-r-package-auto-activate nil))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (plist-get pkg-info :name) "foo"))
        (should (string-match-p "dummy-pkg$" (plist-get pkg-info :root)))
        (kill-buffer)))
    (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (plist-get pkg-info :name) "foo"))
        (should (string-match-p "^/mock:.*dummy-pkg$" (plist-get pkg-info :root)))
        (kill-buffer)))
    (with-ess-test-c-file "dummy-pkg/src/test.c"
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (plist-get pkg-info :name) "foo"))
        (should (string-match-p "dummy-pkg$" (plist-get pkg-info :root)))
        (kill-buffer)))))

(ert-deftest ess-r-package-project-test ()
  (with-ess-test-r-file "dummy-pkg/R/test.R"
    (let ((project-info (ess-r-package-project)))
      (should (equal 'ess-r-package (car project-info)))
      (should (string-match-p "dummy-pkg$" (cdr project-info)))
      (kill-buffer)))
  (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
    (let ((project-info (ess-r-package-project)))
      (should (equal 'ess-r-package (car project-info)))
      (should (string-match-p "^/mock:.*dummy-pkg$" (cdr project-info)))
      (kill-buffer))))

(ert-deftest ess-r-package-use-dir-test ()
  (with-ess-test-r-file "dummy-pkg/R/test.R"
    (ess-set-working-directory "/")
    (ess-wait-for-process)
    (should (string= (ess-get-working-directory) "/"))
    (ess-r-package-use-dir)
    (ess-wait-for-process)
    (should (string-match-p "dummy-pkg$" (ess-get-working-directory)))
    (kill-buffer))
  (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
    (should (string-match-p "/mock:.*/dummy-pkg/R/test.R" buffer-file-name))
    (ess-set-working-directory "/")
    (ess-wait-for-process)
    (should (string= (ess-get-working-directory) "/"))
    (ess-r-package-use-dir)
    (ess-wait-for-process)
    (should (string-match-p "dummy-pkg$" (ess-get-working-directory)))
    (kill-buffer)))

;; Return DIR unless both (i) DIR is the path of package root directory and (ii)
;; the buffer file name is in the tests/ package directory. When both (i) and
;; (ii) hold then return the path corresponding to tests/.
(ert-deftest inferior-ess-r--adjust-startup-directory-test ()
  (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/tests/example.R")
    (let* ((pkg-dir (plist-get (ess-r-package-info) :root))
           (inst-dir (expand-file-name "inst" pkg-dir))) ;; arbitrary non-package root directory choice
      (should (string-match-p "^/mock:.*/dummy-pkg/tests/$" default-directory))
      (should (string-match-p "^/mock:.*/dummy-pkg$" pkg-dir))
      (should (string= inst-dir
                       (inferior-ess-r--adjust-startup-directory inst-dir "R")))
      (should (string= default-directory
                       (inferior-ess-r--adjust-startup-directory pkg-dir "R"))))
    (kill-buffer)))

(ert-deftest ess-r-package-source-dirs-test ()
  (with-ess-test-r-file "dummy-pkg/R/test.R"
    (let ((source-dirs (ess-r-package-source-dirs)))
      (should (string-match-p ".*/dummy-pkg/R$" (car source-dirs)))
      (should (string-match-p ".*/dummy-pkg/src$" (car (cdr source-dirs))))
      (should (null (cdr (cdr source-dirs))))
      (kill-buffer)))
  (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
    (let ((source-dirs (ess-r-package-source-dirs)))
      (should (string-match-p "^/mock:.*/dummy-pkg/R$" (car source-dirs)))
      (should (string-match-p "^/mock:.*/dummy-pkg/src$" (car (cdr source-dirs))))
      (should (null (cdr (cdr source-dirs))))
      (kill-buffer))))

(ert-deftest ess-r-package-save-buffers-test ()
  ;; modify a file within an R package and try to save it
  (with-ess-test-r-file "dummy-pkg/R/test.R"
    (let ((new-path (expand-file-name "tmp.R")))
      (write-region "" nil new-path)
      (find-file new-path)
      (should (string-match-p ".*/dummy-pkg/R/tmp.R$" buffer-file-name))
      (should (not (buffer-modified-p)))
      (insert "# buffer update")
      (should (buffer-modified-p))
      (let ((ess-save-silently t))
        (ess-project-save-buffers)
        (should (string-match-p ".*/dummy-pkg/R/tmp.R$" buffer-file-name))
        (should (not (buffer-modified-p))))
      (kill-buffer "test.R")
      (kill-buffer "tmp.R")
      (delete-file new-path)))
  ;; modify a file within an R package with a remote path and try to save it
  (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
    (let ((new-path (expand-file-name "tmp.R")))
      (write-region "" nil new-path)
      (find-file new-path)
      (should (string-match-p "^/mock:.*/dummy-pkg/R/tmp.R$" buffer-file-name))
      (should (not (buffer-modified-p)))
      (insert "# buffer update")
      (should (buffer-modified-p))
      (let ((ess-save-silently t))
        (ess-project-save-buffers)
        (should (string-match-p "^/mock:.*/dummy-pkg/R/tmp.R$" buffer-file-name))
        (should (not (buffer-modified-p))))
      (kill-buffer "test.R")
      (kill-buffer "tmp.R")
      (delete-file new-path))))

(ert-deftest ess-r-package-eval-linewise-test ()
  (let ((output-regex "^# '.*/dummy-pkg'$"))
    ;; Test with an R package on a local filesystem
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (with-r-running (current-buffer)
        (let ((actual (output (ess-r-package-eval-linewise "# %s"))))
          (should (string-match-p output-regex actual))))
      (kill-buffer))
    ;; Test with an R package on a remote filesystem. The remote prefix portion
    ;; of the package location should be stripped from the command.
    (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
      (with-r-running (current-buffer)
        (should (string-match-p "^/mock:.*/dummy-pkg/R/test.R$" buffer-file-name))
        (let ((actual (output (ess-r-package-eval-linewise "# %s"))))
          (should (string-match-p output-regex actual))
          (should (not (string-match-p "/mock:" actual)))))
      (kill-buffer))))

(ert-deftest ess-r--flymake-parse-output-test ()
  (with-ess-test-r-file (ess-test-create-remote-path "dummy-pkg/R/test.R")
    (let ((ess-proj-file (expand-file-name "../.lintr"))
          (cur-dir-file (expand-file-name ".lintr")))
      ;; no .lintr file
      (should (null (ess-r--find-lintr-file)))
      ;; .lintr file in the package directory
      (write-region "" nil ess-proj-file)
      (let ((actual (ess-r--find-lintr-file)))
        (should (string-match-p "^/mock:.*/dummy-pkg/\\.lintr$" actual)))
      ;; .lintr file in the current directory takes precedence over any other
      ;; locations
      (write-region "" nil cur-dir-file)
      (let ((actual (ess-r--find-lintr-file)))
        (should (string-match-p "^/mock:.*/dummy-pkg/R/\\.lintr$" actual)))
      ;; clean up created files
      (delete-file ess-proj-file)
      (delete-file cur-dir-file))))

(provide 'ess-test-r-package)

;;; ess-test-r-package.el ends here
