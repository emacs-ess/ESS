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
    (let ((r-setwd-cmd (cdr (assq 'ess-setwd-command ess-r-customize-alist)))
          (r-getwd-cmd (cdr (assq 'ess-getwd-command ess-r-customize-alist))))
      (should (string= ess-setwd-command r-setwd-cmd))
      (should (string= ess-getwd-command r-getwd-cmd)))
    (let ((pkg-dir (file-truename (cdr (ess-r-package-project))))
          ;; Not sure why this is needed:
          ess-ask-for-ess-directory)
      (ess-set-working-directory (expand-file-name "src" pkg-dir))
      (ess-r-package-use-dir)
      (should (string= pkg-dir (file-truename (directory-file-name default-directory))))
      (ess-wait-for-process)
      (should (string= pkg-dir (file-truename (ess-get-working-directory))))
      (ess-wait-for-process)
      (let ((proc-buffer (ess-get-process-buffer)))
        (inferior-ess-reload)
        (should (string-match "Process R\\(:.\\)? \\(finished\\|killed\\)"
                              (with-current-buffer proc-buffer
                                (buffer-string))))))))

(ert-deftest ess-r-package-package-info-test ()
  (let ((kill-buffer-query-functions nil)
        (ess-r-package-auto-activate nil))
    (with-ess-test-r-file "dummy-pkg/R/test.R"
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (plist-get pkg-info :name) "foo"))
        (should (string-match-p "dummy-pkg$" (plist-get pkg-info :root)))
        (kill-buffer)))
    (with-ess-test-c-file "dummy-pkg/src/test.c"
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (plist-get pkg-info :name) "foo"))
        (should (string-match-p "dummy-pkg$" (plist-get pkg-info :root)))
        (kill-buffer)))))

(provide 'ess-test-r-package)

;;; ess-test-r-package.el ends here
