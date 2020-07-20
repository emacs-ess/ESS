;; ess-test-org.el --- Test for org-babel integration -*- lexical-binding: t -*-
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
;; Tests for org babel functionality.

;;; Code:

(require 'ert)
(require 'ess-r-mode)
(require 'ess-test-r-utils)
(require 'ob-R)

;; ob-R let-binds `ess-local-process-name' to the process of the
;; session buffer. This session buffer is set to `*R*` by default. It
;; is safer to pass a dedicated inferior buffer as `:session' keyword

(defun test-org-R-ouput (expect input)
  (declare (indent 1))
  (let* ((inf-buf (run-ess-test-r-vanilla))
         (inf-proc (get-buffer-process inf-buf)))
    (setq input (format input (buffer-name inf-buf)))
    (ess-test-unwind-protect inf-buf
      (with-current-buffer (get-buffer-create "*ess-org-test*")
        (let ((org-confirm-babel-evaluate nil)
              (ess-ask-for-ess-directory nil)
              (inhibit-message ess-inhibit-message-in-tests))
          (erase-buffer)
          (insert input)
          (org-mode)
          (goto-char (point-min))
          (forward-line 1)
          (org-ctrl-c-ctrl-c)
          (goto-char (point-max))
          (should (re-search-backward expect nil t)))))))

(ert-deftest test-org-ob-R-output-test ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :results output\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-session-output-test ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :session %s :results output\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-value-test ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :results value\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-session-value-test ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :session %s :results value\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-data-frame-test ()
  (test-org-R-ouput "| 3 | c |"
    "#+BEGIN_SRC R :session :results value :colnames yes\n  data.frame(x=1:3, y=c(\"a\",\"b\",\"c\"))\n#+END_SRC"))


(provide 'ess-test-org)

;;; ess-test-org.el ends here
