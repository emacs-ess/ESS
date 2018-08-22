;; ess-org-test.el --- Test for org-babel integration -*- lexical-binding: t -*-

(require 'ert)
(require 'ess-r-tests-utils)
(require 'ob-R)

(defun test-org-R-ouput (expect input)
  (declare (indent 1))
  (let ((proc (ess-vanila-R))
        (org-confirm-babel-evaluate nil)
        (ess-ask-for-ess-directory nil)
        (inhibit-message ess-inhibit-message-in-tests))
    (unwind-protect
        (with-current-buffer (get-buffer-create "*ess-org-test*")
          (erase-buffer)
          (insert input)
          (org-mode)
          (goto-char (point-min))
          (forward-line 1)
          (org-ctrl-c-ctrl-c)
          (goto-char (point-max))
          (should (re-search-backward expect nil t)))
      (kill-process proc))))

(ert-deftest test-org-ob-R-output ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :results output\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-session-output ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :session :results output\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-value ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :results value\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-session-value ()
  (test-org-R-ouput "hello"
    "#+BEGIN_SRC R :session :results value\n  \"hello\"\n#+END_SRC"))

(ert-deftest test-org-ob-R-data-frame ()
  (test-org-R-ouput "| 3 | c |"
    "#+BEGIN_SRC R :session :results value :colnames yes\n  data.frame(x=1:3, y=c(\"a\",\"b\",\"c\"))\n#+END_SRC"))

