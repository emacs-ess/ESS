;; ess-org-test.el --- Test for org-babel integration -*- lexical-binding: t -*-

(require 'ert)
(require 'ess-r-tests-utils)
(require 'ob-R)

(ert-deftest test-org-ob-R-output ()
  (let ((proc (ess-vanila-R))
        (org-confirm-babel-evaluate nil))
    (unwind-protect
        (with-temp-buffer
          (insert "
#+BEGIN_SRC R :session :results output
  print(\"hello\")
#+END_SRC
")
          (org-mode)
          (goto-char (point-min))
          (forward-line 2)
          (org-ctrl-c-ctrl-c)
          (goto-char (point-max))
          (should (looking-back "\"hello\"\n+")))
      (kill-process proc))))
