
(require 'ert)

(defmacro ess-r-test (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  (apply #'ess-r-test- `(,file (,@body))))

(defun ess-r-test- (file body)
  (let ((ess-r-test-buffer (if file
                               (find-file-noselect file)
                             (generate-new-buffer " *ess-r-test-temp*"))))
    (save-window-excursion
      (switch-to-buffer ess-r-test-buffer)
      (R-mode)
      (mapc #'eval body)))
  nil)

(ert-deftest ess-r-mode-line ()
  (ess-r-test "dummy-pkg/R/test.R"
    (let ((mode-line (eval (plist-get ess-r-package-mode-line :eval))))
      (should (string= mode-line " [pkg:foo]")))

    (ess-r-select-evaluation-namespace "foo")
    (let ((mode-line (eval (plist-get ess-r-package-mode-line :eval))))
      (should (string= mode-line " [pkg:src:foo]")))

    (ess-r-select-evaluation-namespace "bar")
    (let ((pkg-mode-line (eval (plist-get ess-r-package-mode-line :eval)))
          (src-mode-line (eval (plist-get ess-r-special-evaluation-mode-line :eval))))
      (should (string= pkg-mode-line " [pkg:foo]"))
      (should (string= src-mode-line " [src:bar]")))

    (ess-r-select-evaluation-namespace '(4))
    (let ((mode-line (eval (plist-get ess-r-package-mode-line :eval))))
      (should (string= mode-line " [pkg:foo]")))))
