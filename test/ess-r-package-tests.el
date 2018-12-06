(require 'ert)
(require 'ess-r-mode)
(require 'ess-r-package)
(require 'ess-r-tests-utils)

(ert-deftest ess-r-package-auto-activation-test ()
  (let ((inhibit-message ess-inhibit-message-in-tests))
    (with-temp-buffer
      (text-mode)
      (hack-local-variables)
      (should (not ess-r-package-mode)))
    (with-r-file "dummy-pkg/R/test.R"
      (hack-local-variables)
      (should ess-r-package-mode))))

(ert-deftest ess-r-package-auto-activation-in-shell-test ()
  (let ((kill-buffer-query-functions nil))
    (with-r-file "dummy-pkg/R/test.R"
      (shell)
      (should ess-r-package-mode)
      (kill-buffer))
    (with-r-file "dummy-pkg/R/test.R"
      (let ((ess-r-package-auto-activate t))
        (shell)
        (should ess-r-package-mode))
      (kill-buffer))))

(ert-deftest ess-r-package-auto-no-activation-in-shell-test ()
  (let ((kill-buffer-query-functions nil))
    (with-r-file "dummy-pkg/R/test.R"
      (let ((ess-r-package-exclude-modes '(shell-mode)))
        (shell)
        (should (not ess-r-package-mode))
        (kill-buffer)))
    (with-r-file "dummy-pkg/R/test.R"
      (let ((ess-r-package-auto-activate nil))
        (shell)
        (should (not ess-r-package-mode))
        (kill-buffer)))))

(ert-deftest ess-r-package-vars-test ()
  (with-c-file "dummy-pkg/src/test.c"
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
    (with-r-file "dummy-pkg/R/test.R"
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (alist-get :name pkg-info) "foo"))
        (should (string-match-p "dummy-pkg$" (alist-get :root pkg-info)))
        (kill-buffer)))
    (with-c-file "dummy-pkg/src/test.c"
      (let ((pkg-info (ess-r-package-info)))
        (should (string= (alist-get :name pkg-info) "foo"))
        (should (string-match-p "dummy-pkg$" (alist-get :root pkg-info)))
        (kill-buffer)))))
