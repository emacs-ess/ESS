
(require 'ert)
(require 'ess-site)
(require 'ess-r-tests-utils)


;;; Inferior R

(ert-deftest ess-r-build-eval-command ()
  (let ((command "command(\"string\")"))
    (should (string= (ess-r-build-eval-command command)
                     ".ess.eval('command(\"string\")', visibly = FALSE, output = FALSE)\n"))
    (should (string= (ess-r-build-eval-command command nil t)
                     ".ess.eval('command(\"string\")', visibly = FALSE, output = TRUE)\n"))
    (should (string= (ess-r-build-eval-command command t t "file.ext" "foo")
                     ".essDev.eval('command(\"string\")', visibly = TRUE, output = TRUE, package = 'foo', file = 'file.ext')\n"))
    (with-r-file nil
      (let ((command "command(\"string\")"))
        (should (string= (ess-build-eval-command command)
                         (ess-r-build-eval-command command)))))))

(ert-deftest ess-r-build-load-command ()
  (should (string= (ess-r-build-load-command "file.ext")
                   ".ess.source('file.ext', visibly = FALSE, output = FALSE); cat('Sourced file file.ext\n')"))
  (should (string= (ess-r-build-load-command "file.ext" t t)
                   ".ess.source('file.ext', visibly = TRUE, output = TRUE); cat('Sourced file file.ext\n')"))
  (should (string= (ess-r-build-load-command "file.ext" nil t "foo")
                   ".essDev_source('file.ext', visibly = FALSE, output = TRUE, package = 'foo'); cat('[foo] Sourced file file.ext\n')"))
  (with-r-file nil
    (should (string= (ess-build-load-command "file")
                     (ess-r-build-load-command "file")))))


;;; ess-r-package-mode

;; (ert-deftest ess-r-mode-line ()
;;   (with-r-file "dummy-pkg/R/test.R"
;;     (let ((mode-line (eval (plist-get ess-r-package-mode-line :eval))))
;;       (should (string= mode-line " [pkg:foo]")))

;;     (ess-r-set-evaluation-namespace "foo")
;;     (let ((mode-line (eval (plist-get ess-r-package-mode-line :eval))))
;;       (should (string= mode-line " [pkg:src:foo]")))

;;     (ess-r-set-evaluation-namespace "bar")
;;     (let ((pkg-mode-line (eval (plist-get ess-r-package-mode-line :eval)))
;;           (src-mode-line (eval (plist-get ess-r-special-evaluation-mode-line :eval))))
;;       (should (string= pkg-mode-line " [pkg:foo]"))
;;       (should (string= src-mode-line " [src:bar]")))

;;     (ess-r-set-evaluation-namespace '(4))
;;     (let ((mode-line (eval (plist-get ess-r-package-mode-line :eval))))
;;       (should (string= mode-line " [pkg:foo]")))))

