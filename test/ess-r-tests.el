
(require 'ert)
(require 'ess-site)
(require 'ess-r-tests-utils)


;;; Inferior R

(ert-deftest ess-r-build-eval-command ()
  (let ((command "command(\"string\")"))
    (should (string= (ess-r-build-eval-command command)
                     ".ess.eval(\"command(\"string\")\", visibly = FALSE, output = FALSE)\n"))
    (should (string= (ess-r-build-eval-command command nil t)
                     ".ess.eval(\"command(\"string\")\", visibly = FALSE, output = TRUE)\n"))
    (should (string= (ess-r-build-eval-command command t t "file.ext" "foo")
                     ".essDev.eval(\"command(\"string\")\", visibly = TRUE, output = TRUE, package = 'foo', file = 'file.ext')\n"))
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

(ert-deftest ess-r-send-single-quoted-strings ()
  (with-r-running nil
    (insert "'hop'\n")
    (let (ess-eval-visibly)
      (should (output= (ess-eval-buffer nil)
                       "[1] \"hop\"")))))

(ert-deftest ess-r-send-double-quoted-strings ()
  (with-r-running nil
    (insert "\"hop\"\n")
    (let (ess-eval-visibly)
      (should (output= (ess-eval-buffer nil)
                       "[1] \"hop\"")))))


;;; ess-r-package-mode

(ert-deftest ess-r-package-auto-activate ()
  (with-temp-buffer
    (text-mode)
    (hack-local-variables)
    (should (not ess-r-package-mode)))
  (with-r-file "dummy-pkg/R/test.R"
    (hack-local-variables)
    (should ess-r-package-mode)))
