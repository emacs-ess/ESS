
(require 'ert)
(require 'ess-site)
(require 'ess-r-tests-utils)


;;; Inferior R

(ert-deftest ess-build-eval-command:R ()
  (let ((command "command(\"string\")"))
    (should (string= (ess-build-eval-command:R command)
                     ".ess.eval(\"command(\"string\")\", visibly = FALSE, output = FALSE)\n"))
    (should (string= (ess-build-eval-command:R command nil t)
                     ".ess.eval(\"command(\"string\")\", visibly = FALSE, output = TRUE)\n"))
    (should (string= (ess-build-eval-command:R command t t "file.ext" "foo")
                     ".ess.ns_eval(\"command(\"string\")\", visibly = TRUE, output = TRUE, package = 'foo', file = 'file.ext')\n"))
    (with-r-file nil
      (let ((command "command(\"string\")"))
        (should (string= (ess-build-eval-command command)
                         (ess-build-eval-command:R (ess-quote-special-chars command))))))))

(ert-deftest ess-build-load-command:R ()
  (should (string= (ess-build-load-command:R "file.ext")
                   ".ess.source('file.ext', visibly = FALSE, output = FALSE)\n"))
  (should (string= (ess-build-load-command:R "file.ext" t t)
                   ".ess.source('file.ext', visibly = TRUE, output = TRUE)\n"))
  (should (string= (ess-build-load-command:R "file.ext" nil t "foo")
                   ".ess.ns_source('file.ext', visibly = FALSE, output = TRUE, package = 'foo')\n"))
  (with-r-file nil
    (should (string= (ess-build-load-command "file")
                     (ess-build-load-command:R "file")))))

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
