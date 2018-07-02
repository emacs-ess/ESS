
(require 'ert)
(require 'ess-site)
(require 'ess-r-tests-utils)


;;; R

(ert-deftest ess-r-inherits-prog-mode ()
  (let ((prog-mode-hook (lambda () (setq ess-test-prog-hook t))))
    (with-r-file nil
      (should (derived-mode-p 'prog-mode))
      (should ess-test-prog-hook)
      (should
       ;; Test that prog-mode-map is a keymap-parent
       (let ((map (current-local-map))
             found)
         (while (and map (not found))
           (if (eq (keymap-parent map) prog-mode-map)
               (setq found t)
             (setq map (keymap-parent map))))
         found)))))

(ert-deftest ess-build-eval-command:R ()
  (let ((command "command(\"string\")"))
    (should (string= (ess-build-eval-command:R command)
                     ".ess.eval(\"command(\"string\")\", visibly = FALSE, output = FALSE)\n"))
    (should (string= (ess-build-eval-command:R command nil t)
                     ".ess.eval(\"command(\"string\")\", visibly = FALSE, output = TRUE)\n"))
    (should (string= (ess-build-eval-command:R command t t "file.ext" "foo")
                     ".ess.ns_eval(\"command(\"string\")\", visibly = TRUE, output = TRUE, package = 'foo', verbose = TRUE, file = 'file.ext')\n"))
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
                   ".ess.ns_source('file.ext', visibly = FALSE, output = TRUE, package = 'foo', verbose = TRUE)\n"))
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

(ert-deftest ess-set-working-directory ()
  (with-r-running nil
    (ess-set-working-directory "/")
    (ess-eval-linewise "getwd()" 'invisible)
    (should (output= (ess-eval-buffer nil)
              "setwd('/')\n> [1] \"/\""))
    (should (string= default-directory "/"))))


;;; ess-r-package-mode

(ert-deftest ess-r-package-auto-activation ()
  (with-temp-buffer
    (text-mode)
    (hack-local-variables)
    (should (not ess-r-package-mode)))
  (with-r-file "dummy-pkg/R/test.R"
    (hack-local-variables)
    (should ess-r-package-mode)))

(ert-deftest ess-r-package-auto-activation-in-eshell ()
  (with-r-file "dummy-pkg/R/test.R"
    (eshell)
    (should ess-r-package-mode)
    (kill-buffer))
  (with-r-file "dummy-pkg/R/test.R"
    (let ((ess-r-package-auto-activate t))
      (eshell)
      (should ess-r-package-mode))
    (kill-buffer)))

(ert-deftest ess-r-package-auto-no-activation-in-eshell ()
  (with-r-file "dummy-pkg/R/test.R"
    (let ((ess-r-package-exclude-modes '(eshell-mode)))
      (eshell)
      (should (not ess-r-package-mode))
      (kill-buffer)))
  (with-r-file "dummy-pkg/R/test.R"
    (let ((ess-r-package-auto-activate nil))
      (eshell)
      (should (not ess-r-package-mode))
      (kill-buffer))))

;;; Namespaced evaluation

(ert-deftest ess-r-run-presend-hooks ()
  (with-r-running nil
    (let ((ess-presend-filter-functions (list (lambda (string) "\"bar\"")))
          (ess-r-evaluation-env "base")
          ess-eval-visibly)
      (insert "\"foo\"\n")
      (should (output= (ess-eval-region (point-min) (point-max) nil)
                "[1] \"bar\"")))))

(ert-deftest ess-r-namespaced-eval-no-sourced-message ()
  (with-r-running nil
    (let ((ess-r-evaluation-env "base")
          ess-eval-visibly)
      (insert "\"foo\"\n")
      (should (output= (ess-eval-region (point-min) (point-max) nil)
                       "[1] \"foo\"")))))

(ert-deftest ess-r-namespaced-eval-no-srcref-in-errors ()
  ;; Fails since https://github.com/emacs-ess/ESS/commit/3a7d913
  (when nil
    (with-r-running nil
      (let ((ess-r-evaluation-env "base")
            (error-msg "Error: unexpected symbol")
            ess-eval-visibly)
        (insert "(foo bar)\n")
        (let ((output (output (ess-eval-region (point-min) (point-max) nil))))
          (should (string= (substring output 0 (length error-msg))
                           error-msg)))))))


;;; Misc

(ert-deftest ess-r-makevars-mode ()
  (save-window-excursion
    (mapc (lambda (file)
            (switch-to-buffer (find-file-noselect file))
            (should (eq major-mode 'makefile-mode)))
          '("fixtures/Makevars" "fixtures/Makevars.win"))))

(ert-deftest ess-find-newest-date ()
  (should (equal (ess-find-newest-date '(("2003-10-04" . "R-1.7")
                                         ("2006-11-19" . "R-2.2")
                                         ("2007-07-01" . "R-dev")
                                         ("-1"         . "R-broken")
                                         ("2005-12-30" . "R-2.0")))
                 "R-dev")))

(ert-deftest ess-insert-S-assign ()
  ;; one call should insert assignment:
  (should
   (string= " <- "
            (ess-r-test-with-temp-text ""
              (progn
                (call-interactively 'ess-insert-S-assign)
                (buffer-substring (point-min) (point-max)))))))
