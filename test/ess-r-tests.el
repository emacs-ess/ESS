
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

(when (> emacs-major-version 24)
  ;; Skip before Emacs 25 when rectangle-mark-mode was improved. We
  ;; have to wrap the whole thing in that when statement because ert's
  ;; skip-unless was only introduced in Emacs 24.4 and we're testing
  ;; all the way back to Emacs 24.3 still, ugh.
  (ert-deftest ess-r-eval-rectangle-mark-mode ()
    (with-r-running nil
      (insert "x <- 1\nx\nx + 1\nx  +  2\n")
      (let (ess-eval-visibly)
        (should (output= (progn
                           (goto-char (point-min))
                           (transient-mark-mode)
                           (rectangle-mark-mode)
                           (forward-line 3)
                           (end-of-line)
                           (ess-eval-region-or-line-and-step))
                  "> [1] 1\n> [1] 2\n> [1] 3"))))))

(ert-deftest ess-set-working-directory ()
  (with-r-running nil
    (ess-set-working-directory "/")
    (ess-eval-linewise "getwd()" 'invisible)
    (should (output= (ess-eval-buffer nil)
              "setwd('/')\n> [1] \"/\""))
    (should (string= default-directory "/"))))

(ert-deftest ess-inferior-force ()
  (with-r-running nil
    (should (equal (ess-get-words-from-vector "letters[1:2]\n")
                   (list "a" "b")))))


;;; ess-r-package-mode

(ert-deftest ess-r-package-auto-activation ()
  (let ((inhibit-message ess-inhibit-message-in-tests))
    (with-temp-buffer
      (text-mode)
      (hack-local-variables)
      (should (not ess-r-package-mode)))
    (with-r-file "dummy-pkg/R/test.R"
      (hack-local-variables)
      (should ess-r-package-mode))))

(ert-deftest ess-r-package-auto-activation-in-shell ()
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

(ert-deftest ess-r-package-auto-no-activation-in-shell ()
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

(ert-deftest ess-r-package-vars ()
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
                (setq last-input-event ?_)
                (call-interactively 'ess-insert-S-assign)
                (buffer-substring (point-min) (point-max)))))))

(ert-deftest ess-Rout-file ()
  (let ((buf (find-file-noselect "fixtures/file.Rout")))
    (with-current-buffer buf
      (should (eq major-mode 'ess-transcript-mode))
      (font-lock-default-fontify-buffer)
      (should (eq (face-at-point) 'font-lock-function-name-face)))))

(ert-deftest inferior-ess-r-fontification ()
  (with-r-running nil
    (with-ess-process-buffer nil
      ;; Function-like keywords
      (should (eq major-mode 'inferior-ess-mode))
      (insert-fontified "for")
      (should (not (face-at -1)))
      (insert-fontified "(")
      (should (eq (face-at -2) 'ess-keyword-face))
      ;; `in` keyword
      (insert-fontified "foo in bar)")
      (search-backward "in")
      (should (eq (face-at-point) 'ess-keyword-face))
      (erase-buffer)
      (insert-fontified "for foo in bar")
      (search-backward "in")
      (should (not (face-at-point))))))

;; roxy

;; Emacs 24 doesn't have ERT's skip-unless
(when (> emacs-major-version 24)
  (ert-deftest ess-roxy-preview-Rd ()
    ;; (skip-unless (member "roxygen2" (ess-r-installed-packages)))
    (with-r-running nil
      (should (member "roxygen2" (ess-r-installed-packages)))
      (should
       (string= "% Generated by roxygen2: do not edit by hand
\\name{add}
\\alias{add}
\\title{Add together two numbers.
add(10, 1)}
\\usage{
add(x, y)
}
\\description{
Add together two numbers. add(10, 1)
} 
"
                (with-temp-buffer
                  (R-mode)
                  (insert
                   "##' Add together two numbers.
##' add(10, 1)
add <- function(x, y) {
  x + y
}")
                  (goto-char (point-min))
                  (ess-roxy-preview-Rd)
                  ;; Delete the reference to the file which isn't
                  ;; reproducible across different test environments
                  (goto-char (point-min))
                  (forward-line 1)
                  (kill-whole-line)
                  (buffer-substring-no-properties (point-min) (point-max))))))))
