;;; ess-test-rd.el --- ESS tests for Rd-mode  -*- lexical-binding: t; -*-
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
;;

(require 'ert)
(require 'etest)
(require 'ess-r-mode)
(require 'ess-test-r-utils)

;; Make sure inferiors have been loaded so that messages etc. do not
;; interfere with the tests
(ess-r-test-proc-buf 'output)
(ess-r-test-proc-buf 'tracebug)

(etest-deftest ess-r-eval-visibility-eval-standard-filter-test ()
  "`ess-eval-region' respects `ess-eval-visibly'.
Standard filter."
  :init ((mode . r)
         (ess-eval-deactivate-mark . nil)
         (eval . (ess-test-r-set-local-process 'output)))

  :case "
¶1
2
{
  3
  4
}
5; 6×
"

  :eval ((setq-local ess-eval-visibly t)
         "C-c C-r")
  :inf-result "1
[1] 1
> 2
[1] 2
> {
+   3
+   4
+ }
[1] 4
> 5; 6
[1] 5
[1] 6
> "

  :eval ((setq-local ess-eval-visibly 'nowait)
         "C-c C-r")
  :inf-result "1
+ 2
+ {
+   3
+   4
+ }
+ 5; 6
[1] 1
> [1] 2
> + + + [1] 4
> [1] 5
[1] 6
> "

  :eval ((setq-local ess-eval-visibly nil)
         "C-c C-r")
  :inf-result "[1] 1
> [1] 2
> + + + [1] 4
> [1] 5
[1] 6
> ")

(etest-deftest ess-r-eval-visibility-eval-test ()
  "`ess-eval-region' respects `ess-eval-visibly'.
Default filter"
  :init ((mode . r)
         (eval . (ess-test-r-set-local-process 'tracebug))
         (ess-eval-deactivate-mark . nil))

  :case "
¶1
2
{
  3
  4
}
5; 6×
"

  ;; 'nowait curiously causes sporadic failures with the { block
  ;; output. The number of continuation `+` varies. The chance of
  ;; failure increases when input has already been sent to the
  ;; process. Probably because of the special `nowait` handling in the
  ;; tracebug filter.

  ;;   :eval ((setq-local ess-eval-visibly 'nowait)
  ;;          "C-c C-r")
  ;;   :inf-result "1
  ;; + 2
  ;; + {
  ;; +   3
  ;; +   4
  ;; + }
  ;; + 5; 6
  ;; [1] 1
  ;; > [1] 2
  ;; > + + + [1] 4
  ;; > [1] 5
  ;; [1] 6
  ;; > "

  :eval ((setq-local ess-eval-visibly t)
         "C-c C-r")
  :inf-result "1
[1] 1
> 2
[1] 2
> {
+   3
+   4
+ }
[1] 4
> 5; 6
[1] 5
[1] 6
> "

  :eval ((setq-local ess-eval-visibly nil)
         "C-c C-r")
  :inf-result "[1] 1
> [1] 2
> + + + [1] 4
> [1] 5
[1] 6
> ")

(etest-deftest ess-r-eval-ns-env-roxy-standard-test ()
  "Roxygen blocks are not evaluated in current eval-env (#1026).
Standard filter."
  :init ((mode . r)
         (ess-r-evaluation-env . "base")
         (eval . (ess-test-r-set-local-process 'output)))

  :case "#' ¶identical(environment(), globalenv())"
  :eval "C-c C-j"
  :inf-result "identical(environment(), globalenv())
[1] TRUE
> "
  ;; Shouldn't mention "[base]"
  :messages "Starting evaluation...
Loading line: #' identical(environment(), globalenv())"

  :case "
#' ¶identical(environment(), globalenv())
NULL×"

  :eval "C-c C-r"
  :inf-result "+ > identical(environment(), globalenv())
[1] FALSE
> NULL
NULL
> "
  ;; Mentions "[base]"
  :messages "Starting evaluation...
[base] Eval region")

(etest-deftest ess-r-eval-ns-env-roxy-tracebug-test ()
  "Roxygen blocks are not evaluated in current eval-env (#1026).
Tracebug filter."
  :init ((mode . r)
         (eval . (ess-test-r-set-local-process 'tracebug))
         (ess-r-evaluation-env . "base")
         (ess-eval-visibly . nil))
  :case "#' ¶identical(environment(), globalenv())"
  :eval "C-c C-j"
  :inf-result "[1] TRUE
> "
  ;; Shouldn't mention "[base]"
  :messages "Starting evaluation...
Loading line: #' identical(environment(), globalenv())"

  :case "
#' ¶identical(environment(), globalenv())
NULL×"

  :eval "C-c C-r"
  :inf-result "+ [1] FALSE
NULL
> "
  ;; Mentions "[base]"
  :messages "Starting evaluation...
[base] Eval region")

(ert-deftest ess-rd-eval-ns-env-test ()
  "Namespaced eval is disabled in doc files (#1026)."
  (with-temp-buffer
    (find-file-noselect "dummy-pkg/man/doc.Rd")
    (Rd-mode)
    (should (not ess-r-evaluation-env))))

;;; ess-test-r-eval.el ends here
