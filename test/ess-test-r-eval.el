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
         (eval . (progn
                   (ess-test-r-set-local-process)
                   (setq-local old-replace inferior-ess-replace-long+)
                   ;; Replacing strings of " +" causes random
                   ;; failures, probably because of the buffered
                   ;; output
                   (setq inferior-ess-replace-long+ nil)))
         (ess-eval-deactivate-mark . nil))
  :cleanup (setq inferior-ess-replace-long+ old-replace)

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

;;; ess-test-r-eval.el ends here
