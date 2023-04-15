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
(require 'etest "test/etest/etest")
(require 'ess-r-mode)
(require 'ess-test-r-utils)
(require 'ess-rd)

;;; Code:

(defun ess-r-test-rd-init ()
  (Rd-mode)
  (ess-test-r-set-local-process))

(etest-deftest ess-rd-eval-linewise-visibly-test ()
  "C-c C-n always evaluates visibly.
https://github.com/emacs-ess/ESS/issues/725#issuecomment-431781558"
  :init ((eval . (ess-r-test-rd-init)))
  :case "
\\examples{
¶1
2
3
}
"

  (setq-local ess-eval-visibly-p 'nowait)
  "C-c C-n"
  :result "
\\examples{
1
¶2
3
}
"
  :inf-result "1
[1] 1
> "

  (setq-local ess-eval-visibly-p nil)
  "C-c C-n"
  :result "
\\examples{
1
2
¶3
}
"
  :inf-result "2
[1] 2
> "
  )
