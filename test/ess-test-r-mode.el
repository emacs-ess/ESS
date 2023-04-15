;;; ess-test-r-mode.el --- ESS tests for R mode  -*- lexical-binding: t; -*-
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

(etest-deftest test-ess-inferior-r-backticked ()
  "Backticked symbols are not fontified as strings."
  :case "¶`f¶oo¶`"
  (setq-local font-lock-syntactic-face-function
              #'inferior-ess-r-font-lock-syntactic-face-function)
  (font-lock-ensure)
  (should (not (face-at-point))))


;; Local Variables:
;; etest-local-config: etest-r-config
;; End:
