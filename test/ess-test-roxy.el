;;; ess-test-roxy.el --- ESS tests for Roxygen  -*- lexical-binding: t; -*-
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
(require 'etest "etest/etest")
(require 'ess-r-mode)

(defun ess-test--faces-at-point ()
  (let ((face (get-char-property (point) 'face)))
    (if (listp face)
        face
      (list face))))

(etest-deftest ess-roxy-fill-paragraph-test ()
  :case "
##' Title
##'
##' @param¶ Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
¶##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
"

  "M-q" :result "
##' Title
##'
##' @param¶ Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor incididunt ut labore et dolore magna
##'     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
##'     ullamco laboris nisi ut aliquip ex ea commodo consequat.
¶##' @param Lorem ipsum dolor sit amet, consectetur adipiscing elit,
##'     sed do eiusmod tempor incididunt ut labore et dolore magna
##'     aliqua. Ut enim ad minim veniam, quis nostrud exercitation
##'     ullamco laboris nisi ut aliquip ex ea commodo consequat.
")

(etest-deftest ess-roxy-ret-test ()
  :case "
##' ¶
"

  "RET"
  :result "
##'
##' ¶
"
  "RET"
  :result "
##'
##'
##' ¶
"

  (setq-local ess-roxy-insert-prefix-on-newline nil)
  "RET"
  :result "
##'
##'
##'
¶
"

  :case "
##' ¶
"
  "M-j"
  :result "
##'
##' ¶
")

(etest-deftest ess-roxy-faces-param-test ()
  :case "
##' ¶@param foo
##' @¶param foo
"
  (should (memq 'font-lock-keyword-face (ess-test--faces-at-point)))

  :case "
##' @param ¶foo
"
  (should (not (memq 'font-lock-keyword-face (ess-test--faces-at-point))))
  (should (memq 'font-lock-variable-name-face (ess-test--faces-at-point)))

  ;; Comma-separated params
  :case "
##' @param ¶foo,bar baz
##' @param foo¶,bar baz
##' @param foo,¶bar baz
NULL
"
  (should (memq 'font-lock-variable-name-face (ess-test--faces-at-point)))

  :case "
##' @param foo,bar ¶baz
NULL
"
  (should (not (memq 'font-lock-variable-name-face (ess-test--faces-at-point)))))

;; Local Variables:
;; etest-local-config: etest-r-config
;; End:
