;;; ess-test-r-tokens.el --- ESS tests for R tokens  -*- lexical-binding: t; -*-
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

(etest-deftest ess-r-syntax-backslash-test ()
  :case "sapply(x, ¶\\(y) y"
  (should (equal (syntax-after (point))
                 (string-to-syntax ".")))
  :case "c(\"¶\\\"\")"
  (should (equal (syntax-after (point))
                 (string-to-syntax "\\"))))

(etest-deftest ess-r-font-lock-boolean-operator-test ()
  :case "foo ¶| foo ¶|¶| foo ¶& foo ¶&¶& foo"
  (ess-with-enabled-font-lock-keyword 'ess-fl-keyword:operators
    (font-lock-ensure)
    (should (eq (face-at-point) 'ess-operator-face))))

(etest-deftest ess-r-font-lock-pipe-operator-test ()
  :case "a ¶|¶> b"
  (ess-with-enabled-font-lock-keyword 'ess-fl-keyword:operators
    (font-lock-ensure)
    (should (eq (face-at-point) 'ess-operator-face))))

(etest-deftest ess-r-tokens-pipe-operator-test ()
  :case "a ¶|> b"
  (should (token= "|>"))
  :result "a |>¶ b")

(etest-deftest ess-r-raw-strings-test ()
  :case "
r¶\"(foo\"bar))¶\"
r¶\"(foo}\"bar))¶\"
r¶\"(foo)'bar))¶\"
r¶\"---[foo\"bar]---¶\"
r¶\"---[foo\"bar]--\"baz]---¶\"
r¶'{foo'bar}¶'
r¶\"---{foobar}-\\--\"
"
  (should (equal (syntax-after (point))
                 (string-to-syntax "|")))

  :case "
r\"¶(foo\"¶bar))¶\"
r\"¶(foo}\"¶bar))¶\"
r\"¶(foo)'¶bar))¶\"
r\"¶---[foo\"¶bar]---¶\"
r\"¶---[foo\"¶bar]--\"¶baz]---¶\"
r'¶{foo'¶bar}¶'
r\"¶---{foobar}¶-\\--\"¶
"
  (should (ess-inside-string-p))

  :case "
¶r\"(foo\"bar))\"¶,
¶r\"(foo}\"bar))\"¶,
¶r\"(foo)'bar))\"¶,
¶r\"---[foo\"bar]---\"¶,
¶r\"---[foo\"bar]--\"baz]---\"¶,
¶r'{foo'bar}'¶,
¶r\"---{foobar}-\\--\"
"
  (should (not (ess-inside-string-p)))

  :case "r\"(foor\"()\"ba¶r))\""
  (should (not (ess-inside-string-p)))

  :case "
r\"(foor¶'()¶'bar))\"
# r¶\"{foo}¶\"
"
  (should (not (equal (syntax-after (point))
                      (string-to-syntax "|")))))


;; Local Variables:
;; etest-local-config: etest-r-config
;; End:
