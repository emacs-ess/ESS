;;; ess-test-r-syntax.el --- ESS tests for R syntax  -*- lexical-binding: t; -*-
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
  (should (ess-test-token= "|>"))
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

(etest-deftest ess-r-syntax-climb-test ()
  :case "
stuff1 =, ¶stuff2
stuff1 =; ¶stuff2
stuff1 := ¶stuff2
stuff1 %a?a:a% ¶stuff2
stuff1 %% ¶stuff2
stuff1 => ¶stuff2
"
  (ess-climb-operator)
  :result "
stuff1 =, ¶stuff2
stuff1 =; ¶stuff2
stuff1¶ := stuff2
stuff1¶ %a?a:a% stuff2
stuff1¶ %% stuff2
stuff1¶ => stuff2
"

  :case "
function_call()
¶
"
  (ess-climb-block-prefix)
  :result "
function_call()
¶
"
  (ess-climb-block-prefix "function")
  :result "
function_call()
¶
")

(etest-deftest ess-r-syntax-climb-continuations-test ()
  :case "(!stuff1 ||¶ stuff2)"
  (ess-climb-continuations)
  :result "(¶!stuff1 || stuff2)"

  :case "
object <-
    fun_call() %>%
    ¶fun_call()

object <-
    fun_call() %>% fun_call() %>%
    ¶fun_call()

object <-
    namespace::fun_call() %>%
    ¶fun_call()

object <-
    namespace:::fun_call() %>%
    ¶fun_call()

object <-
    object@fun_call() %>%
    ¶fun_call()

object <-
    object$fun_call() %>%
    ¶fun_call()
"

  (ess-climb-continuations)
  :result "
¶object <-
    fun_call() %>%
    fun_call()

¶object <-
    fun_call() %>% fun_call() %>%
    fun_call()

¶object <-
    namespace::fun_call() %>%
    fun_call()

¶object <-
    namespace:::fun_call() %>%
    fun_call()

¶object <-
    object@fun_call() %>%
    fun_call()

¶object <-
    object$fun_call() %>%
    fun_call()
")

(etest-deftest ess-r-syntax-climb-sticky-ops-test ()
  :case "
object@field¶
object$field¶
namespace::object¶
namespace:::object¶
"

  (ess-climb-expression)
  :result "
¶object@field
¶object$field
¶namespace::object
¶namespace:::object
"

  :case reset
  (ess-climb-object)
  :result "
¶object@field
¶object$field
¶namespace::object
¶namespace:::object
")

(etest-deftest ess-r-syntax-jump-test ()
  :case "
    ¶if (test1)
        stuff1
    if (test2)
        stuff2"

  (ess-jump-expression)
  :result "
    if (test1)
        stuff1¶
    if (test2)
        stuff2")


;; Local Variables:
;; etest-local-config: etest-r-config
;; End:
