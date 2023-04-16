;;; ess-test-r-token.el --- ESS tests for R tokens  -*- lexical-binding: t; -*-
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

(etest-deftest ess-test-r-token-jump-strings-symbols-test ()
  :case "¶`a\"a\"a` \"a`a`a\""

  (ess-test-should-token= "identifier" "`a\"a\"a`")
  :result "`a\"a\"a`¶ \"a`a`a\""

  (ess-test-should-token= "string" "\"a`a`a\"")
  :result "`a\"a\"a` \"a`a`a\"¶")

(etest-deftest ess-test-r-token-jump-identifiers-test ()
  :case "¶.a_a a10"

  (ess-test-should-token= "identifier" ".a_a")
  :result ".a_a¶ a10"

  (ess-test-should-token= "identifier" "a10")
  :result ".a_a a10¶")

(etest-deftest ess-test-r-token-jump-numbers-test ()
  :case "¶100 1E10 1e10 1.10"

  (ess-test-should-token= "number" "100")
  :result "100¶ 1E10 1e10 1.10"

  (ess-test-should-token= "number" "1E10")
  :result "100 1E10¶ 1e10 1.10"

  (ess-test-should-token= "number" "1e10")
  :result "100 1E10 1e10¶ 1.10"

  (ess-test-should-token= "number" "1.10")
  :result "100 1E10 1e10 1.10¶")

(etest-deftest ess-test-r-token-jump-delimiters-test ()
  :case "¶() a[[[]]] {}"

  (ess-test-should-token= "(")
  :result "(¶) a[[[]]] {}"

  (ess-test-should-token= ")")
  :result "()¶ a[[[]]] {}"

  (ess-jump-token)
  (ess-test-should-token= "[[")
  :result "() a[[¶[]]] {}"

  (should (ess-jump-token "["))
  (should (ess-token-before= "[["))
  :result "() a[[[¶]]] {}"

  (ess-test-should-token= "]]")
  :result "() a[[[]]¶] {}"

  (should (ess-jump-token "]"))
  (should (ess-token-before= "]]"))
  :result "() a[[[]]]¶ {}"

  (ess-test-should-token= "{")
  :result "() a[[[]]] {¶}"

  (ess-test-should-token= "}")
  :result "() a[[[]]] {}¶")

(etest-deftest ess-test-r-token-buffer-boundaries-test ()
  :case "¶"

  (should (ess-token-before= "buffer-start"))
  (should (ess-token-after= "buffer-end"))
  :result "¶")

(etest-deftest ess-test-r-token-jump-punctuation-test ()
  :case "¶.; .,"

  (ess-jump-token)
  (ess-test-should-token= ";")
  :result ".;¶ .,"

  (ess-jump-token)
  (ess-test-should-token= ",")
  :result ".; .,¶")

(etest-deftest ess-test-r-token-jump-keywords-test ()
  :case "¶if if_else else function while for"

  (ess-test-should-token= "if")
  (ess-test-should-token= "identifier" "if_else")
  (ess-test-should-token= "else")
  (ess-test-should-token= "function")
  (ess-test-should-token= "while")
  (ess-test-should-token= "for")

  :result "if if_else else function while for¶")

(etest-deftest ess-test-r-token-jump-logical-operators-test ()
  :case "¶a & a && a &&& a | a || a ||| a"

  (ess-jump-token)
  (ess-test-should-token= "&")

  :result "a &¶ a && a &&& a | a || a ||| a"

  (ess-jump-token)
  (ess-test-should-token= "&&")

  :result "a & a &&¶ a &&& a | a || a ||| a"

  (ess-jump-token)
  (ess-test-should-token= "&&")

  :result "a & a && a &&¶& a | a || a ||| a"

  (should (ess-jump-token "&"))
  (should (ess-token-before= "&&"))

  :result "a & a && a &&&¶ a | a || a ||| a"

  (ess-jump-token)
  (ess-test-should-token= "|")

  :result "a & a && a &&& a |¶ a || a ||| a"

  (ess-jump-token)
  (ess-test-should-token= "||")

  :result "a & a && a &&& a | a ||¶ a ||| a"

  (ess-jump-token)
  (ess-test-should-token= "||")

  :result "a & a && a &&& a | a || a ||¶| a"

  (should (ess-jump-token "|"))
  (should (ess-token-before= "||"))

  :result "a & a && a &&& a | a || a |||¶ a")

(etest-deftest ess-test-r-token-jump-comparison-operators-test ()
  :case "¶a = a := a == a === a :== a != a :!= a"

  (ess-jump-token)
  (ess-test-should-token= "=")
  :result "a =¶ a := a == a === a :== a != a :!= a"

  (ess-jump-token)
  (ess-test-should-token= ":=")
  :result "a = a :=¶ a == a === a :== a != a :!= a"

  (ess-jump-token)
  (ess-test-should-token= "==")
  :result "a = a := a ==¶ a === a :== a != a :!= a"

  (ess-jump-token)
  (ess-test-should-token= "==")
  :result "a = a := a == a ==¶= a :== a != a :!= a"

  (should (ess-jump-token "="))
  (should (ess-token-before= "=="))
  :result "a = a := a == a ===¶ a :== a != a :!= a"

  (ess-jump-token)
  (ess-test-should-token= ":=")
  :result "a = a := a == a === a :=¶= a != a :!= a"

  (should (ess-jump-token "="))
  (should (ess-token-before= "=="))
  :result "a = a := a == a === a :==¶ a != a :!= a"

  (ess-jump-token)
  (ess-test-should-token= "!=")
  :result "a = a := a == a === a :== a !=¶ a :!= a"

  (ess-jump-token)
  (ess-test-should-token= ":")
  :result "a = a := a == a === a :== a != a :¶!= a"

  (ess-test-should-token= "!=")
  :result "a = a := a == a === a :== a != a :!=¶ a")

(etest-deftest ess-test-r-token-jump-%%-operators-test ()
  :case "¶a %>% a %a`a`a\"a\"a$a@a% a %% a %%% a % a"

  (ess-jump-token)
  (ess-test-should-token= "%infix%" "%>%")
  :result "a %>%¶ a %a`a`a\"a\"a$a@a% a %% a %%% a % a"

  (ess-jump-token)
  (ess-test-should-token= "%infix%" "%a`a`a\"a\"a$a@a%")
  :result "a %>% a %a`a`a\"a\"a$a@a%¶ a %% a %%% a % a"

  (ess-jump-token)
  (ess-test-should-token= "%%")
  :result "a %>% a %a`a`a\"a\"a$a@a% a %%¶ a %%% a % a"

  (ess-jump-token)
  (ess-test-should-token= "%%")
  :result "a %>% a %a`a`a\"a\"a$a@a% a %% a %%¶% a % a"

  (ess-test-should-token= "%infix%" "% a %")
  :result "a %>% a %a`a`a\"a\"a$a@a% a %% a %%% a %¶ a")

(etest-deftest ess-test-r-token-jump-arithmetic-operators-test ()
  :case "¶a + a - a - -a * a ** a ^ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "+")
  :result "a +¶ a - a - -a * a ** a ^ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "-")
  :result "a + a -¶ a - -a * a ** a ^ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "-")
  :result "a + a - a -¶ -a * a ** a ^ a ^ ++a"

  (ess-test-should-token= "-")
  :result "a + a - a - -¶a * a ** a ^ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "*")
  :result "a + a - a - -a *¶ a ** a ^ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "**")
  :result "a + a - a - -a * a **¶ a ^ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "^")
  :result "a + a - a - -a * a ** a ^¶ a ^ ++a"

  (ess-jump-token)
  (ess-test-should-token= "^")
  :result "a + a - a - -a * a ** a ^ a ^¶ ++a"

  (ess-test-should-token= "+")
  :result "a + a - a - -a * a ** a ^ a ^ +¶+a"

  (ess-test-should-token= "+")
  :result "a + a - a - -a * a ** a ^ a ^ ++¶a")

(etest-deftest ess-test-r-token-jump-:-operators-test ()
  :case "¶a:  a::  a:::  a::::  a:::="

  (ess-jump-token)
  (ess-test-should-token= ":")
  :result "a:¶  a::  a:::  a::::  a:::="

  (ess-jump-token)
  (ess-test-should-token= "::")
  :result "a:  a::¶  a:::  a::::  a:::="

  (ess-jump-token)
  (ess-test-should-token= ":::")
  :result "a:  a::  a:::¶  a::::  a:::="

  (ess-jump-token)
  (ess-test-should-token= ":::")
  :result "a:  a::  a:::  a:::¶:  a:::="

  (should (ess-jump-token ":"))
  (should (ess-token-before= ":::"))
  :result "a:  a::  a:::  a::::¶  a:::="

  (ess-jump-token)
  (ess-test-should-token= ":::")
  :result "a:  a::  a:::  a::::  a:::¶=")

(etest-deftest ess-test-r-token-jump-assignment-operators-test ()
  :case "¶a <-  a <<-  a -> >  a ->> a >> a"

  (ess-jump-token)
  (ess-test-should-token= "<-")
  :result "a <-¶  a <<-  a -> >  a ->> a >> a"

  (ess-jump-token)
  (ess-test-should-token= "<<-")
  :result "a <-  a <<-¶  a -> >  a ->> a >> a"

  (ess-jump-token)
  (ess-test-should-token= "->")
  :result "a <-  a <<-  a ->¶ >  a ->> a >> a"

  (ess-test-should-token= ">")
  :result "a <-  a <<-  a -> >¶  a ->> a >> a"

  (ess-jump-token)
  (ess-test-should-token= "->>")
  :result "a <-  a <<-  a -> >  a ->>¶ a >> a"

  (ess-jump-token)
  (ess-test-should-token= ">")
  (ess-test-should-token= ">")
  :result "a <-  a <<-  a -> >  a ->> a >>¶ a")

(etest-deftest ess-test-r-token-jump-inequality-operators-test ()
  :case "¶a < >  a >=  a > =  a <="

  (ess-jump-token)
  (ess-test-should-token= "<")
  :result "a <¶ >  a >=  a > =  a <="

  (ess-test-should-token= ">")
  :result "a < >¶  a >=  a > =  a <="

  (ess-jump-token)
  (ess-test-should-token= ">=")
  :result "a < >  a >=¶  a > =  a <="

  (ess-jump-token)
  (ess-test-should-token= ">")
  :result "a < >  a >=  a >¶ =  a <="

  (ess-test-should-token= "=")
  :result "a < >  a >=  a > =¶  a <="

  (ess-jump-token)
  (ess-test-should-token= "<=")
  :result "a < >  a >=  a > =  a <=¶")

(etest-deftest ess-test-r-token-jump-special-operators-test ()
  :case "¶~a~~a"

  (ess-test-should-token= "~")
  :result "~¶a~~a"

  (ess-jump-token)
  (ess-test-should-token= "~")
  (ess-test-should-token= "~")
  :result "~a~~¶a")

(etest-deftest ess-test-r-token-refine-param-assignment-test ()
  :case "call(param ¶= NULL)"

  (ess-test-should-token= "=")
  (should (ess-refined-token= (ess-token-before) "param-assign"))
  :result "call(param =¶ NULL)")

(etest-deftest ess-test-r-token-refine-quoted-param-names-test ()
  :case "call(¶\"param\" = NULL)"

  (ess-test-should-token= "string" "\"param\"")
  (should (ess-refined-token= (ess-token-before) "identifier"))
  :result "call(\"param\"¶ = NULL)")

(etest-deftest ess-test-r-token-refine-quoted-call-names-test ()
  :case "¶\"call\"()"

  (ess-test-should-token= "string" "\"call\"")
  (should (ess-refined-token= (ess-token-before) "identifier"))
  :result "\"call\"¶()")

(etest-deftest ess-test-r-token-skip-blanks-test ()
  :case "
text¶
text"

  (should (ess-skip-blanks-forward t))
  :result "
text
¶text"

  (should (not (ess-skip-blanks-backward)))
  :result "
text
¶text"

  (should (ess-skip-blanks-backward t))
  :result "
text¶
text")

(etest-deftest ess-test-r-token-skip-comments-test ()
  :case "text¶ # comment"

  (should (ess-skip-blanks-forward t))
  :result "text ¶# comment"

  (should (not (ess-skip-blanks-forward t)))
  :result "text ¶# comment")

(etest-deftest ess-test-r-token-skip-form-feed-test ()
  :case "
text


¶text"

  (should (ess-skip-blanks-backward t))
  :result "
text¶


text")

;; Local Variables:
;; etest-local-config: etest-r-config
;; End:
