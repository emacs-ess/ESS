;;; ess-test-r-fontification.el --- ESS tests for R fontification  -*- lexical-binding: t; -*-
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

(defmacro with-ess-toggled-font-lock-keyword (enable keywords &rest body)
  (declare (indent 2)
           (debug (&rest form)))
  `(progn
     (let* ((enable ,enable)
            (keywords ,keywords)
            (keywords (if (listp keywords)
                          keywords
                        (list keywords)))
            toggled)
       (mapc (lambda (kw)
               (if (not (eq enable (cdr (assq kw ess-R-font-lock-keywords))))
                   (progn
                     (ess-font-lock-toggle-keyword kw)
                     (push kw toggled))))
             keywords)
       ,@body
       (mapc #'ess-font-lock-toggle-keyword
             toggled))))

(defmacro with-ess-disabled-font-lock-keyword (keywords &rest body)
  (declare (indent 1)
           (debug (&rest form)))
  `(with-ess-toggled-font-lock-keyword nil ,keywords ,@body))

(defmacro with-ess-enabled-font-lock-keyword (keywords &rest body)
  (declare (indent 1)
           (debug (&rest form)))
  `(with-ess-toggled-font-lock-keyword t ,keywords ,@body))

(etest-deftest ess-test-r-fontification-keywords-bare-fn-test ()
  "Function-like keywords without parentheses are not fontified."
  :case "
¶while ¶for ¶if ¶switch ¶function ¶return ¶on.exit ¶stop
¶tryCatch ¶withRestarts ¶invokeRestart ¶recover ¶browser
¶message ¶warning ¶signalCondition ¶withCallingHandlers
¶\\
"
  (should (not (face-at-point))))

(etest-deftest ess-test-r-fontification-keywords-fn-test ()
  "Function-like keywords with parentheses are fontified."
  :case "
¶while() ¶for() ¶if() ¶function()
¶switch() ¶return() ¶on.exit() ¶stop() ¶tryCatch()
¶withRestarts() ¶invokeRestart() ¶recover() ¶browser()
¶.Defunct() ¶\\(\\\\\\)()
"
  (should (eq (face-at-point) 'ess-keyword-face))

  (ess-forward-sexp)
  :result "
while¶() for¶() if¶() function¶()
switch¶() return¶() on.exit¶() stop¶() tryCatch¶()
withRestarts¶() invokeRestart¶() recover¶() browser¶()
.Defunct¶() \\(\\\\\\)¶()
"
  (should (not (face-at-point)))

  ;; Weak keywords
  :case "
¶message() ¶warning() ¶signalCondition() ¶withCallingHandlers()
¶.Deprecated()
"
  (should (eq (face-at-point) 'ess-modifiers-face))

  (ess-forward-sexp)
  :result "
message¶() warning¶() signalCondition¶() withCallingHandlers¶()
.Deprecated¶()
"
  (should (not (face-at-point))))

(etest-deftest ess-test-r-fontification-keywords-simple-test ()
  "Simple keywords are always fontified."
  :case "¶else ¶break ¶next ¶repeat ¶\?"
  (should (eq (face-at-point) 'ess-keyword-face)))

(etest-deftest ess-test-r-fontification-keywords-in-test ()
  "`in` is fontified only inside `for ()`."
  :case "for (foo ¶in bar) {}"
  (should (eq (face-at-point) 'ess-keyword-face))

  :case "for foo ¶in bar {}"
  (should (not (face-at-point))))

(etest-deftest ess-test-r-fontification-keywords-modifiers-test ()
  "Search list modifiers are fontified only if in function position"
  :case "¶library ¶attach ¶detach ¶source ¶require"
  (should (not (face-at-point)))

  :case "¶library() ¶attach() ¶detach() ¶source() ¶require()"
  (should (eq (face-at-point) 'ess-modifiers-face))

  (forward-word)
  :result "library¶() attach¶() detach¶() source¶() require¶()"
  (should (not (face-at-point))))

(etest-deftest ess-test-r-fontification-keywords-assignment-operators-test ()
  :case "foo¶ <- foo¶ <<- foo¶ -> foo¶ ->> foo"
  (should (not (face-at-point)))

  (forward-char)
  :result "foo ¶<- foo ¶<<- foo ¶-> foo ¶->> foo"
  (should (eq (face-at-point) 'ess-assignment-face))

  (skip-syntax-forward ".")
  :result "foo <-¶ foo <<-¶ foo ->¶ foo ->>¶ foo"
  (should (not (face-at-point))))

(etest-deftest ess-test-r-fontification-keywords-constants-test ()
  :case "
¶TRUE ¶FALSE ¶NA ¶NULL ¶Inf ¶NaN
¶NA_integer_ ¶NA_real_ ¶NA_complex_ ¶NA_character_
"
  (should (eq (face-at-point) 'ess-constant-face)))

(etest-deftest ess-test-r-fontification-keywords-user-modif ()
  "Can add keywords."
  :case "¶foobar"
  (let ((ess-R-keywords (append '("foobaz") ess-R-keywords)))
    (ess-r-mode)
    (font-lock-ensure))
  (should (not (face-at-point)))

  :case "¶foobaz()"
  (should (eq (face-at-point) 'ess-keyword-face)))

(etest-deftest ess-test-r-fontification-keywords-disabled-backquoted-defun ()
  "Can disable backquoted function definition fontification."
  :case "
¶`[.foo` <- function(...) NULL
¶\"[.foo\" <- function(...) NULL
"

  (should (eq (face-at-point) 'font-lock-function-name-face))

  (with-ess-disabled-font-lock-keyword 'ess-R-fl-keyword:fun-defs
    (font-lock-ensure)
    (if (looking-at "\"")
        (should (eq (face-at-point) 'font-lock-string-face))
      (should (not (face-at-point))))))

(etest-deftest ess-test-r-fontification-keywords-user-nil ()
  "Can set keywords to nil."
  :case "¶stop()"
  (let (ess-R-keywords)
    (ess-r-mode)
    (font-lock-ensure))
  (should (not (face-at-point)))

  :case "¶for (foo ¶in bar) NULL"
  (let ((ess-R-keywords '("in")))
    (ess-r-mode)
    (font-lock-ensure))
  (if (looking-at "for")
      (should (not (face-at-point)))
    (should (eq (face-at-point) 'ess-keyword-face)))

  :case "foo ¶%>% bar()"
  (should (eq (face-at-point) 'ess-%op%-face))

  (with-ess-disabled-font-lock-keyword '(ess-fl-keyword:operators
                                         ess-R-fl-keyword:%op%)
    (font-lock-ensure)
    (should (not (face-at-point)))
    (forward-char)
    (should (not (face-at-point))))

  (with-ess-disabled-font-lock-keyword 'ess-R-fl-keyword:%op%
    (font-lock-ensure)
    (should (not (face-at-point)))))

(etest-deftest ess-test-r-fontification-keywords-disabled-backquoted-call ()
  "Can disable backquoted function call fontification."
  :case "¶`fun`()"
  (should (not (face-at-point)))

  (with-ess-enabled-font-lock-keyword 'ess-fl-keyword:fun-calls
    (font-lock-ensure)
    (should (eq (face-at-point) 'ess-function-call-face))))

(etest-deftest ess-test-r-fontification-keywords-modulo-operator ()
  "Modulo operator is fontified independently from special ops."
  :case "foo ¶%¶% bar"

  (with-ess-disabled-font-lock-keyword 'ess-R-fl-keyword:%op%
    (with-ess-enabled-font-lock-keyword 'ess-fl-keyword:operators
      (font-lock-ensure)
      (should (eq (face-at-point) 'ess-operator-face))))

  (with-ess-enabled-font-lock-keyword '(ess-R-fl-keyword:%op%
                                        ess-fl-keyword:operators)
    (font-lock-ensure)
    (should (eq (face-at-point) 'ess-operator-face)))

  (with-ess-disabled-font-lock-keyword '(ess-R-fl-keyword:%op%
                                         ess-fl-keyword:operators)
    (font-lock-ensure)
    (should (not (face-at-point)))))

(etest-deftest ess-test-r-fontification-keywords-backticked-default-face ()
  "NOTE: These used to return `default', now return `nil'."
  :case "
`¶repeat`
`¶%>%`
`¶-`
"

  ;; Disabled
  (when nil
    (with-ess-enabled-font-lock-keyword '(ess-R-fl-keyword:%op%
                                          ess-fl-keyword:operators)
      (font-lock-ensure)
      (should (not (face-at-point))))))

(etest-deftest test-ess-r-fontification-inferior-r-backticked ()
  "Backticked symbols are not fontified as strings."
  :case "¶`f¶oo¶`"
  (setq-local font-lock-syntactic-face-function
              #'inferior-ess-r-font-lock-syntactic-face-function)
  (font-lock-ensure)
  (should (not (face-at-point))))


;; Local Variables:
;; etest-local-config: etest-r-config
;; End:
