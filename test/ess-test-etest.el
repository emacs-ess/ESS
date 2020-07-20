
(require 'ert)
(require 'ess-test-literate)

(etest-deftest etest-local-vars-test ()
  "`:init' keyword specifies local variables."
  :init ((mode . text)
         (foo . t))
  :init ((bar . t))
  :test ((should (eq major-mode 'text-mode))
         (should foo)
         (should bar)))

(etest-deftest etest-test-result-test ()
  "`:test' causes side effects in test buffer and `:result' checks output."
  :case "¶foo bar"
  :test (forward-word)
  :result "foo¶ bar"
  :test ((forward-char)
         (forward-char))
  :test ((forward-char)
         "RET")
  :result "foo ba\n¶r")

(etest-deftest etest-climb-deftest-test ()
  "Find enclosing `etest-deftest'."

  ;; Within parentheses
  :case "
(etest-deftest name ()
  :test (foo (bar¶))
"
  :test ((etest--climb-deftest))
  :result "
¶(etest-deftest name ()
  :test (foo (bar))
"

  ;; Within a string
  :case "
(etest-deftest name ()
  :test (foo \"bar¶\")
"
  :test ((etest--climb-deftest))
  :result "
¶(etest-deftest name ()
  :test (foo \"bar\")
"

  ;; Behind deftest
  :case "
¶(etest-deftest name ()
  :foo)
"
  :test ((etest--climb-deftest))
  :result "
¶(etest-deftest name ()
  :foo)
"

  ;; In front of deftest
  :case "
(etest-deftest name ()
  :foo)¶
"
  :test ((etest--climb-deftest))
  :result "
¶(etest-deftest name ()
  :foo)
")

(etest-deftest etest-update-test ()
  "`etest-update' updates test block at point."
  :case "
  (etest-deftest test-etest-update ()
    :case \"¶foo bar\"
    :test ((forward-word))
    :result \"\"
    :test ((forward-char)
           (forward-char))
    :test ((forward-char)
           \"RET\")
    :result \"\")
"
  :test ((etest-update))
  :result "
  (etest-deftest test-etest-update ()
    :case \"¶foo bar\"
    :test ((forward-word))
    :result \"foo¶ bar\"
    :test ((forward-char)
           (forward-char))
    :test ((forward-char)
           \"RET\")
    :result \"foo ba
¶r\")
")

(etest-deftest etest-keep-state-test ()
  "`last-command' is preserved"
  :case "¶foo ¶bar"
  :test ("M-f")
  :result "foo¶ bar¶"
  :test ((should (eq last-command 'forward-word))))

(ert-deftest etest-wrap-test-keyword-test ()
  "`:test' keywords are appropriately wrapped in lists."
  (should (equal (etest--wrap-test "foo")
                 '("foo")))
  (should (equal (etest--wrap-test 'foo)
                 '(foo)))
  (should (equal (etest--wrap-test '(foo))
                 '((foo))))
  (should (equal (etest--wrap-test '((foo)))
                 '((foo)))))
