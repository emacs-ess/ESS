
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
  :test ((forward-word))
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
