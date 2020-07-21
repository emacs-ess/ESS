
(require 'ert)
(require 'etest)

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

(etest-deftest etest-cleanup-test ()
  "`:cleanup' keywords are evaluated in LIFO order."
  :init ((foo . "foo")
         (bar . "bar"))
  :cleanup (progn
             (should (equal foo "FOO"))
             (should (equal bar "BAR")))
  :cleanup (progn
             (should (equal foo "foo"))
             (should (equal bar "BAR"))
             (setq foo "FOO"))
  :cleanup (progn
             (should (equal foo "foo"))
             (should (equal bar "bar"))
             (setq bar "BAR")))

(etest-deftest etest-inferior-buffer-test ()
  "Inferior buffer is flushed and tested."
  :inf-buffer (get-buffer-create "aux-buffer")
  :test (with-current-buffer etest-local-inferior-buffer
          (insert "foo"))
  :inf-result "foo"
  :inf-result ""
  :test (with-current-buffer etest-local-inferior-buffer
          (insert "foobar"))
  :inf-result "foobar"
  :inf-result "")

(etest-deftest etest-messages-test ()
  "Can retrieve messages with `:messages'."
  :messages ""
  :test (message "foo")
  :test (message "bar")
  :messages "foo
bar"
  :messages "")

(ert-deftest etest-unalias-prefix-key ()
  "Can supply keymap prefix commands like `C-c C-c`."
  (with-temp-buffer
    (let ((map (make-sparse-keymap))
          called)
      (define-key map (kbd "C-c C-c") (lambda () (interactive) (setq called t)))
      (use-local-map map)
      (etest--unalias (kbd "C-c C-c"))
      (should called))))
