;; -*- lexical-binding: t -*-

(require 'ert)
(require 'etest "test/etest/etest")

(etest-deftest etest-local-vars-test ()
  "`:init' keyword specifies local variables."
  :init ((mode . text)
         (foo . t))
  :init ((bar . t))
  (should (eq major-mode 'text-mode))
  (should foo)
  (should bar))

(etest-deftest etest-test-result-test ()
  "`:eval' causes side effects in test buffer and `:result' checks output."
  :case "¶foo bar"
  :eval (forward-word)
  :result "foo¶ bar"
  :eval ((forward-char)
         (forward-char))
  :eval ((forward-char)
         "RET")
  :result "foo ba\n¶r")

(etest-deftest etest-climb-deftest-test ()
  "Find enclosing `etest-deftest'."

  ;; Within parentheses
  :case "
(etest-deftest name ()
  (foo (bar¶))
"
  (etest--climb-deftest)
  :result "
¶(etest-deftest name ()
  (foo (bar))
"

  ;; Within a string
  :case "
(etest-deftest name ()
  (foo \"bar¶\")
"
  (etest--climb-deftest)
  :result "
¶(etest-deftest name ()
  (foo \"bar\")
"

  ;; Behind deftest
  :case "
¶(etest-deftest name ()
  :foo)
"
  (etest--climb-deftest)
  :result "
¶(etest-deftest name ()
  :foo)
"

  ;; In front of deftest
  :case "
(etest-deftest name ()
  :foo)¶
"
  (etest--climb-deftest)
  :result "
¶(etest-deftest name ()
  :foo)
")

(etest-deftest etest-update-test ()
  "`etest-update' updates test block at point."
  :case "
  (etest-deftest test-etest-update ()
    :case \"¶foo bar\"
    (forward-word)
    :result \"\"
    (forward-char)
    (forward-char)
    (forward-char)
    \"RET\"
    :result \"\")
"
  (etest-update)
  :result "
  (etest-deftest test-etest-update ()
    :case \"¶foo bar\"
    (forward-word)
    :result \"foo¶ bar\"
    (forward-char)
    (forward-char)
    (forward-char)
    \"RET\"
    :result \"foo ba
¶r\")
")

(etest-deftest etest-update-result-not-on-bol-test ()
  "`etest-update' works when `:result` is not at bol."
  :case "
  (etest-deftest test-etest-update ()
    :case \"¶foo bar\"
    (forward-word) :result \"\"
    (forward-word) :result \"\")
"
  (etest-update)
  :result "
  (etest-deftest test-etest-update ()
    :case \"¶foo bar\"
    (forward-word) :result \"foo¶ bar\"
    (forward-word) :result \"foo bar¶\")
")

(etest-deftest etest-keep-state-test ()
  "`last-command' is preserved.
Using multiple cursors in the test to make sure Emacs state is
reset after a cursor has finished evaluating."
  :case "¶foo ¶bar"
  "M-f"
  (should (eq last-command 'forward-word))
  ;; Ideally we'd test `this-command` at the time "M-f" is
  ;; called but for simplicity we do it here
  (should (eq this-command 'forward-word))
  "M-b"
  (should (eq last-command 'backward-word))
  (should (eq this-command 'backward-word))
  :result "¶foo ¶bar")

(ert-deftest etest-wrap-test-keyword-test ()
  "`:eval' keywords are appropriately wrapped in lists."
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
  (with-current-buffer etest-local-inferior-buffer
    (insert "foo"))
  :inf-result "foo"
  :inf-result ""
  (with-current-buffer etest-local-inferior-buffer
    (insert "foobar"))
  :inf-result "foobar"
  :inf-result "")

(etest-deftest etest-messages-test ()
  "Can retrieve messages with `:messages'."
  :messages ""
  (message "foo")
  (message "bar")
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

(etest-deftest etest-multiple-results-test ()
  "Parser is not fazed by multiple consecutive results."
  :case "
(etest-deftest etest-multiple-results-test ()
  :case \"¶1\"
  \"<right>\"
  :result \"\"
  :result \"\")
"
  (etest-update)
  :result "
(etest-deftest etest-multiple-results-test ()
  :case \"¶1\"
  \"<right>\"
  :result \"1¶\"
  :result \"1¶\")
")

(etest-deftest etest-skip-comments-test ()
  "Parser skips any comments when looking for `:result' keywords."
  :case "
(etest-deftest test ()
  :case \"¶1\"
  \"<right>\"
  ;; Comment
  ;; Comment

  ;; Comment
  :result \"\")
"
  (etest-update)
  :result "
(etest-deftest test ()
  :case \"¶1\"
  \"<right>\"
  ;; Comment
  ;; Comment

  ;; Comment
  :result \"1¶\")
")

(etest-deftest etest-mark-test ()
  "Mark is properly handled."
  :case "¶foo×"
  "<right>"
  :result "f¶oo×"
  "<right>"
  :result "fo¶o×")


(defun etest-make-config ()
  '(:init ((mode . text))))

(etest-deftest etest-config-fun-test ()
  "Configuration is picked up from function."
  :config (etest-make-config)
  (should (eq major-mode 'text-mode)))


(defvar etest-some-config '(:init ((mode . text))))

(etest-deftest etest-config-var-test ()
  "Configuration is picked up from variable."
  :config etest-some-config
  (should (eq major-mode 'text-mode)))


;; `let' doesn't seem to work here, perhaps an interaction between
;; scoping in macros and file-local variables
(setq etest-local-config '(:init ((mode . text))))

(etest-deftest etest-config-local-test ()
  "Local configuration is picked up."
  (should (eq major-mode 'text-mode)))

(etest-deftest etest-config-keyword-test ()
  "Keyword config has precedence over local config."
  :config nil
  (should (eq major-mode 'fundamental-mode)))

(setq etest-local-config nil)

(etest-deftest etest-default-mode ()
  "Default mode is fundamental.
Also tests local config test is cleaned up properly."
  (should (eq major-mode 'fundamental-mode)))


(etest-deftest etest-truncation-test ()
  "`backward-up-list' isn't confused by syntax in strings."
  :case "
(etest-deftest test ()
  :result \"\")
\")\"
"
  (etest-update)
  :result "¶
(etest-deftest test ()
  :result \"\")
\")\"
")
