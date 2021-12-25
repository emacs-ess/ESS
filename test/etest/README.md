
# Introduction

etest is an ERT extension to make it easier to test Emacs behavior such as cursor and mark movement and inferior buffer output.

This package contains programming tools as well as user commands. Add `ESS/test/etest/` to your load-path or install it with:

```
M-x package-install-file
```


# Usage

The interface of etest is inspired by the ERT and use-package macros. Like ERT, the `etest-deftest` macro has a similar syntax to `defun` and `defmacro`. It takes an ERT test name, an empty list of arguments, and an optional documentation string:

```elisp
(etest-deftest my-test ()
  "Documentation string for the unit test.")
```

This macro wraps around `ert-deftest` so these tests can be run with the usual ERT tools.


## Defining test cases

At its heart, the `etest-deftest` macro is an interpreter of lisp forms and keywords command. The main keywords to know about are `:case` and `:result`.


### `:case`

All etest tests are run in a dedicated buffer. The `:case` takes a string that defines the content of the test buffer. The special characters `¶` and `×` specify the positions of the Emacs point and mark cursors in that buffer. These characters were chosen to be 1 character wide so they don't distract from the general structure of the test case, and outside of the ASCII range so they don't conflict with valid symbols in the tested languages.

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer.")
```

The mark cursor `×` is optional. When the point cursor `¶` is omitted, the cursor is set at `(point-min)`.


### Lisp forms and commands

Any lisp form that isn't prefixed with a keyword is evaluated as lisp. These forms may contain ert code such as `should` or may cause side effects in the test buffer.

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer."
  (forward-word)
```

These free forms may also be commands described in strings as key descriptions (the sort that you would pass to `kbd`). The key is looked up in the active keymaps of the test buffer:

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer."
  "M-f"
```

These can be freely mixed:

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer."
  "M-f"
  (forward-word)
  "M-f"
```


### `:result`

You can test how these commands affect the test buffer with the `:result` keyword. An easy way to add this keyword is to first supply an empty string:

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer."
  "M-f"
  :result "")
```

Then move the point inside the `etest-deftest` block and call `M-x etest-update`. This function runs the test command in the dedicated test buffer, examines the state of the buffer for each `:result` keyword, and updates it in place. You should now see the `:result` value filled in:

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer."
  "M-f"
  :result "Initial¶ text in the test buffer.")
```

This block has now become a unit test for the behavior of the `"M-f"` keybinding. When ERT runs the test, the contents of the buffer as well as the position of the cursor after typin `"M-f"` must correspond to the comparison case recorded in `:result`, otherwise the test fails.

Note that you don't need a `:result` keyword to perform tests. You can also use `should` statement or any other ERT testing device in keyword-free forms:

```elisp
(etest-deftest my-test ()
  :case "¶Initial text in the test buffer."
  (should (looking-at "Initial")))
```


## Initializing and cleaning up the test buffer

### `:init`

By default, the test buffer is in fundamental mode. The `:init` keyword offers a convenient initialization syntax. It takes an alist of local variables with the same syntax as [directory local variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html).

```elisp
(etest-deftest my-test ()
  :init ((mode . text)
         (var . value)))
```

The `eval` command supports arbitrary lisp code. This is a convenient way of abstracting initialization in a function:

```elisp
(defun my-init ()
  (text-mode 1)
  (setq var value))

(etest-deftest my-test ()
  :init ((eval . (my-init))))
```


### `:cleanup`

The `:cleanup` takes lisp expressions evaluated in LIFO order with unwind protection when the test exits. It is useful to clean up side effects introduced in your tests:

```elisp
(defun my-init ()
  (setq-local my-buf (get-buffer-create "my-buffer")))

(etest-deftest my-test ()
  :init ((eval . (my-init)))
  :cleanup (kill-buffer my-buf))
```


## Reusing mode-specific configuration

Mode-specific configuration can be stored in a variable or created with a function. This typically includes an `:init` keyword that sets up the major-mode for the test buffer:

```elisp
(defvar my-config '(:init ((mode . text))))
```

The configuration may contain any valid etest code and is registered with the `:config` keyword. This keyword must appear before any other keywords:

```elisp
(etest-deftest my-test ()
  :config my-config))
```

You can also set it with the buffer-local variable `etest-local-config`. It is convenient to set it as a file-local variable, this way `etest-update` is automatically aware of the configuration relevant to the test file.

```elisp
(etest-deftest my-test ())

;; Local Variables:
;; etest-local-config: my-config
;; End:
```

If supplied, the `:config` keyword has precedence over the configuration stored in `etest-local-config` and completely replaces it.


## Checking the contents of an inferior buffer

In addition to checking the side effects in the test buffer, it is often useful to check the side effects in an auxiliary buffer. For instance, checking the output in an inferior process buffer.


### `:inf-buffer`

Let's define rudimentary initialization and cleanup functions for an R inferior. These save the inferior buffer in a buffer-local variable `inf-buf`.

```elisp
(defun my-inferior-init ()
  (setq-local inf-buf (run-ess-r))
  (setq-local inf-proc (get-buffer-process inf-buf))
  (setq ess-local-process-name (process-name inf-proc))
  (with-current-buffer inf-buf
    (comint-clear)))

(defun my-inferior-cleanup ()
  (kill-process inf-proc))
```

Use `:inf-buffer` keyword to set up `inf-buf` as the inferior buffer to monitor:

```elisp
(etest-deftest my-test ()
  :init ((mode . r)
         (eval . (my-inferior-init)))
  :cleanup (my-inferior-cleanup)
  :inf-buffer inf-buf)
```

This enables usage of `:inf-result`.


### `:inf-result`

We'll test the result of calling `C-c C-c` in a buffer containing `1 + 1`. The `:inf-result` keyword is similar to `:result`. Set it to the empty string to start with:

```elisp
(etest-deftest my-test ()
  :init ((mode . r)
         (eval . (my-inferior-init)))
  :cleanup (my-inferior-cleanup)
  :inf-buffer inf-buf
  :case "1 + 1"
  "C-c C-c"
  :inf-result "")
```

After calling `etest-update` we get:

```elisp
(etest-deftest my-test ()
  :init ((mode . r)
         (eval . (my-inferior-init)))
  :cleanup (my-inferior-cleanup)
  :inf-buffer inf-buf
  :case "1 + 1"
  "C-c C-c"
  :inf-result "> 1 + 1
[1] 2
> ")
```


## Other features

### Using multiple cursors

Use multiple `¶` characters to define multiple cursors. Lisp code and commands are run once for each cursor. This is useful for testing motions on many keywords at a time for instance:

```elisp
(etest-deftest my-test ()
  :case "¶if ¶while ¶for"
  "M-f"
  :result "if¶ while¶ for¶")
```

Or for testing properties of many keywords:

```elisp
(etest-deftest my-test ()
  "Test that control flow keywords are only fontified if they are
followed by an open parenthesis."
  :init ((mode . r))

  :case "¶if ¶while ¶for"
  (should (not (face-at-point)))

  :case "¶if () ¶while () ¶for ()"
  (should (eq (face-at-point) 'ess-keyword-face)))
```


### Checking messages

#### `:messages`

Messages are normally inhibited during the duration of the test. If you want to check the messages, use the `:messages` keyword. As usual, start with an empty string value:

```elisp
(etest-deftest my-test ()
  (message "foo")
  (message "bar")
  :messages "")
```

And call `etest-update` to define a comparison case:

```elisp
(etest-deftest my-test ()
  (message "foo")
  (message "bar")
  :messages "foo
bar")
```
