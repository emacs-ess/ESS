(require 'cl-lib)
(require 'ert)
(require 'ess-julia)

(defmacro ess-julia-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with `ess-julia-mode' as the active
mode holding TEXT.  If the string \"¶\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	 (ess-julia-mode-hook nil))
     (with-temp-buffer
       (ess-julia-mode)
       (let ((point (string-match "¶" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       ,@body)))
(def-edebug-spec ess-julia-test-with-temp-text (form body))

(ert-deftest ess-julia-function-beg-end ()
  (ess-julia-test-with-temp-text
      "function x(a)\n  a + 1\nend\n"
    (search-forward "x(a)")
    (ess-beginning-of-function)
    (should (eql (point) 1))
    (should (cl-equalp (ess-end-of-function) '(1 26))))
  (ess-julia-test-with-temp-text
      "function x(a)\n  a + 1\n  function (b)\n    b + 2\n    end\nend\n"
    (search-forward "x(a)")
    (should (eql (ess-beginning-of-function) 1))
    (search-forward "b + 2")
    ;; Ignore nested functions:
    (should (eql (ess-beginning-of-function) 1))
    ;; FIXME: This isn't right, should be 1 67?
    (should (cl-equalp (ess-end-of-function) '(1 59))))
  (ess-julia-test-with-temp-text
      "macro p(n)\n  return n\nend\n"
    (search-forward "return")
    (ess-beginning-of-function)
    (should (eql (point) 1)))
  (ess-julia-test-with-temp-text
      "f(x, y) = x + y"
    (search-forward "x + ")
    (ess-beginning-of-function)
    (should (eql (point) 1))
    (should (cl-equalp (ess-end-of-function) `(,(point-min) ,(point-max)))))
  (ess-julia-test-with-temp-text
      "1 + 1"
    (search-forward "+")
    (should-error (ess-beginning-of-function))
    (should-error (ess-end-of-function))))
