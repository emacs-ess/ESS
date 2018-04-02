;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")
  (bug-reference-url-format . "https://github.com/emacs-ess/ess/issues/%s"))
 (emacs-lisp-mode
  ;; (coding . utf-8-unix) ;; emacs 24.3.5 gives "Coding cannot be specified by dir-locals"
  (indent-tabs-mode)
  (whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark)))
