;;; ess-r-syntax.el --- Utils to work with R code

;; Copyright (C) 2015 Lionel Henry

;; Author: Lionel Henry <lionel.hry@gmail.com>
;; Created: 12 Oct 2015
;; Maintainer: ESS-core <ESS-core@r-project.org>

;; This file is part of ESS

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/

;;; Commentary:

;; API is not yet stable.

;;; Code:


;;*;; Utils

;; The three following wrappers return t if successful, nil on error
(defun ess-backward-sexp (&optional N)
  (ess-forward-sexp (- (or N 1))))

(defun ess-forward-sexp (&optional N)
  (or N (setq N 1))
  (condition-case nil
      (prog1 t
        (goto-char (or (scan-sexps (point) N)
                       (buffer-end N))))
    (error nil)))

(defun ess-up-list (&optional N)
  (condition-case nil
      (progn (up-list N) t)
    (error nil)))

;; Going forth and back is a fast and reliable way of skipping in
;; front of the next sexp despite blanks, newlines and comments that
;; may be in the way.
(defun ess-forth-and-back-sexp ()
  (ess-save-excursion-when-nil
    (and (ess-forward-sexp)
         (ess-backward-sexp))))

(defun ess-back-and-forth-sexp ()
  (ess-save-excursion-when-nil
    (and (ess-backward-sexp)
         (ess-forward-sexp))))

;; Avoids let-binding a variable just to check a returned position is
;; not nil
(defun ess-goto-char (pos)
  (when pos
    (goto-char pos)))

(defun ess-looking-at (regex &optional newlines)
  "Compared to a simple `(looking-at)', this uses sexp motions to
skip any blanks, newlines and comments. Should be more reliable
and possibly faster than using complicated regexes."
  (save-excursion
    (ess-skip-blanks-forward newlines)
    (looking-at regex)))

(defun ess-back-to-indentation ()
  "Move point to the first non-whitespace character on this line.
This non-interactive version of (back-to-indentation) should not
be advised"
  (beginning-of-line 1)
  (skip-syntax-forward " " (line-end-position))
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(defmacro ess-save-excursion-when-nil (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(let ((orig-point (point)))
     (cond ((progn ,@body))
           (t (prog1 nil
                (goto-char orig-point))))))

(defmacro ess-while (test &rest body)
  "Like (while) but return `t' when body gets executed once."
  (declare (indent 1)
           (debug (&rest form)))
  `(let (executed)
     (while ,test
       (setq executed t)
       ,@body)
     executed))

(defmacro ess-at-indent-point (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(save-excursion
     (goto-char indent-point)
     (ess-back-to-indentation)
     (progn ,@body)))

(defmacro ess-at-containing-sexp (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  '(when (null containing-sexp)
     (error "Internal error: containing-sexp is nil"))
  `(save-excursion
     (goto-char containing-sexp)
     (progn ,@body)))

(defmacro ess-any (&rest forms)
  "Evaluates all arguments and return non-nil if one of the
arguments is non-nil. This is useful to trigger
side-effects. FORMS follows the same syntax as arguments to
`(cond)'."
  (declare (indent 0) (debug nil))
  `(let ((forms (list ,@(mapcar (lambda (form) `(progn ,@form)) forms))))
     (some 'identity (mapcar 'eval forms))))


;;*;; Point predicates

(defun ess-point-in-call-p (&optional call)
  "Is point in a function or indexing call?"
  (let ((containing-sexp (or (bound-and-true-p containing-sexp)
                             (ess-containing-sexp-position))))
    (save-excursion
      (and (prog1 (ess-goto-char containing-sexp)
             (ess-climb-chained-delims))
           (save-excursion
             (forward-char)
             (ess-up-list))
           (or (ess-looking-at-call-opening "(")
               (looking-at "\\["))
           (ess-point-on-call-name-p call)))))

(defun ess-point-in-continuation-p ()
  (unless (or (looking-at ",")
              (ess-looking-at-call-opening "[[(]"))
    (or (save-excursion
          (ess-jump-object)
          (and (not (ess-looking-at-parameter-op-p))
               (ess-looking-at-operator-p)))
        (save-excursion
          (ess-climb-object)
          (ess-climb-operator)
          (and (ess-looking-at-operator-p)
               (not (ess-looking-at-parameter-op-p)))))))

(defun ess-point-on-call-name-p (&optional call)
  (save-excursion
    (ess-climb-call-name call)))

(defun ess-point-in-prefixed-block-p (&optional call)
  "Is point in a prefixed block? Prefixed blocks refer to the
blocks following function declarations, control flow statements,
etc.

If CALL not nil, check if the prefix corresponds to CALL. If nil,
return the prefix."
  (save-excursion
    (ess-climb-outside-prefixed-block call)))

(defun ess-point-in-comment-p (&optional state)
  (let ((state (or state (syntax-ppss))))
        (eq (syntax-ppss-context state) 'comment)))

(defun ess-point-in-string-p (&optional state)
  (let ((state (or state (syntax-ppss))))
        (eq (syntax-ppss-context state) 'string)))


;;*;; Syntactic Travellers and Predicates

;;;*;;; Blanks, Characters and Comments

(defun ess-skip-blanks-backward (&optional newlines)
  "Skip blanks and newlines backward, taking end-of-line comments
into account."
  (ess-any ((ess-skip-blanks-backward-1))
           ((when newlines
              (ess-while (and (/= (point) (point-min))
                              (= (point) (line-beginning-position)))
                (forward-line -1)
                (goto-char (ess-code-end-position))
                (ess-skip-blanks-backward-1))))))

(defun ess-skip-blanks-backward-1 ()
  (and (/= (point) (point-min))
       (/= 0 (skip-chars-backward " \t"))))

(defun ess-skip-blanks-forward (&optional newlines)
  "Skip blanks and newlines forward, taking end-of-line comments
into account."
  (ess-any ((/= 0 (skip-chars-forward " \t")))
           ((ess-while (and newlines
                            (= (point) (ess-code-end-position))
                            (when (ess-save-excursion-when-nil
                                    ;; Handles corner cases such as point being on last line
                                    (let ((orig-point (point)))
                                      (forward-line)
                                      (ess-back-to-indentation)
                                      (> (point) orig-point)))
                              (skip-chars-forward " \t")
                              t))))))

(defun ess-jump-char (char)
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (when (looking-at char)
      (goto-char (match-end 0)))))

(defun ess-climb-comment ()
  (when (and (ess-point-in-comment-p)
             (not (ess-roxy-entry-p)))
    (prog1 (comment-beginning)
     (skip-chars-backward "#+[ \t]*"))))

(defun ess-looking-back-closing-p ()
  (memq (char-before) '(?\] ?\} ?\))))

(defun ess-looking-back-boundary-p ()
  (looking-back "[][ \t\n(){},]" (1- (point))))


;;;*;;; Blocks

(defun ess-block-opening-p ()
  (save-excursion
    (cond
     ((looking-at "{"))
     ;; Opening parenthesis not attached to a function opens up a
     ;; block too. Only pick up those that are last on their line
     ((ess-looking-at-block-paren-p)))))

(defun ess-block-closing-p ()
  (save-excursion
    (cond
     ((looking-at "}"))
     ((looking-at ")")
      (forward-char)
      (backward-sexp)
      (not (looking-back
            (concat ess-R-name-pattern "[[:blank:]]*")
            (line-beginning-position)))))))

(defun ess-block-p ()
  (or (save-excursion
        (when containing-sexp
          (goto-char containing-sexp)
          (ess-block-opening-p)))
      (ess-unbraced-block-p)))

;; Parenthesised expressions
(defun ess-looking-at-block-paren-p ()
  (and (looking-at "(")
       (not (ess-looking-back-attached-name-p))))

(defun ess-climb-block (&optional ignore-ifelse)
  (ess-save-excursion-when-nil
    (cond
     ((and (not ignore-ifelse)
           (ess-climb-if-else 'to-start)))
     ((and (eq (char-before) ?\})
           (prog2
               (forward-char -1)
               (ess-up-list -1)
             (ess-climb-block-prefix)))))))

(defvar ess-prefixed-block-patterns
  (mapcar (lambda (fun) (concat fun "[ \t\n]*("))
          '("function" "if" "for" "while")))

(defun ess-looking-at-prefixed-block-p (&optional call)
  (if call
      (looking-at (concat call "[ \t]*("))
    (some 'looking-at ess-prefixed-block-patterns)))

(defun ess-unbraced-block-p (&optional ignore-ifelse)
  "This indicates whether point is in front of an unbraced
prefixed block following a control flow statement. Returns
position of the control flow function (if, for, while, etc)."
  (save-excursion
    (and (ess-backward-sexp)
         (or (and (looking-at "else\\b")
                  (not ignore-ifelse))
             (and (looking-at "(")
                  (ess-backward-sexp)
                  (some 'looking-at ess-prefixed-block-patterns)
                  (if ignore-ifelse
                      (not (looking-at "if\\b"))
                    t)))
         (point))))

(defun ess-climb-block-prefix (&optional call ignore-ifelse)
  "Climb the prefix of a prefixed block. Prefixed blocks refer to
the blocks following function declarations, control flow
statements, etc.

Should be called either in front of a naked block or in front
of the curly brackets of a braced block.

If CALL not nil, check if the prefix corresponds to CALL. If nil,
return the prefix."
  (ess-save-excursion-when-nil
    (or (and (not ignore-ifelse)
             (prog1 (and (ess-climb-if-else-call)
                         (or (null call)
                             (looking-at call)))
               (when (looking-at "else\\b")
                 (ess-skip-curly-backward))))
        (let ((pos (ess-unbraced-block-p ignore-ifelse)))
          (and (ess-goto-char pos)
               (if call
                   (looking-at call)
                 (cond ((looking-at "function")
                        "function")
                       ((looking-at "for")
                        "for")
                       ((looking-at "if")
                        "if")
                       ((looking-at "else")
                        "else"))))))))

(defun ess-climb-outside-prefixed-block (&optional call)
  "Climb outside of a prefixed block."
  (let ((containing-sexp (or (bound-and-true-p containing-sexp)
                             (ess-containing-sexp-position))))
    (or (ess-save-excursion-when-nil
          (and (ess-goto-char containing-sexp)
               (looking-at "{")
               (ess-climb-block-prefix call)))
        (ess-climb-outside-unbraced-block call))))

(defun ess-climb-outside-unbraced-block (&optional call)
  (ess-save-excursion-when-nil
    (while (and (not (ess-unbraced-block-p))
                (or (ess-climb-outside-continuations)
                    (ess-climb-outside-call))))
    (ess-climb-block-prefix call)))

(defun ess-jump-block ()
  (cond
   ;; if-else blocks
   ((ess-jump-if-else))
   ;; Prefixed blocks such as `function() {}'
   ((ess-looking-at-prefixed-block-p)
    (ess-jump-prefixed-block))
   ;; Naked blocks
   ((and (or (looking-at "{")
             (ess-looking-at-block-paren-p))
         (ess-forward-sexp)))))

(defun ess-jump-prefixed-block (&optional call)
  (ess-save-excursion-when-nil
    (when (ess-looking-at-prefixed-block-p call)
      (ess-forward-sexp 2)
      (ess-skip-blanks-forward t)
      (if (looking-at "{")
          (ess-forward-sexp)
        (prog1 (ess-jump-expression)
          (ess-jump-continuations))))))


;;;*;;; Calls

(defun ess-call-closing-p ()
  (save-excursion
    (when (cond ((looking-at ")")
                 (ess-up-list -1))
                ((looking-at "]")
                 (when (ess-up-list -1)
                   (prog1 t (ess-climb-chained-delims)))))
      (ess-looking-back-attached-name-p))))

(defun ess-looking-at-call-opening (pattern)
  (and (looking-at pattern)
       (ess-looking-back-attached-name-p)))

;; Should be called just before the opening brace
(defun ess-looking-back-attached-name-p ()
  (save-excursion
    (ess-climb-object)))

(defun ess-looking-at-parameter-op-p ()
  "Are we looking at a function argument? To be called just
before the `=' sign."
  (save-excursion
    (and (looking-at "[ \t]*=[^=]")
         (ess-climb-object)
         (looking-back "[(,][ \t\n]*" (line-beginning-position 0)))))

(defun ess-looking-at-arg-p ()
  (save-excursion
    (ess-jump-arg)))

(defun ess-looking-at-parameter-p ()
  (save-excursion
    (ess-jump-parameter)))

(defun ess-jump-parameter ()
  (ess-save-excursion-when-nil
    (and (ess-jump-name)
         (when (looking-at "[ \t]*=\\([^=]\\)")
           (goto-char (match-beginning 1))
           (ess-skip-blanks-forward)
           t))))

(defun ess-jump-arg ()
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (ess-any ((ess-jump-parameter))
             ((ess-jump-expression))
             ((ess-jump-continuations)))))

(defun ess-arg-bounds ()
  "Should be called in front of the argument."
  (save-excursion
    (let ((beg (point)))
      (and (ess-jump-arg)
           (list beg (point))))))

(defun ess-climb-call (&optional call)
  "Climb functions (e.g. ggplot) and parenthesised expressions."
  (or (ess-while (ess-save-excursion-when-nil
                   (ess-climb-name)
                   (and (ess-climb-chained-delims ?\])
                        ;; (ess-climb-expression)
                        (if (eq (char-before) ?\))
                            (ess-climb-call)
                          (ess-climb-name))
                        )))
      (ess-save-excursion-when-nil
        (when (and (memq (char-before) '(?\] ?\) ?\}))
                   (ess-backward-sexp))
          (if call
              (and (ess-climb-name)
                   (looking-at call)))
          (prog1 t
            (ess-climb-name))))))

(defun ess-climb-call-name (&optional call)
  (ess-save-excursion-when-nil
    (ess-jump-name)
    (ess-skip-blanks-forward)
    (and (ess-looking-at-call-opening "[[(]")
         (ess-climb-name)
         (or (null call)
             (looking-at call)))))

(defun ess-step-to-first-arg ()
  (let ((containing-sexp (ess-containing-sexp-position)))
    (cond ((ess-point-in-call-p)
           (goto-char containing-sexp)
           (forward-char)
           t)
          ((ess-point-on-call-name-p)
           (ess-jump-name)
           (ess-skip-blanks-forward)
           (forward-char)
           t))))

(defun ess-jump-to-next-arg ()
  (and (ess-jump-arg)
       (prog1 (ess-jump-char ",")
         (ess-skip-blanks-forward t))))

(defun ess-jump-call ()
  (ess-save-excursion-when-nil
    (or (and (ess-jump-object)
             (cond ((eq (char-before) ?\)))
                   ((looking-at "\\[")
                    (ess-jump-chained-brackets))
                   ((looking-at "(")
                    (ess-forward-sexp))))
        (and (looking-at "[ \t]*(")
             (ess-forward-sexp)))))

(defun ess-looking-at-call-p ()
  (save-excursion
    (ess-jump-object)
    (ess-skip-blanks-forward)
    (looking-at "[[(]")))

(defun ess-climb-chained-delims (&optional delim)
  "Should be called with point between delims, e.g. `]|['."
  (setq delim (if delim
                  (list delim)
                '(?\] ?\))))
  (ess-while (ess-save-excursion-when-nil
               (when (memq (char-before) delim)
                 (ess-backward-sexp)))))

(defun ess-jump-chained-brackets ()
  (ess-while (ess-save-excursion-when-nil
               (when (eq (char-after) ?\[)
                 (ess-forward-sexp)))))

(defun ess-climb-outside-call (&optional call)
  (let ((containing-sexp (ess-containing-sexp-position)))
    (if (ess-point-in-call-p)
        (ess-save-excursion-when-nil
          (goto-char containing-sexp)
          (ess-climb-chained-delims)
          (and (ess-climb-name)
               (or (null call)
                   (looking-at call))))
      ;; At top level or inside a block, check if point is on the
      ;; function name.
      (ess-save-excursion-when-nil
        (let ((orig-pos (point)))
          (and (ess-jump-name)
               (looking-at "[[(]")
               (ess-climb-name)
               (or (null call)
                   (looking-at call))
               (/= (point) orig-pos)))))))

(defun ess-climb-outside-calls ()
  (ess-while (ess-climb-outside-call)))

(defun ess-jump-inside-call ()
  (ess-save-excursion-when-nil
    (when (ess-jump-name)
      (ess-skip-blanks-forward)
      (when (looking-at "(")
        (forward-char)
        t))))

(defun ess-args-bounds (&optional marker)
  (let ((containing-sexp (ess-containing-sexp-position)))
    (when (ess-point-in-call-p)
      (save-excursion
        (let ((beg (1+ containing-sexp))
              (call-beg (ess-at-containing-sexp
                          (ess-climb-name)
                          (point))))
          ;; (ess-up-list) can't find its way when point is on a
          ;; backquoted name, so start from `beg'.
          (and (goto-char beg)
               (ess-up-list)
               (prog1 t
                 (forward-char -1))
               (let ((end (if marker
                              (point-marker)
                            (point))))
                 (list beg end call-beg))))))))

(defun ess-args-alist ()
  "Return all arguments as an alist with cars set to argument
names and cdrs set to the expressions given as argument. Both
cars and cdrs are returned as strings."
  (save-excursion
    (when (ess-step-to-first-arg)
      (let (args current-arg)
        (while (and (setq current-arg (ess-cons-arg))
                    (setq args (nconc args (list current-arg)))
                    (ess-jump-to-next-arg)))
        args))))

(defun ess-cons-arg ()
  "Return a cons cell of the current argument with car set to the
parameter name (nil if not specified) and cdr set to the argument
expression."
  (save-excursion
    (ess-skip-blanks-forward t)
    (let ((param (when (ess-looking-at-parameter-p)
                   (buffer-substring-no-properties
                    (point)
                    (prog2
                        (ess-jump-name)
                        (point)
                      (ess-jump-char "=")
                      (ess-skip-blanks-forward)))))
          (arg (buffer-substring-no-properties
                (point)
                (progn
                  (ess-jump-arg)
                  (point)))))
      (cons param arg))))


;;;*;;; Statements

(defun ess-looking-back-operator-p (&optional fun-arg)
  (save-excursion
    (and (ess-climb-operator)
         (if (not fun-arg)
             (not (ess-looking-at-parameter-op-p))
           t))))

(defun ess-climb-lhs (&optional no-fun-arg climb-line)
  (ess-save-excursion-when-nil
    (let ((start-line (line-number-at-pos)))
      (ess-climb-operator)
      (when (and (or climb-line (equal (line-number-at-pos) start-line))
                 (ess-looking-at-definition-op-p no-fun-arg))
        (prog1 t
          (ess-climb-expression))))))

(defun ess-jump-lhs ()
  (ess-save-excursion-when-nil
    (and (ess-jump-name)
         (ess-looking-at-definition-op-p)
         (ess-jump-operator))))

;; Useful to check presence of operators. Need to check for
;; (point-min) because that won't work if there is no previous sexp
;; Should be called right at the beginning of current sexp.
(defun ess-climb-operator ()
  (ess-save-excursion-when-nil
    (let ((orig-pos (point)))
      (while (forward-comment -1))
      (cond ((memq (char-before) '(?, ?\;))
             nil)
            ((eq (char-before) ?%)
             (forward-char -1)
             (skip-chars-backward "^%")
             (forward-char -1)
             (ess-skip-blanks-backward)
             t)
            ;; Fixme: Don't use SEXP motion, simply check for ops
            ((ess-backward-sexp)
             ;; When there is only empty space or commented code left to
             ;; climb (e.g. roxygen documentation), there is no previous
             ;; SEXP, but (ess-backward-sexp) will nevertheless climb the
             ;; empty space without failing. So we need to skip it.
             (while (forward-comment 1))
             ;; Handle %op% operators
             (when (and (< (point) orig-pos)
                        (ess-forward-sexp)
                        (ess-looking-at-operator-p))
               (prog1 t
                 (when (and (equal (char-after) ?=)
                            (equal (char-before) ?:))
                   (forward-char -1)
                   (ess-skip-blanks-backward)))))))))

;; Currently doesn't check that the operator is not binary
(defun ess-climb-unary-operator ()
  (ess-save-excursion-when-nil
    (ess-skip-blanks-backward t)
    (when (memq (char-before) '(?+ ?- ?! ?? ?~))
      (forward-char -1)
      t)))

;; Currently returns t if we climbed lines, nil otherwise.
(defun ess-climb-continuations (&optional cascade ignore-ifelse)
  (let ((start-line (line-number-at-pos))
        (moved 0)
        (last-pos (point))
        last-line prev-point def-op expr)
    (setq last-line start-line)
    (when (ess-while (and (<= moved 1)
                          (or (ess-save-excursion-when-nil
                                (and (ess-climb-operator)
                                     (ess-climb-continuations--update-state 'op)
                                     (ess-climb-expression ignore-ifelse)))
                              (ess-climb-unary-operator))
                          (/= last-pos (point)))
            (ess-climb-continuations--update-state)
            (setq last-pos (point)))
      (when (and prev-point
                 (or (= moved 3)
                     (not expr)))
        (goto-char prev-point))
      (if def-op 'def-op (< (line-number-at-pos) start-line)))))

(defun ess-climb-continuations--update-state (&optional op)
  ;; Climbing multi-line expressions should not count as moving up
  (when op
    (setq expr (ess-looking-back-closing-p)))
  (let ((cur-line (line-number-at-pos)))
    (when (and last-line
               (< cur-line last-line)
               (or cascade (not expr)))
      (setq moved (1+ moved))
      (setq last-line cur-line)))
  ;; Don't update counter after climbing operator or climbing too high
  (when (and (not op)
             (<= moved 1))
    (setq prev-point (point)))
  (when (and (ess-looking-at-definition-op-p)
             (<= moved 1))
    (setq def-op t))
  t)

(defun ess-jump-operator ()
  (when (ess-looking-at-operator-p)
    (goto-char (match-end 1))
    (ess-skip-blanks-forward t)
    t))

(defun ess-jump-continuation ()
  (and (ess-jump-operator)
       (ess-jump-expression)))

(defun ess-jump-continuations ()
  (let (last-pos)
    (when (ess-while (and (or (null last-pos)
                              (/= (point) last-pos))
                          (setq last-pos (point))
                          (ess-jump-continuation)))
      ;; In calls, operators can start on newlines
      (let ((start-line (line-number-at-pos)))
        (when (ess-save-excursion-when-nil
                (and (ess-point-in-call-p)
                     (ess-skip-blanks-forward t)
                     (/= (line-number-at-pos) start-line)
                     (ess-looking-at-operator-p)))
          (ess-jump-continuations)))
      t)))

(defun ess-looking-at-continuation-p (&optional or-parameter)
  (or (save-excursion
        (ess-skip-blanks-backward t)
        (ess-back-and-forth-sexp)
        (when (ess-looking-at-operator-p)
          (if or-parameter t
            (not (ess-looking-at-parameter-op-p)))))
      (save-excursion
        (ess-climb-block-prefix))
      (save-excursion
        (or (looking-at "else\\b")
            (ess-climb-if-else-call)))))

(defvar ess-R-operator-pattern "<-\\|:=\\|!=\\|%[^ \t]*%\\|[-:+*/><=&|~]"
  "Regular expression for an operator")

(defvar ess-R-definition-op-pattern "<<?-\\|:=\\|~"
  "Regular expression for a definition operator")

(defun ess-looking-at-operator-p ()
  (looking-at (concat "[[:blank:]]*\\(" ess-R-operator-pattern "\\)")))

(defun ess-looking-at-definition-op-p (&optional no-fun-arg)
  (save-excursion
    (skip-chars-forward "[ \t]")
    (or (looking-at ess-R-definition-op-pattern)
        (and (looking-at "=[^=]")
             (if no-fun-arg
                 (not (ess-looking-at-parameter-op-p))
               t)))))

(defun ess-looking-at-assignment-op-p ()
  (save-excursion
    (ess-skip-blanks-forward t)
    (and (looking-at "<-\\|=")
         (not (ess-looking-at-parameter-op-p)))))

(defun ess-looking-back-definition-op-p (&optional no-fun-arg)
  (save-excursion
    (and (ess-backward-sexp)
         (ess-forward-sexp)
         (ess-looking-at-definition-op-p no-fun-arg))))

(defun ess-climb-outside-continuations ()
  (ess-any ((unless (ess-looking-back-boundary-p)
              (ess-climb-expression)))
           ((ess-while (ess-climb-continuations)))))

(defun ess-continuations-bounds (&optional marker)
  (save-excursion
    (let ((orig-point (point))
          (beg (progn
                 (ess-climb-outside-continuations)
                 (point))))
      (when beg
        (ess-jump-expression)
        (ess-jump-continuations)
        (let ((end (if marker
                       (point-marker)
                     (point))))
          (list beg end))))))

(defun ess-climb-to-top-level ()
  (while (ess-goto-char (ess-containing-sexp-position)))
  (ess-climb-outside-continuations))


;;;*;;; Statements: Control Flow

(defun ess-climb-if-else-call (&optional multi-line)
  "Climb if, else, and if else calls."
  (ess-save-excursion-when-nil
    (ess-backward-sexp)
    (cond ((looking-at "(")
           (when (and (ess-backward-sexp)
                      (looking-at "if\\b"))
             ;; Check for `else if'
             (prog1 t
               (ess-save-excursion-when-nil
                 (let ((orig-line (line-number-at-pos)))
                   (and (ess-backward-sexp)
                        (or multi-line
                            (eq orig-line (line-number-at-pos)))
                        (looking-at "else\\b")))))))
          ((looking-at "else\\b")))))

(defun ess-climb-if-else-body (&optional from-else)
  (cond
   ;; Climb braced body
   ((ess-save-excursion-when-nil
      (and (when (progn (ess-skip-blanks-backward t)
                        (eq (char-before) ?\}))
             (prog1 t (forward-char -1)))
           (ess-up-list -1))))
   ;; Climb unbraced body
   ((when from-else
      (ess-save-excursion-when-nil
        (ess-skip-blanks-backward t)
        (prog1 (ess-climb-expression 'ignore-ifelse)
          (or (ess-climb-continuations nil 'ignore-ifelse)
              (ess-climb-block-prefix nil 'ignore-ifelse))))))))

(defun ess-climb-if-else (&optional to-start)
  "Climb horizontal as well as vertical if-else chains, with or
without curly braces."
  ;; Don't climb if we're atop the current chain of if-else
  (unless (looking-at "if\\b")
    (ess-save-excursion-when-nil
      (let ((from-else (looking-at "else\\b")))
        (when (and (ess-climb-if-else-body from-else)
                   (ess-climb-if-else-call to-start))
          ;; If we start from a final else and climb to another else, we
          ;; are in the wrong chain of if-else. In that case,
          ;; climb-recurse to the top of the current chain and climb
          ;; again to step in the outer chain.
          (when (and from-else (ess-looking-at-final-else))
            (ess-climb-if-else 'to-start)
            (ess-climb-continuations)
            (ess-climb-block-prefix nil 'ignore-ifelse)
            (ess-climb-if-else-call nil))
          (ess-maybe-climb-broken-else)
          (when to-start
            (ess-climb-if-else to-start))
          t)))))

;; Handles multi-line such as if \n else, with comments in the way etc
(defun ess-looking-at-final-else ()
  (or (save-excursion
        (and (looking-at "else\\b")
             (ess-forward-sexp)
             (ess-forth-and-back-sexp)
             (not (looking-at "if\\b"))))))

;; Broken else: if \n else
(defun ess-maybe-climb-broken-else (&optional same-line)
  (ess-save-excursion-when-nil
    ;; Don't record current line if not needed (expensive operation)
    (let ((cur-line (when same-line (line-number-at-pos))))
      (and (ess-backward-sexp)
           (looking-at "else\\b")
           (if same-line
               (= cur-line (line-number-at-pos))
             t)))))

(defun ess-skip-curly-backward ()
  (re-search-backward "}[ \t]*" (line-beginning-position) t))

(defun ess-jump-if-else ()
  (let (from)
    (ess-while (ess-save-excursion-when-nil
                 (ess-skip-blanks-forward t)
                 (cond
                  ((and (not (eq from 'if))
                        (ess-jump-if)
                        (setq from 'if)))
                  ((looking-at "else")
                   (ess-forward-sexp)
                   (or (ess-jump-if)
                       (progn
                         (ess-skip-blanks-forward t)
                         (ess-jump-expression)))
                   (setq from 'else))
                  (t
                   nil))))))

(defun ess-jump-if ()
  (ess-save-excursion-when-nil
    (ess-skip-blanks-forward t)
    (and (looking-at "if[ \t\n]*(")
         (ess-forward-sexp 2)
         (progn
           (ess-skip-blanks-forward t)
           (ess-jump-expression)))))


;;;*;;; Function Declarations

(defun ess-looking-at-defun-p ()
  (or (looking-at "function[ \t]*(")
      (ess-looking-at-enclosed-defun-p)))

(defun ess-looking-at-enclosed-defun-p ()
  (save-excursion
    (and (ess-looking-at-call-p)
         (ess-jump-inside-call)
         (some (lambda (arg)
                 (string-match "^function\\b"
                               (cdr arg)))
               (ess-args-alist)))))


;;;*;;; Names / Objects / Expressions

;; Should  climb any names, including backquoted ones or those
;; containing `@' or `$'. Difficult to achieve with regexps, but
;; skipping chars is faster anyway.
(defun ess-climb-object ()
  (ess-save-excursion-when-nil
    (let (climbed)
      (ess-skip-blanks-backward)
      ;; Backquoted names can contain any character
      (if (and (memq (char-before) '(?` ?\" ?\'))
               (ess-backward-sexp))
          (setq climbed t)
        (while (some (apply-partially '/= 0)
                     `(,(skip-syntax-backward "w_")
                       ,(skip-chars-backward "\"'")))
          (setq climbed t)))
      ;; Recurse if we find an indexing char
      (when (memq (char-before) '(?$ ?@))
        (forward-char -1)
        (ess-climb-object))
      climbed)))

;; Todo: split name and object climbing
(defun ess-climb-name ()
  (ess-climb-object))

;; This jumps both object names and atomic objects like strings or
;; numbers.
(defun ess-jump-object ()
  (cond
   ;; Jump over object names
   ((ess-jump-name))
   ;; Jump over strings))
   ((ess-save-excursion-when-nil
      (skip-chars-forward " \t")
      (memq (char-after) '(?\" ?\')))
    (ess-forward-sexp))))

(defun ess-jump-name ()
  (ess-save-excursion-when-nil
    (let (climbed quote-char)
      (skip-chars-forward " \t")
      ;; Jump over backquoted names
      (cond ((and (eq (char-after) ?`)
                  (looking-back ess-R-symbol-pattern
                                (1- (point))))
             (forward-char)
             (setq climbed t))
            ((eq (char-after) ?`)
             (forward-char)
             (when (ess-while (not (memq (char-after) '(?` ?\C-J)))
                     (forward-char))
               (setq climbed t)
               (forward-char)))
            ;; Jump over regular names
            ((when (/= 0 (skip-syntax-forward "w_"))
               ;; Maybe point was inside backticks
               (when (eq (char-after) ?`)
                 (forward-char))
               (setq climbed t))))
      climbed)))

(defun ess-climb-expression (&optional ignore-ifelse)
  (ess-save-excursion-when-nil
    (or (ess-climb-block ignore-ifelse)
        (ess-climb-call)
        (ess-climb-object))))

(defun ess-jump-expression ()
  (or (ess-jump-block)
      (ess-jump-call)
      (ess-jump-object)))

(provide 'ess-r-syntax)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-r-syntax.el ends here
