;;; etest.el --- Emacs behavioural test framework  -*- lexical-binding: t; -*-






;;; Code:

(require 'ert)
(eval-when-compile
  (require 'cl-lib))

(defvar-local etest-local-inferior-buffer nil
  "External buffer containing output to check.
Use the `:inf-result' to flush this buffer and test its
contents.")

(cl-defmacro etest-deftest (name args &body body)
  (declare (doc-string 3)
           (indent 2))
  (let ((etest--docstring (when (stringp (car body))
                            (list (pop body)))))
    `(ert-deftest ,name ,args
       ,@etest--docstring
       (etest--run-test (quote ,body)
                        (lambda (actual expected)
                          (should (string= actual expected)))))))

(defmacro etest--pop-init (place)
  `(let (local)
     (while (eq (car ,place) :init)
       (pop ,place)
       (setq local (append local (pop ,place))))
     local))

(defmacro etest--with-test-buffer (init &rest body)
  (declare (indent 1))
  `(let ((etest--buf (etest--new-buffer ,init)))
     (unwind-protect
         (with-current-buffer etest--buf
           ,@body)
       (kill-buffer etest--buf))))

(defun etest--new-buffer (init)
  (let ((buf (generate-new-buffer " *elt-temp*")))
    (with-current-buffer buf
      (setq-local file-local-variables-alist (nreverse (copy-alist init)))
      (hack-local-variables-apply)
      (transient-mark-mode 1))
    buf))

(defun etest--run-test (body do-result)
  "Parse BODY as list of expressions.
`:test' arguments are evaluated in a dedicated buffer. The
keyword can be a command or a list of commands. Strings are
interpreted as `kbd' commands.

 The buffer is initialised with the list of local variables found
in `:init' keywords. The `:cleanup' keyword takes
unwind-protected expressions that are evaluated in LIFO order
after the test succeeds or fails.

`:result' keywords are processed with DO-RESULT. This should be a
function taking ACTUAL and EXPECTED strings.

`:inf-buffer' takes an auxiliary buffer whose contents can be
tested with `:inf-result'. The latter keyword work similarly to
`:result' but returns the current output in the inferior buffer.
This buffer is flushed. The inferior buffer is stored in the
buffer-local variable `etest-local-inferior-buffer'.

`:messages' keywords check the contents of the messages buffers
and are processed with DO-RESULT."
  (etest--with-test-buffer (etest--pop-init body)
    (let* ((inhibit-message t)
           (etest--msg-sentinel (etest--make-message-sentinel))
           etest--cleanup)
      (unwind-protect
          (while body
            (let ((etest--key (pop body))
                  (etest--value (pop body)))
              (pcase etest--key
                (`:inf-buffer (setq etest-local-inferior-buffer (eval etest--value)))
                (`:cleanup (push etest--value etest--cleanup))
                (`:case (progn
                          (erase-buffer)
                          (insert etest--value)))
                (`:test (etest-run (current-buffer) (etest--wrap-test etest--value)))
                (`:result (funcall do-result
                                   (etest--result (current-buffer))
                                   etest--value))
                (`:inf-result (etest--flush-inferior-buffer do-result etest--value))
                (`:messages (progn
                              (etest--flush-messages etest--msg-sentinel do-result etest--value)
                              (setq etest--msg-sentinel (etest--make-message-sentinel))))
                (_ (error (format "Expected an etest keyword, not `%s`" etest--key))))))
        (mapc #'eval etest--cleanup)))))

(defun etest--wrap-test (x)
  (if (or (not (listp x))
          (symbolp (car x)))
      (list x)
    x))

(defun etest--flush-inferior-buffer (do-result value)
  (unless etest-local-inferior-buffer
    (error "Must set `etest-local-inferior-buffer'"))
  (let ((result (funcall do-result
                         (etest--result etest-local-inferior-buffer t)
                         value)))
    (with-current-buffer etest-local-inferior-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    result))

(defun etest--make-message-sentinel ()
  (let ((sentinel (format "etest-messages-%s" (gensym))))
    (message sentinel)
    sentinel))

(defun etest--flush-messages (msg-sentinel do-result value)
  (let ((msgs (with-current-buffer (get-buffer "*Messages*")
                (save-excursion
                  (goto-char (point-min))
                  (re-search-forward msg-sentinel nil t)
                  (let ((start (1+ (point))))
                    (buffer-substring start (max start (1- (point-max)))))))))
    (funcall do-result msgs value)))

;;;###autoload
(defun etest-update ()
  "Update all result keywords for the etest block at point."
  (interactive)
  (save-window-excursion
    (save-excursion
      (let* ((beg (etest--climb-deftest))
             (end (progn (forward-sexp) (point-marker)))
             (str (buffer-substring-no-properties beg end))
             (body (car (read-from-string str)))
             results)
        ;; Skip `etest-deftest` and initial arguments
        (dotimes (i 3)
          (pop body))
        (when (stringp (car body))
          (pop body))
        (let ((results (etest--read-results body)))
          (goto-char beg)
          (while results
            (unless (re-search-forward "^ *:\\(\\(inf-\\)?result\\|messages\\) *\s\"" end t)
              (error "Can't find any result keyword"))
            (let ((result-beg (1- (point)))
                  (result-end (progn (backward-up-list -1 t) (point)))
                  (result-str (prin1-to-string (pop results))))
              (goto-char result-beg)
              (delete-region result-beg result-end)
              (insert result-str))))))))

(defun etest--read-results (body)
  (let (results)
    (etest--run-test body (lambda (actual expected)
                            (push actual results)))
    (nreverse results)))

(defun etest--climb-deftest ()
  ;; Climb one character when point is in front of a parenthesis.
  ;; This puts the cursor inside the `etest-deftest` when it is in
  ;; front.
  (unless (looking-at "(")
    (backward-char 1)
    (unless (looking-at ")")
      (forward-char 1)))
  ;; Climb enclosing lists until we find the `test-deftest`
  (while (and (not (looking-at "(etest-deftest"))
              (ignore-errors (prog1 t (backward-up-list nil t)))))
  (point))


;;; Run commands with cursor and mark tracking

;; The following code is adapted from the `lispy-with' macro by Oleh
;; Krehel in <https://github.com/abo-abo/lispy/blob/master/lispy-test.el>.
;; The main difference is support for multiple cursors.

(defun etest-run (buf cmds &optional reset-state)
  "Run CMDS in BUF.
If RESET-STATE is non-nil, `last-command' and
`current-prefix-arg' are set to nil for all cursors."
  (with-current-buffer buf
    (goto-char (point-min))
    (when (search-forward "×" nil t)
      (backward-delete-char 1)
      (set-mark (point))
      (when (search-forward "×" nil t)
        (error "There can only be one mark cursor")))
    (goto-char (point-max))
    (let (cursors-start
          cursors-end)
      (while (search-backward "¶" nil t)
        (delete-char 1)
        (let ((marker (point-marker)))
          (set-marker-insertion-type marker t)
          (push marker cursors-start)))
      (unless cursors-start
        (setq cursors-start (list (point-min))))
      ;; Fontification must take place after removing "¶"
      ;; FIXME Emacs 25: Use `font-lock-ensure'
      (font-lock-default-fontify-buffer)
      (dolist (cursor cursors-start)
        (goto-char cursor)
        ;; Reset Emacs state for each cursor
        (when reset-state
          (setq last-command nil)
          (setq current-prefix-arg nil))
        (mapcar (lambda (x)
                  (cond ((equal x '(kbd "C-u"))
                         (setq current-prefix-arg (list 4)))
                        ((stringp x)
                         (if (string= x "C-u")
                             (setq current-prefix-arg (list 4))
                           (etest--unalias (kbd x))))
                        ((and (listp x)
                              (eq (car x) 'kbd))
                         (etest--unalias x))
                        (t (eval x))))
                cmds)
        (let ((marker (point-marker)))
          (set-marker-insertion-type marker t)
          (push marker cursors-end)))
      (dolist (cursor cursors-end)
        (goto-char cursor)
        (insert "¶")))
    (when (region-active-p)
      (exchange-point-and-mark)
      (insert "×"))
    t))

(defun etest--result (buf &optional trim-last-newline)
  (with-current-buffer buf
    (let ((beg (point-min))
          (end (point-max)))
      (when (and trim-last-newline
                 (> end beg)
                 (string= (buffer-substring (1- end) end) "\n"))
        (setq end (1- end)))
      (buffer-substring-no-properties beg end))))

(defun etest--unalias (seq)
  "Emulate pressing keys decoded from SEQ."
  (if (vectorp seq)
      (etest--unalias-key seq (key-binding seq))
    (let ((lkeys (etest--decode-keysequence seq))
          (current-prefix-arg current-prefix-arg)
          key)
      (while (setq key (pop lkeys))
        (when (numberp key)
          (setq current-prefix-arg (list key))
          (setq key (pop lkeys)))
        (let ((cmd (key-binding key)))
          (while (keymapp cmd)
            (setq key (pop lkeys))
            (setq cmd (or (lookup-key cmd key)
                          (error "Can't find binding in keymap"))))
          (etest--unalias-key key cmd))))))

(defun etest--unalias-key (key cmd)
  "Call command that corresponds to KEY.
Insert KEY if there's no command."
  (setq last-input-event (aref key 0))
  (if (eq cmd 'self-insert-command)
      (insert key)
    (setq last-command-event (aref key 0))
    (call-interactively cmd)
    (setq last-command cmd)))

(defun etest--decode-keysequence (str)
  "Decode STR from e.g. \"23ab5c\" to '(23 \"a\" \"b\" 5 \"c\")"
  (let ((table (copy-sequence (syntax-table))))
    (cl-loop for i from ?0 to ?9 do
             (modify-syntax-entry i "." table))
    (cl-loop for i from ? to ? do
             (modify-syntax-entry i "w" table))
    (cl-loop for i in '(? ?\( ?\) ?\[ ?\] ?{ ?} ?\" ?\' ?\ )
             do (modify-syntax-entry i "w" table))
    (cl-mapcan (lambda (x)
                 (let ((y (ignore-errors (read x))))
                   (if (numberp y)
                       (list y)
                     (mapcar #'string x))))
               (with-syntax-table table
                 (split-string str "\\b" t)))))

(provide 'etest)

;;; etest.el ends here
