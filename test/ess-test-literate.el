;;; ess-test-literate.el --- ESS literate tests


;;; Commentary:
;; Literate tests


;;; Code:

(require 'ess-r-mode)

(eval-when-compile
  (require 'cl-lib))
(require 'ert)

(defmacro elt-deftest (name args file)
  `(ert-deftest ,name ,args
     (let ((inhibit-message ess-inhibit-message-in-tests)
           (path (expand-file-name ,file "literate")))
       (elt-do 'test path))))

(defun elt--activate-font-lock-keywords ()
  "Activate font-lock keywords for some of ELT's symbols."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<elt-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
(add-hook 'emacs-lisp-mode-hook #'elt--activate-font-lock-keywords)

(defvar elt-ess-r-chunk-pattern "^###[ \t]*\\([0-9]+[a-zA-Z]*\\) \\([^\n]*\\)$")
(defvar elt-ess-r-code-start-pattern "^##!")
(defvar elt-ess-r-code-cont-pattern "^##>")
(defvar elt-ess-r-code-pattern "^##[!>]")
(defvar elt-ess-r-section-pattern "^##### \\([^\n]*\\)$")
(defvar elt-ess-r-mode-init '((mode . R)))

(defun elt-do (action file)
  (unless (memq action '(test regenerate))
    (error "Invalid literate test action"))
  (let ((verb (if (eq action 'test)
                  "Testing"
                "Regenerating")))
    (message "---%s %s" verb (file-name-nondirectory file)))
  (let* ((src-buffer (if (file-exists-p file)
                         (find-file-noselect file)
                       (error "Can't find literate test file")))
         (src-string (with-current-buffer src-buffer
                       (buffer-string)))
         (output (elt-buffer-string file src-string)))
    (pcase action
      (`test (should (string= src-string output)))
      (`regenerate (with-current-buffer src-buffer
                     (erase-buffer)
                     (insert output)
                     (save-buffer))))))

(defun elt-buffer-string (file src-string)
  (let ((el-file (concat (file-name-sans-extension file) ".el")))
    (when (file-exists-p el-file)
      (load-file el-file)))
  (with-temp-buffer
    (insert src-string)
    ;; Don't check safety of local variables declared in test files
    (cl-letf (((symbol-function 'safe-local-variable-p) (lambda (sym val) t)))
      (let ((enable-dir-local-variables nil))
        (hack-local-variables)))
    (let ((elt-chunk-pattern elt-ess-r-chunk-pattern)
          (elt-code-cont-pattern elt-ess-r-code-cont-pattern)
          (elt-code-pattern elt-ess-r-code-pattern)
          (elt-section-pattern elt-ess-r-section-pattern)
          (elt-mode-init (append (assq-delete-all 'mode file-local-variables-alist)
                                 elt-ess-r-mode-init)))
      (elt-this-buffer)
      (buffer-string))))

(defun elt-this-buffer ()
  (goto-char 1)
  (let ((undo-inhibit-record-point t))
    (undo-boundary)
    ;; Print first section header
    (elt-print-section-header)
    (when (elt-search-chunk nil t)
      (while (looking-at elt-chunk-pattern)
        (elt-print-chunk-id)
        (elt-process-next-chunk)))
    (skip-chars-backward "\n")
    (let ((point-max (or (elt-local-variables-pos)
                         (point-max))))
      (delete-region (1+ (point)) point-max)
      (insert "\n"))
    (when (looking-at (concat "\n" comment-start "+ +Local Variables:"))
      (insert "\n"))
    (undo-boundary)))

(defun elt-local-variables-pos ()
  (save-excursion
    (let ((pattern (concat "^" comment-start "+ +Local Variables:")))
      (when (re-search-forward pattern nil t)
        (match-beginning 0)))))

(defun elt-print-section-header ()
  (save-excursion
    (skip-chars-forward " \n\t")
    (when (looking-at elt-section-pattern)
      (message (match-string-no-properties 1)))))

(defun elt-print-chunk-id ()
  (let ((number (concat "#" (match-string-no-properties 1)))
        (msg (match-string-no-properties 2)))
    (setq msg (substring msg 0 (string-match "-+$" msg)))
    (message (if (> (length msg) 0)
                 (concat number " - " msg)
               number))))

(defun elt-search-chunk (&optional n skip-section)
  (let* ((next-chunk (save-excursion
                       (cond ((re-search-forward elt-chunk-pattern
                                                 nil t (or n 1))
                              (match-beginning 0))
                             ((elt-local-variables-pos))
                             (t
                              (point-max)))))
         (next-section (save-excursion
                         (when (re-search-forward elt-section-pattern
                                                  next-chunk t)
                           (match-beginning 0)))))
    (goto-char (if (and (not skip-section) next-section)
                   next-section
                 next-chunk))))

(defun elt-process-next-chunk ()
  (let* ((chunk-beg (point))
         (chunk-end (progn
                      (forward-line)
                      (save-excursion
                        (elt-search-chunk)
                        (point-marker))))
         (orig-chunk (buffer-substring chunk-beg chunk-end)))
    (condition-case cnd
        (let* ((test-case (progn
                            (skip-chars-forward " \t\n")
                            (elt-process-case)))
               (test-case-state test-case))
          (while (looking-at elt-code-pattern)
            (elt-process-next-subchunk chunk-end))
          (insert "\n")
          (elt-print-section-header)
          (when (looking-at elt-section-pattern)
            (insert "\n")
            (elt-search-chunk nil t)))
      (ert-test-skipped
        (message (concat "  Skipping test: " (cadr cnd)))
        (goto-char chunk-beg)
        (delete-region chunk-beg chunk-end)
        (insert orig-chunk)
        nil))))

(defun elt-process-next-subchunk (chunk-end)
  (let* ((continuation (looking-at elt-code-cont-pattern))
         (test-code (elt-process-code))
         (test-result (elt-run- (if continuation test-case-state test-case)
                            test-code elt-mode-init
                            continuation))
         (subchunk-end (save-excursion
                         (if (re-search-forward elt-code-pattern chunk-end t)
                             (match-beginning 0)
                           chunk-end))))
    (setq test-case-state test-result)
    (delete-region (point) subchunk-end)
    (insert (concat "\n" test-result "\n\n"))))

(defun elt-process-case ()
  (let ((case-start (progn
                      (skip-chars-forward " \t\n")
                      (goto-char (line-beginning-position))
                      (point)))
        (code-start (if (re-search-forward elt-code-pattern chunk-end t)
                        (goto-char (match-beginning 0))
                      (error "No test code found")))
        (case-end (progn
                    (skip-chars-backward " \t\n")
                    (point))))
    (forward-char 1)
    (delete-region (point) code-start)
    (insert "\n")
    (buffer-substring-no-properties case-start case-end)))

(defun elt-process-code ()
  (let* ((test-start (point))
         (test-end (if (re-search-forward "^$" chunk-end t)
                       (1- (match-beginning 0))
                     (goto-char chunk-end)))
         (test-code (buffer-substring-no-properties test-start test-end)))
    ;; Remove comment prefix
    (while (string-match (concat (substring elt-code-pattern 1) "[ \t]?")
                         test-code)
      (setq test-code (replace-match "" t t test-code)))
    ;; Parse elisp
    (setq test-code (concat "(" test-code ")"))
    (car (read-from-string test-code))))


;; The following functions are borrowed from Lispy's testing
;; framework. The main difference is that they restore state when
;; `keep-state' is t. They also run `(kbd)' on strings.

(defvar elt--state-buffer nil
  "Evaluation buffer of previous test chunk.")

(defmacro elt-run (init &rest body)
  (apply 'elt-run- `(,init (,@body))))

(defun elt-run- (init body local-variables &optional keep-state)
  (unless keep-state
    (and elt--state-buffer
         (buffer-name elt--state-buffer)
         (kill-buffer elt--state-buffer))
    (setq elt--state-buffer (generate-new-buffer " *temp*")))
  (save-window-excursion
    (switch-to-buffer elt--state-buffer)
    (if keep-state
        (delete-region (point-min) (point-max))
      (transient-mark-mode 1)
      (setq-local file-local-variables-alist (copy-alist local-variables))
      (hack-local-variables-apply))
    (insert init)
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
        (error "There must be at least one point cursor"))
      ;; Fontification must take place after removing "¶"
      ;; FIXME Emacs 25: Use `font-lock-ensure'
      (font-lock-default-fontify-buffer)
      (dolist (cursor cursors-start)
        (goto-char cursor)
        ;; Reset Emacs state
        (unless keep-state
          (setq last-command nil)
          (setq current-prefix-arg nil))
        (mapcar (lambda (x)
                  (cond ((equal x '(kbd "C-u"))
                         (setq current-prefix-arg (list 4)))
                        ((stringp x)
                         (if (string= x "C-u")
                             (setq current-prefix-arg (list 4))
                           (elt-unalias (kbd x))))
                        ((and (listp x)
                              (eq (car x) 'kbd))
                         (elt-unalias x))
                        (t (eval x))))
                body)
        (let ((marker (point-marker)))
          (set-marker-insertion-type marker t)
          (push marker cursors-end)))
      (dolist (cursor cursors-end)
        (goto-char cursor)
        (insert "¶")))
    (when (region-active-p)
      (exchange-point-and-mark)
      (insert "×"))
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun elt-decode-keysequence (str)
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

(defun elt-unalias (seq)
  "Emulate pressing keys decoded from SEQ."
  (if (vectorp seq)
      (elt--unalias-key seq)
    (let ((lkeys (elt-decode-keysequence seq))
          key)
      (while (setq key (pop lkeys))
        (if (numberp key)
            (let ((current-prefix-arg (list key)))
              (when lkeys
                (elt--unalias-key (pop lkeys))))
          (elt--unalias-key key))))))

(defun elt--unalias-key (key)
  "Call command that corresponds to KEY.
Insert KEY if there's no command."
  (setq last-input-event (aref key 0))
  (let ((cmd (key-binding key)))
    (if (eq cmd 'self-insert-command)
        (insert key)
      (setq last-command-event (aref key 0))
      (call-interactively cmd)
      (setq last-command cmd))))

(provide 'ess-test-literate)

;;; ess-test-literate.el ends here
