
(defvar ess-ltest-R-chunk-pattern "^###[ \t]*\\([0-9]+[a-zA-Z]*\\) \\([^\n]*\\)$")
(defvar ess-ltest-R-code-start-pattern "^##!")
(defvar ess-ltest-R-code-cont-pattern "^##>")
(defvar ess-ltest-R-code-pattern "^##[!>]")
(defvar ess-ltest-R-section-pattern "^##### \\([^\n]*\\)$")
(defvar ess-ltest-R-mode-init '((mode . R)))

(defun ess-ltest-R (&optional file)
  (when file
    (let ((r-file (concat file ".R"))
          (el-file (concat file ".el")))
      (message "---Testing %s" (file-name-nondirectory r-file))
      (set-buffer (find-file-noselect r-file))
      (when (file-exists-p el-file)
        (load-file el-file))))
  ;; Don't check safety of local variables declared in test files
  (cl-letf (((symbol-function 'safe-local-variable-p) (lambda (sym val) t)))
    (let ((enable-dir-local-variables nil))
      (hack-local-variables)))
  (let ((ess-ltest-chunk-pattern ess-ltest-R-chunk-pattern)
        (ess-ltest-code-cont-pattern ess-ltest-R-code-cont-pattern)
        (ess-ltest-code-pattern ess-ltest-R-code-pattern)
        (ess-ltest-section-pattern ess-ltest-R-section-pattern)
        (ess-ltest-mode-init (append (assq-delete-all 'mode file-local-variables-alist)
                                     ess-ltest-R-mode-init)))
    (ess-ltest-this-buffer))
  (save-buffer))

(defun ess-ltest-this-buffer ()
  (goto-char 1)
  (let ((undo-inhibit-record-point t))
    (undo-boundary)
    ;; Print first section header
    (ess-ltest-print-section-header)
    (when (ess-ltest-search-chunk nil t)
      (while (looking-at ess-ltest-chunk-pattern)
        (ess-ltest-print-chunk-id)
        (ess-ltest-process-next-chunk)))
    (skip-chars-backward "\n")
    (let ((point-max (or (ess-ltest-local-variables-pos)
                         (point-max))))
      (delete-region (1+ (point)) point-max)
      (insert "\n"))
    (when (looking-at (concat "\n" comment-start "+ +Local Variables:"))
      (insert "\n"))
    (undo-boundary)))

(defun ess-ltest-local-variables-pos ()
  (save-excursion
    (let ((pattern (concat "^" comment-start "+ +Local Variables:")))
      (when (re-search-forward pattern nil t)
        (match-beginning 0)))))

(defun ess-ltest-print-section-header ()
  (save-excursion
    (skip-chars-forward " \n\t")
    (when (looking-at ess-ltest-section-pattern)
      (message (match-string-no-properties 1)))))

(defun ess-ltest-print-chunk-id ()
  (let ((number (concat "#" (match-string-no-properties 1)))
        (msg (match-string-no-properties 2)))
    (setq msg (substring msg 0 (string-match "-+$" msg)))
    (message (if (> (length msg) 0)
                 (concat number " - " msg)
               number))))

(defun ess-ltest-search-chunk (&optional n skip-section)
  (let* ((next-chunk (save-excursion
                       (cond ((re-search-forward ess-ltest-chunk-pattern
                                                 nil t (or n 1))
                              (match-beginning 0))
                             ((ess-ltest-local-variables-pos))
                             (t
                              (point-max)))))
         (next-section (save-excursion
                         (when (re-search-forward ess-ltest-section-pattern
                                                  next-chunk t)
                           (match-beginning 0)))))
    (goto-char (if (and (not skip-section) next-section)
                   next-section
                 next-chunk))))

(defun ess-ltest-process-next-chunk ()
  (let* ((chunk-beg (point))
         (chunk-end (progn
                      (forward-line)
                      (save-excursion
                        (ess-ltest-search-chunk)
                        (point-marker))))
         (test-case (progn
                      (skip-chars-forward " \t\n")
                      (ess-ltest-process-case)))
         (test-case-state test-case))
    (while (looking-at ess-ltest-code-pattern)
      (ess-ltest-process-next-subchunk chunk-end))
    (insert "\n")
    (ess-ltest-print-section-header)
    (when (looking-at ess-ltest-section-pattern)
      (insert "\n")
      (ess-ltest-search-chunk nil t))))

(defun ess-ltest-process-next-subchunk (chunk-end)
  (let* ((continuation (looking-at ess-ltest-code-cont-pattern))
         (test-code (ess-ltest-process-code))
         (test-result (ess-ltest- (if continuation test-case-state test-case)
                                  test-code ess-ltest-mode-init
                                  continuation))
         (subchunk-end (save-excursion
                         (if (re-search-forward ess-ltest-code-pattern chunk-end t)
                             (match-beginning 0)
                           chunk-end))))
    (setq test-case-state test-result)
    (delete-region (point) subchunk-end)
    (insert (concat "\n" test-result "\n\n"))))

(defun ess-ltest-process-case ()
  (let ((case-start (progn
                      (skip-chars-forward " \t\n")
                      (goto-char (line-beginning-position))
                      (point)))
        (code-start (if (re-search-forward ess-ltest-code-pattern chunk-end t)
                        (goto-char (match-beginning 0))
                      (error "No test code found")))
        (case-end (progn
                    (skip-chars-backward " \t\n")
                    (point))))
    (forward-char 1)
    (delete-region (point) code-start)
    (insert "\n")
    (buffer-substring-no-properties case-start case-end)))

(defun ess-ltest-process-code ()
  (let* ((test-start (point))
         (test-end (if (re-search-forward "^$" chunk-end t)
                       (1- (match-beginning 0))
                     (goto-char chunk-end)))
         (test-code (buffer-substring-no-properties test-start test-end)))
    ;; Remove comment prefix
    (while (string-match (concat (substring ess-ltest-code-pattern 1) "[ \t]?")
                         test-code)
      (setq test-code (replace-match "" t t test-code)))
    ;; Parse elisp
    (setq test-code (concat "(" test-code ")"))
    (car (read-from-string test-code))))


;; The following functions are borrowed from Lispy's testing
;; framework. The main difference is that they restore state when
;; `keep-state' is t. They also run `(kbd)' on strings.

(defvar ess-ltest--state-buffer nil
  "Evaluation buffer of previous test chunk.")

(defmacro ess-ltest (init &rest body)
  (apply 'ess-ltest- `(,init (,@body))))

(defun ess-ltest- (init body local-variables &optional keep-state)
  (unless keep-state
    (and ess-ltest--state-buffer
         (buffer-name ess-ltest--state-buffer)
         (kill-buffer ess-ltest--state-buffer))
    (setq ess-ltest--state-buffer (generate-new-buffer " *temp*")))
  (save-window-excursion
    (switch-to-buffer ess-ltest--state-buffer)
    (if keep-state
        (delete-region (point-min) (point-max))
      (transient-mark-mode 1)
      (setq-local file-local-variables-alist (copy-alist local-variables))
      (hack-local-variables-apply))
    (insert init)
    (goto-char (point-min))
    (when (search-forward "×" nil t)
      (backward-delete-char 1)
      (set-mark (point)))
    (goto-char (point-max))
    (search-backward "¶")
    (delete-char 1)
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
                       (ess-ltest-unalias (kbd x))))
                    ((and (listp x)
                          (eq (car x) 'kbd))
                     (ess-ltest-unalias x))
                    (t (eval x))))
            body)
    (insert "¶")
    (when (region-active-p)
      (exchange-point-and-mark)
      (insert "×"))
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun ess-ltest-decode-keysequence (str)
  "Decode STR from e.g. \"23ab5c\" to '(23 \"a\" \"b\" 5 \"c\")"
  (let ((table (copy-seq (syntax-table))))
    (loop for i from ?0 to ?9 do
         (modify-syntax-entry i "." table))
    (loop for i from ? to ? do
         (modify-syntax-entry i "w" table))
    (loop for i in '(? ?\( ?\) ?\[ ?\] ?{ ?} ?\" ?\' ?\ )
       do (modify-syntax-entry i "w" table))
    (cl-mapcan (lambda (x)
                 (let ((y (ignore-errors (read x))))
                   (if (numberp y)
                       (list y)
                     (mapcar #'string x))))
               (with-syntax-table table
                 (split-string str "\\b" t)))))

(defun ess-ltest-unalias (seq)
  "Emulate pressing keys decoded from SEQ."
  (if (vectorp seq)
      (ess-ltest--unalias-key seq)
    (let ((lkeys (ess-ltest-decode-keysequence seq))
          key)
      (while (setq key (pop lkeys))
        (if (numberp key)
            (let ((current-prefix-arg (list key)))
              (when lkeys
                (ess-ltest--unalias-key (pop lkeys))))
          (ess-ltest--unalias-key key))))))

(defun ess-ltest--unalias-key (key)
  "Call command that corresponds to KEY.
Insert KEY if there's no command."
  (let ((cmd (key-binding key)))
    (if (eq cmd 'self-insert-command)
        (insert key)
      (setq last-command-event (aref key 0))
      (call-interactively cmd)
      (setq last-command cmd))))
