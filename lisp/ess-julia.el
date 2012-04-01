;
; Emacs mode for Julia
;

(defvar julia-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.j\\'\\|\\.jl\\'" . julia-mode))

(defvar julia-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)   ; underscores in words
    (modify-syntax-entry ?@ "w" table)
    (modify-syntax-entry ?# "<" table)   ; #  single-line comment start
    (modify-syntax-entry ?\n ">" table)  ; \n single-line comment end
    (modify-syntax-entry ?\{ "(} " table)
    (modify-syntax-entry ?\} "){ " table)
    (modify-syntax-entry ?\[ "(] " table)
    (modify-syntax-entry ?\] ")[ " table)
    (modify-syntax-entry ?\( "() " table)
    (modify-syntax-entry ?\) ")( " table)
    ;(modify-syntax-entry ?\\ "." table)  ; \ is an operator outside quotes
    (modify-syntax-entry ?'  "." table)  ; character quote or transpose
    ;(modify-syntax-entry ?\" "." table)
    (modify-syntax-entry ?? "." table)
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    table)
  "Syntax table for julia-mode")

;; syntax table that holds within strings
(defvar julia-mode-string-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for julia-mode")

;; disable " inside char quote
(defvar julia-mode-char-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table)
  "Syntax table for julia-mode")

(defconst julia-string-regex
  "\"[^\"]*?\\(\\(\\\\\\\\\\)*\\\\\"[^\"]*?\\)*\"")

(defconst julia-char-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|$!\\^~\\\\;:]\\|^\\)\\('\\(\\([^']*?[^\\\\]\\)\\|\\(\\\\\\\\\\)\\)'\\)")

(defconst julia-unquote-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|!\\^~\\\\;:]\\|^\\)\\($[a-zA-Z0-9_]+\\)")

(defconst julia-forloop-in-regex
  "for +[^ 	]+ +.*\\(in\\)\\(\\s-\\|$\\)+")

(defconst julia-font-lock-keywords
  (list '("\\<\\(\\|Uint\\(8\\|16\\|32\\|64\\)\\|Int\\(8\\|16\\|32\\|64\\)\\|Integer\\|Float\\|Float32\\|Float64\\|Complex128\\|Complex64\\|ComplexNum\\|Bool\\|Char\\|Number\\|Scalar\\|Real\\|Int\\|Uint\\|Array\\|DArray\\|AbstractArray\\|AbstractVector\\|AbstractMatrix\\|SubArray\\|StridedArray\\|StridedVector\\|StridedMatrix\\|VecOrMat\\|StridedVecOrMat\\|Range\\|Range1\\|SparseMatrixCSC\\|Tuple\\|NTuple\\|Buffer\\|Size\\|Index\\|Symbol\\|Function\\|Vector\\|Matrix\\|Union\\|Type\\|Any\\|Complex\\|None\\|String\\|Ptr\\|Void\\|Exception\\|PtrInt\\|Long\\|Ulong\\)\\>" .
      font-lock-type-face)
    (cons
     (concat "\\<\\("
         (mapconcat
          'identity
          '("if" "else" "elseif" "while" "for" "begin" "end" "quote"
            "try" "catch" "return" "local" "abstract" "function" "macro" "ccall"
	    "typealias" "break" "continue" "type" "global" "@\\w+"
	    "module" "import" "export" "const" "let" "bitstype")
          "\\|") "\\)\\>")
     'font-lock-keyword-face)
    '("\\<\\(true\\|false\\|C_NULL\\|Inf\\|NaN\\|Inf32\\|NaN32\\)\\>" . font-lock-constant-face)
    (list julia-unquote-regex 2 'font-lock-constant-face)
    (list julia-char-regex 2 'font-lock-string-face)
    (list julia-forloop-in-regex 1 'font-lock-keyword-face)
    ;(list julia-string-regex 0 'font-lock-string-face)
))

(defconst julia-block-start-keywords
  (list "if" "while" "for" "begin" "try" "function" "type" "let" "macro"
	"quote"))

(defconst julia-block-other-keywords
  (list "else" "elseif"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch"))

(defun member (item lst)
  (if (null lst)
      nil
    (or (equal item (car lst))
	(member item (cdr lst)))))

; TODO: skip keywords and # characters inside strings

(defun in-comment ()
  (member ?# (string-to-list (buffer-substring (line-beginning-position)
					       (point)))))

(defun strcount (str chr)
  (let ((i 0)
	(c 0))
    (while (< i (length str))
      (if (equal (elt str i) chr)
	  (setq c (+ c 1)))
      (setq i (+ i 1)))
    c))

(defun in-brackets ()
  (let ((before (buffer-substring (line-beginning-position) (point))))
    (> (strcount before ?[)
       (strcount before ?]))))

(defun at-keyword (kw-list)
  ; not a keyword if used as a field name, X.word, or quoted, :word
  (and (or (= (point) 1)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (in-comment))
       (not (in-brackets))
       (member (current-word) kw-list)))

; get the position of the last open block
(defun last-open-block-pos (min)
  (let ((count 0))
    (while (not (or (> count 0) (<= (point) min)))
      (backward-word 1)
      (setq count
	    (cond ((at-keyword julia-block-start-keywords)
		   (+ count 1))
		  ((and (equal (current-word) "end")
			(not (in-comment)) (not (in-brackets)))
		   (- count 1))
		  (t count))))
    (if (> count 0)
	(point)
      nil)))

; get indent for last open block
(defun last-open-block (min)
  (let ((pos (last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ julia-basic-offset (current-indentation))))))

; return indent implied by a special form opening on the previous line, if any
(defun form-indent ()
  (forward-line -1)
  (end-of-line)
  (backward-sexp)
  (if (at-keyword julia-block-other-keywords)
      (+ julia-basic-offset (current-indentation))
    (if (char-equal (char-after (point)) ?\()
        (progn
          (backward-word 1)
          (let ((cur (current-indentation)))
            (if (at-keyword julia-block-start-keywords)
                (+ julia-basic-offset cur)
              nil)))
      nil)))

;(defun far-back ()
;  (max (point-min) (- (point) 2000)))

(defmacro error2nil (body) `(condition-case nil ,body (error nil)))

(defun paren-indent ()
  (let* ((p (parse-partial-sexp (save-excursion
				  ;; only indent by paren if the last open
				  ;; paren is closer than the last open
				  ;; block
				  (or (last-open-block-pos (point-min))
				      (point-min)))
				(progn (beginning-of-line)
				       (point))))
         (pos (cadr p)))
    (if (or (= 0 (car p)) (null pos))
        nil
      (progn (goto-char pos) (+ 1 (current-column))))))
;  (forward-line -1)
;  (end-of-line)
;  (let ((pos (condition-case nil
;                (scan-lists (point) -1 1)
;              (error nil))))
;   (if pos
;       (progn (goto-char pos) (+ 1 (current-column)))
;     nil)))

(defun julia-indent-line ()
  "Indent current line of julia code"
  (interactive)
;  (save-excursion
    (end-of-line)
    (indent-line-to
     (or (save-excursion (error2nil (form-indent)))
         (save-excursion (error2nil (paren-indent)))
         (save-excursion
           (let ((endtok (progn
                           (beginning-of-line)
                           (forward-to-indentation 0)
                           (at-keyword julia-block-end-keywords))))
             (error2nil (+ (last-open-block (point-min))
                           (if endtok (- julia-basic-offset) 0)))))
	 ;; previous line ends in =
	 (save-excursion
	   (if (and (not (equal (point-min) (line-beginning-position)))
		    (progn
		      (forward-line -1)
		      (end-of-line) (backward-char 1)
		      (equal (char-after (point)) ?=)))
	       (+ julia-basic-offset (current-indentation))
	     nil))
	 ;; take same indentation as previous line
	 (save-excursion (forward-line -1)
			 (current-indentation))
         0))
    (when (at-keyword julia-block-end-keywords)
      (forward-word 1)))

;; (defun julia-mode ()
;;   "Major mode for editing julia code"
;;   (interactive)
;;   (kill-all-local-variables)
;;   (set-syntax-table julia-mode-syntax-table)
;;   (set (make-local-variable 'comment-start) "# ")
;;   (set (make-local-variable 'comment-start-skip) "#+\\s-*")
;;   (set (make-local-variable 'font-lock-defaults) '(julia-font-lock-keywords))
;; ;  (set (make-local-variable 'font-lock-syntactic-keywords)
;; ;      (list
;; ;       (list "\\(\\\\\\)\\s-*\".*?\"" 1 julia-mode-char-syntax-table)))
;;   (set (make-local-variable 'font-lock-syntactic-keywords)
;;        (list
;; 	(list julia-char-regex 2
;; 	      julia-mode-char-syntax-table)
;; ;        (list julia-string-regex 0
;; ;              julia-mode-string-syntax-table)
;; ))
;;   (set (make-local-variable 'indent-line-function) 'julia-indent-line)
;;   (set (make-local-variable 'julia-basic-offset) 4)
;;   (setq indent-tabs-mode nil)
;;   (setq major-mode 'julia-mode)
;;   (setq mode-name "julia")
;;   (run-hooks 'julia-mode-hook))

;;;### autoload
(defun julia-mode  (&optional proc-name)
  "Major mode for editing R source.  See `ess-mode' for more help."
  (interactive "P")
  ;; (setq ess-customize-alist julia-customize-alist)
  ;;(setq imenu-generic-expression R-imenu-generic-expression)
  (ess-mode julia-customize-alist proc-name)
  ;; for emacs < 24
  ;; (add-hook 'comint-dynamic-complete-functions 'ess-complete-object-name nil 'local)
  ;; for emacs >= 24
  ;; (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  ;; (add-hook 'completion-at-point-functions 'ess-object-completion nil 'local)
  ;; (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (set (make-local-variable 'end-of-defun-function)
       'ess-end-of-function)
  (ess-imenu-julia)
  (run-hooks 'julia-mode-hook))

(defvar julia-editing-alist
  '((paragraph-start		  . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-separate		  . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline	  . t)
    (comment-start		  . "# ")
    (comment-add                  . 1)
    (comment-start-skip		  . "#+\\s-*")
    (comment-column		  . 40)
    ;;(comment-indent-function	. 'S-comment-indent)
    ;;(ess-comment-indent	    . 'S-comment-indent)
    ;; (ess-indent-line			    . 'S-indent-line)
    ;;(ess-calculate-indent	      . 'ess-calculate-indent)
    (indent-line-function	  . 'julia-indent-line)
    (parse-sexp-ignore-comments	  . t)
    (ess-style		  	  . ess-default-style) ;; ignored
    (ess-local-process-name	  . nil)
    ;;(ess-keep-dump-files	    . 'ask)
    (ess-mode-syntax-table	  . julia-syntax-table)
    ;; For Changelog add, require ' ' before <- : "attr<-" is a function name :
    ;; (add-log-current-defun-header-regexp . "^\\(.+\\)\\s-+=[ \t\n]*function")
    (add-log-current-defun-header-regexp . "^.*function[ \t]*\\([^ \t(]*\\)[ \t]*(")
    (font-lock-defaults		  . '(ess-R-mode-font-lock-keywords
				      nil nil ((?\. . "w") (?\_ . "w"))))
    )
  "General options for R source files.")

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(defvar inferior-julia-program-name nil
  "Path to julia-release-basic executable")

(defvar julia-customize-alist
  '((comint-use-prompt-regexp		. t)
    (inferior-ess-primary-prompt	. "> ")
    (inferior-ess-secondary-prompt	. "+ ")
    (inferior-ess-prompt	        . inferior-ess-S-prompt)
    (ess-local-customize-alist		. 'julia-customize-alist)
    (inferior-ess-program		. inferior-julia-program-name)
    (inferior-ess-font-lock-keywords	. julia-font-lock-keywords)
    (inferior-ess-load-command		. "load(\"%s\")\n")
    ;; (inferior-ess-objects-command	. inferior-R-objects-command)
    ;; (inferior-ess-search-list-command	. "search()\n")
    (inferior-ess-help-command		. "help(%s)\n")
    (ess-language			. "julia")
    (ess-dialect			. "julia")
    (ess-suffix				. "jl")
    (ess-dump-filename-template		. (ess-replace-regexp-in-string
					   "S$" ess-suffix ; in the one from custom:
					   ess-dump-filename-template-proto))
    (ess-mode-syntax-table		. julia-syntax-table)
    (ess-mode-editing-alist	        . julia-editing-alist)
    (ess-change-sp-regexp		. nil );ess-R-change-sp-regexp)
    (ess-help-sec-regex			. ess-help-R-sec-regex)
    (ess-help-sec-keys-alist		. ess-help-R-sec-keys-alist)
    (ess-loop-timeout			. ess-S-loop-timeout);fixme: dialect spec.
    (ess-cmd-delay			. ess-R-cmd-delay)
    (ess-function-pattern		. ess-R-function-pattern)
    (ess-object-name-db-file		. "ess-r-namedb.el" )
    (ess-imenu-mode-function		. 'ess-imenu-R)
    (ess-smart-operators		. ess-R-smart-operators)
    (inferior-ess-help-filetype        . nil)
    (inferior-ess-exit-command		. "exit()\n")
    ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
    (inferior-ess-start-file		. nil) ;; "~/.ess-R"
    (inferior-ess-start-args		. "")
    (inferior-ess-language-start	. nil)
    (ess-STERM		. "iESS")
    (ess-editor	. R-editor)
    (ess-pager		. R-pager)
    )
  "Variables to customize for Julia -- set up later than emacs initialization.")


(defvar ess-julia-versions '("julia")
  "List of partial strings for versions of Julia to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of Julia is made available.  ")

(defcustom inferior-julia-args ""
  "String of arguments (see 'R --help') used when starting R.
These arguments are currently not passed to other versions of R that have
been created using the variable `ess-r-versions'."
  :group 'ess-julia
  :type 'string)

;;;### autoload
(defun julia (&optional start-args)
  "Call 'julia',
Optional prefix (C-u) allows to set command line arguments, such as
--vsize.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to R, put them in the variable `inferior-julia-args'."
  (interactive "P")
  ;; get settings, notably inferior-R-program-name :
  (setq ess-customize-alist julia-customize-alist)
  (ess-write-to-dribble-buffer   ;; for debugging only
   (format
    "\n(julia): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
    ess-dialect (current-buffer) start-args current-prefix-arg))
  (let* ((r-start-args
	   (concat inferior-julia-args " " ; add space just in case
		   (if start-args
		       (read-string
			(concat "Starting Args [other than `"
				inferior-julia-args
				"'] ? "))
		     nil))))

    (inferior-ess r-start-args) ;; -> .. (ess-multi ...) -> .. (inferior-ess-mode) ..
    ;;   (set (make-local-variable 'font-lock-syntactic-keywords)
    ;;        (list
    ;; 	(list julia-char-regex 2
    ;; 	      julia-mode-char-syntax-table)
    ;; ;        (list julia-string-regex 0
    ;; ;              julia-mode-string-syntax-table)
    ;; ))
    (set (make-local-variable 'indent-line-function) 'julia-indent-line)
    (set (make-local-variable 'julia-basic-offset) 4)
    (setq indent-tabs-mode nil)
    ;; (if inferior-ess-language-start
    ;; 	(ess-eval-linewise inferior-ess-language-start
    ;; 			   nil nil nil 'wait-prompt)))
    ))

(defvar julia-imenu-generic-expression
  '(("Functions" "^\\s-*function\\s-*\\([^ \t\n(]*\\)(" 1)
    ;; ("Classes" "^.*setClass(\\(.*\\)," 1)
    ;; ("Coercions" "^.*setAs(\\([^,]+,[^,]*\\)," 1) ; show from and to
    ;; ("Generics" "^.*setGeneric(\\([^,]*\\)," 1)
    ;; ("Methods" "^.*set\\(Group\\|Replace\\)?Method(\"\\(.+\\)\"," 2)
    ;; ;;[ ]*\\(signature=\\)?(\\(.*,?\\)*\\)," 1)
    ;; ;;
    ;; ;;("Other" "^\\(.+\\)\\s-*<-[ \t\n]*[^\\(function\\|read\\|.*data\.frame\\)]" 1)
    ;; ("Package" "^.*\\(library\\|require\\)(\\(.*\\)," 2)
    ;; ("Data" "^\\(.+\\)\\s-*<-[ \t\n]*\\(read\\|.*data\.frame\\).*(" 1)))
    ))

(defun ess-imenu-julia (&optional arg)
  "Julia Language Imenu support for ESS."
  (interactive)
  (setq imenu-generic-expression julia-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu-jl"))

;; (fset 'ess-imenu-R 'ess-imenu-S)


(setq inferior-julia-program-name "~/VC/julia/julia-release-basic")

(provide 'ess-julia)
