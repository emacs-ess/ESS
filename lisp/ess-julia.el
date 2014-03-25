;; ess-julia.el --- ESS julia mode and inferior interaction
;;
;; Copyright (C) 2012 Vitalie Spinu.
;;
;; Filename: ess-julia.el
;; Author: Vitalie Spinu (based on julia-mode.el from julia-lang project)
;; Maintainer: Vitalie Spinu
;; Created: 02-04-2012 (ESS 12.03)
;; Keywords: ESS, julia
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;; This file is part of ESS
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; http://www.r-project.org/Licenses/
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  customise inferior-julia-program-name to point to your julia-basic
;;  and start the inferior with M-x julia.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'compile); for compilation-* below
(require 'ess-utils)
(eval-when-compile
  (require 'cl)); in Emacs <= 23.x for (mapcan .)

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(defvar julia-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "_" table)   ; underscores in words
    (modify-syntax-entry ?@ "_" table)
    (modify-syntax-entry ?. "_" table)
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
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?` "\"" table)
    ;; (modify-syntax-entry ?\" "." table)
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
  "Syntax table for `julia-mode'.")

;; syntax table that holds within strings
(defvar julia-mode-string-syntax-table
  (let ((table (make-syntax-table)))
    table)
  "Syntax table for `julia-mode'.")

;; disable " inside char quote
(defvar julia-mode-char-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table)
  "Syntax table for `julia-mode'.")

(defconst julia-char-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|$!\\^~\\\\;:]\\|^\\)\\('\\(\\([^']*?[^\\\\]\\)\\|\\(\\\\\\\\\\)\\)'\\)")

(defconst julia-unquote-regex
  "\\(\\s(\\|\\s-\\|-\\|[,%=<>\\+*/?&|!\\^~\\\\;:]\\|^\\)\\($[a-zA-Z0-9_]+\\)")

(defconst julia-forloop-in-regex
  "for +[^ 	]+ +.*\\(in\\)\\(\\s-\\|$\\)+")

(defconst julia-font-lock-keywords
  (list '("\\<\\(\\|Uint\\(8\\|16\\|32\\|64\\|128\\)\\|Int\\(8\\|16\\|32\\|64\\|128\\)\\|BigInt\\|Integer\\|BigFloat\\|FloatingPoint\\|Float16\\|Float32\\|Float64\\|Complex128\\|Complex64\\|ComplexPair\\|Bool\\|Char\\|Number\\|Real\\|Int\\|Uint\\|Array\\|DArray\\|AbstractArray\\|AbstractVector\\|AbstractMatrix\\|AbstractSparseMatrix\\|SubArray\\|StridedArray\\|StridedVector\\|StridedMatrix\\|VecOrMat\\|StridedVecOrMat\\|Range\\|Range1\\|SparseMatrixCSC\\|Tuple\\|NTuple\\|Symbol\\|Function\\|Vector\\|Matrix\\|Union\\|Type\\|Any\\|Complex\\|None\\|String\\|Ptr\\|Void\\|Exception\\|Task\\|Signed\\|Unsigned\\|Associative\\|Dict\\|IO\\|IOStream\\|Ranges\\|Rational\\|Regex\\|RegexMatch\\|Set\\|IntSet\\|ASCIIString\\|UTF8String\\|ByteString\\|Expr\\|WeakRef\\|Nothing\\|ObjectIdDict\\|SubString\\)\\>" .
      font-lock-type-face)
    (cons
     (concat "\\<\\("
         (mapconcat
          'identity
          '("if" "else" "elseif" "while" "for" "begin" "end" "quote"
            "try" "catch" "return" "local" "abstract" "function" "macro" "ccall"
	    "finally" "typealias" "break" "continue" "type" "global" "@\\w+"
	    "module" "using" "import" "export" "const" "let" "bitstype" "do"
	    "baremodule" "importall" "immutable")
          "\\|") "\\)\\>")
     'font-lock-keyword-face)
    '("\\<\\(true\\|false\\|C_NULL\\|Inf\\|NaN\\|Inf32\\|NaN32\\|nothing\\)\\>" . font-lock-constant-face)
    (list julia-unquote-regex 2 'font-lock-constant-face)
    (list julia-char-regex 2 'font-lock-string-face)
    (list julia-forloop-in-regex 1 'font-lock-keyword-face)
    ;; (cons ess-subset-regexp 'font-lock-constant-face)
    (cons "\\(\\sw+\\) ?(" '(1 font-lock-function-name-face keep))
    ;(list julia-string-regex 0 'font-lock-string-face)
))

(defconst julia-block-start-keywords
  (list "if" "while" "^\s*for" "begin" "try" "function" "type" "let" "macro"
	"quote" "do" "immutable"))

(defconst julia-block-other-keywords
  (list "else" "elseif"))

(defconst julia-block-end-keywords
  (list "end" "else" "elseif" "catch" "finally"))

(defun julia-at-keyword (kw-list)
  "Return the word at point if it matches any keyword in KW-LIST.
KW-LIST is a list of strings.  The word at point is not considered
a keyword if used as a field name, X.word, or quoted, :word."
  (and (or (= (point) 1)
	   (and (not (equal (char-before (point)) ?.))
		(not (equal (char-before (point)) ?:))))
       (not (ess-inside-string-or-comment-p (point)))
       (not (ess-inside-brackets-p (point)))
       (member (current-word) kw-list)))

(defun julia-last-open-block-pos (min)
  "Move back and return the position of the last open block, if one found.
Do not move back beyond position MIN."
  (let ((count 0))
    (while (not (or (> count 0) (<= (point) min)))
      (backward-word 1)
      (setq count
	    (cond ((julia-at-keyword julia-block-start-keywords)
		   (+ count 1))
		  ((and (equal (current-word) "end")
			(not (ess-inside-comment-p))
                        (not (ess-inside-brackets-p)))
		   (- count 1))
		  (t count))))
    (if (> count 0)
	(point)
      nil)))

(defun julia-last-open-block (min)
  "Move back and return indentation level for last open block.
Do not move back beyond MIN."
  (let ((pos (julia-last-open-block-pos min)))
    (and pos
	 (progn
	   (goto-char pos)
	   (+ julia-basic-offset (current-indentation))))))

(defun julia-form-indent ()
  "Return indent implied by a special form opening on the previous line."
  (forward-line -1)
  (end-of-line)
  (backward-sexp)
  (if (julia-at-keyword julia-block-other-keywords)
      (+ julia-basic-offset (current-indentation))
    (if (char-equal (char-after (point)) ?\()
        (progn
          (backward-word 1)
          (let ((cur (current-indentation)))
            (if (julia-at-keyword julia-block-start-keywords)
                (+ julia-basic-offset cur)
              nil)))
      nil)))

(defun julia-paren-indent ()
  "Return indent by last opening paren."
  (let* ((p (parse-partial-sexp
             (save-excursion
               ;; only indent by paren if the last open
               ;; paren is closer than the last open
               ;; block
               (or (julia-last-open-block-pos (point-min))
                   (point-min)))
             (progn (beginning-of-line)
                    (point))))
         (pos (cadr p)))
    (if (or (= 0 (car p)) (null pos))
        nil
      (progn (goto-char pos) (+ 1 (current-column))))))

(defun julia-indent-line ()
  "Indent current line of julia code."
  (interactive)
  (end-of-line)
  (indent-line-to
   (or (and (ess-inside-string-p (point-at-bol)) 0)
       (save-excursion (ignore-errors (julia-form-indent)))
       (save-excursion (ignore-errors (julia-paren-indent)))
       ;; previous line ends in =
       (save-excursion
         (beginning-of-line)
         (skip-chars-backward " \t\n")
         (when (eql (char-before) ?=)
           (+ julia-basic-offset (current-indentation))))
       (save-excursion
         (let ((endtok (progn
                         (beginning-of-line)
                         (forward-to-indentation 0)
                         (julia-at-keyword julia-block-end-keywords))))
           (ignore-errors (+ (julia-last-open-block (point-min))
                             (if endtok (- julia-basic-offset) 0)))))
       ;; take same indentation as previous line
       (save-excursion (forward-line -1)
                       (current-indentation))
       0))
  (when (julia-at-keyword julia-block-end-keywords)
    (forward-word 1)))

(defvar julia-editing-alist
  '((paragraph-start		  . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-separate		  . (concat "\\s-*$\\|" page-delimiter))
    (paragraph-ignore-fill-prefix . t)
    (require-final-newline	  . t)
    (comment-start		  . "# ")
    (comment-add                  . 1)
    (comment-start-skip		  . "#+\\s-*")
    (comment-column		  . 40)
    (ess-indent-line-function	  . 'julia-indent-line)
    (indent-line-function	  . 'julia-indent-line)
    (parse-sexp-ignore-comments	  . t)
    (ess-style		  	  . ess-default-style) ;; ignored
    (ess-local-process-name	  . nil)
    (ess-mode-syntax-table	  . julia-syntax-table)
    (add-log-current-defun-header-regexp . "^.*function[ \t]*\\([^ \t(]*\\)[ \t]*(")
    (font-lock-defaults		  . '(julia-font-lock-keywords nil nil ((?\_ . "w"))))
    )
  "General options for julia source files.")

(defun julia-send-string-function (process string visibly)
  "Send the Julia STRING to the PROCESS.
VISIBLY is not currently used."
  (let ((file (concat temporary-file-directory "julia_eval_region.jl")))
    (with-temp-file file
      (insert string))
    (process-send-string process (format ess-load-command file))))


;;; HELP
(defun julia-get-help-topics (&optional proc)
  (append (ess-get-words-from-vector "ESS.all_help_topics()\n")
          (julia--get-objects)))
    ;; (ess-command com)))

(defun julia--retrive-topics (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (require 'url)
    (goto-char (point-min))
    (let (out)
      (while (re-search-forward "toctree.*href=\"\\(.+\\)\">\\(.+\\)</a" nil t)
        (push (propertize (match-string 2)
                          :manual (concat url (match-string 1)))
              out))
      (kill-buffer)
      (nreverse out))))

(defvar julia--manual-topics nil)
(defun julia-manual-lookup-function (&rest args) ; args are not used
  (interactive)
  "Look up topics at http://docs.julialang.org/en/latest/manual/"
  ;; <li class="toctree-l1"><a class="reference internal" href="introduction/">Introduction</a></li>
  (let* ((pages (or julia--manual-topics
                    (setq julia--manual-topics
                          (julia--retrive-topics "http://docs.julialang.org/en/latest/manual/"))))
         (page (ess-completing-read "Lookup:" pages nil t)))
    (browse-url (get-text-property 1 :manual page))))

(defvar julia--reference-topics nil)
(defun julia-reference-lookup-function (&rest args) ; args are not used
  (interactive)
  "Look up reference topics"
  ;; <li class="toctree-l1"><a class="reference internal" href="introduction/">Introduction</a></li>
  (let* ((pages (ess-get-words-from-vector "ESS.help_categories()\n")))
    (ess-display-help-on-object
     (ess-completing-read "Category" pages nil t))))



;;; COMPLETION
(defun julia-object-completion ()
  "Return completions at point in a format required by `completion-at-point-functions'. "
  (let ((proc (ess-get-next-available-process ess-dialect t))
        (beg (ess-symbol-start)))
    (if proc
        (when beg
          (let* ((prefix (buffer-substring-no-properties beg (point)))
                 (obj (and (string-match "\\(.*\\)\\..*$" prefix)
                           (match-string 1 prefix)))
                 (beg (if obj
                          (+ beg 1 (length obj))
                        beg)))
            (list beg (point)
                  (nreverse (mapcar 'car (julia--get-objects proc obj)))
                  :exclusive 'no)))
      (when (string-match "complet" (symbol-name last-command))
        (message "No ESS process of dialect %s started" ess-dialect)
        nil))))

(defun julia--get-objects (&optional proc obj)
  "Return all available objects.
Local caching might be used. If MODULE is givven, return only
objects from that MODULE."
  (setq proc (or proc
                 (get-process ess-local-process-name)))
  (when (process-live-p proc)
    (let ((objects (process-get proc 'objects)))
     (if (process-get proc 'busy)
         (if obj
             (assoc obj objects)
           (process-get proc 'objects))
       (if obj
           (or (cdr (assoc obj objects))
               ;; don't cache composite objects and datatypes
               (julia--get-components proc obj))
         ;; this segment is entered when user completon at top level is
         ;; requested, either Tab or AC. Hence Main is always updated.
         (let ((modules (ess-get-words-from-vector
                         "ESS.main_modules()\n" nil nil proc))
               (loc (process-get proc 'last-objects-cache))
               (lev (process-get proc 'last-eval)))
           (prog1
               (mapcan (lambda (mod)
                         ;; we are caching all modules, and reinit Main every
                         ;; time user enters commands
                         (copy-sequence
                          (or (and (or (not (equal mod "Main"))
                                       (ignore-errors (time-less-p lev loc)))
                                   (cdr (assoc mod objects)))
                              (julia--get-components proc mod t))))
                       modules)
             (process-put proc 'last-objects-cache (current-time)))))))))

(defun julia--get-components (proc obj &optional cache?)
  (with-current-buffer (ess-command (format "ESS.components(%s)\n" obj)
                                    nil nil nil nil proc)
    (goto-char (point-min))
    (let (list)
      (while (re-search-forward
              "^\\([^ \t\n]+\\) +\\([^ \t\n]+\\)$" nil t)
        (push (cons (match-string 1) (match-string 2)) list))
      (when cache?
        (let ((objects (process-get proc 'objects)))
         (push (cons obj list) objects)
         (process-put proc 'objects objects)))
      list)))


;;; AC
(defvar  ac-source-julia-objects
  '((prefix     . ess-symbol-start)
    (requires   . 2)
    (candidates . ess-ac-julia-objects)
    (document   . ess-ac-help-object)
    )
  "Auto-completion source for julia objects")

(defun ess-ac-julia-objects ()
  "Get all cached objects"
  (let ((aprf ac-prefix))
    (let ((proc (ess-get-next-available-process nil t)))
      (when aprf
        (if (string-match "\\(.*\\)\\..*$" aprf)
            (let ((module (match-string 1 aprf)))
              (mapcar (lambda (el) (concat module "." (car el)))
                      (julia--get-objects proc module)))
          (julia--get-objects proc))))))



;;; ERRORS
(defvar julia-error-regexp-alist '(julia-in julia-at)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(julia-in  "^\\s-*in [^ \t\n]* \\(at \\(.*\\):\\([0-9]+\\)\\)" 2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(julia-at "^\\S-+\\s-+\\(at \\(.*\\):\\([0-9]+\\)\\)"  2 3 nil 2 1))



;;; ELDOC
(defun julia-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (interactive)
  (when (and (ess-process-live-p)
             (not (ess-process-get 'busy)))
    (let ((funname (or (and ess-eldoc-show-on-symbol ;; aggressive completion
                            (symbol-at-point))
                       (car (ess--funname.start)))))
      (when funname
        (let* ((args (copy-sequence (nth 2 (ess-function-arguments funname))))
               (W (- (window-width (minibuffer-window)) (+ 4 (length funname))))
               (doc (concat (propertize funname 'face font-lock-function-name-face) ": ")))
          (when args
            (setq args (sort args (lambda (s1 s2)
                                    (< (length s1) (length s2)))))
            (setq doc (concat doc (pop args)))
            (while (and args (< (+ (length doc) (length (car args))) W))
              (setq doc (concat doc "  "
                                (pop args))))
            (when (and args (< (length doc) W))
              (setq doc (concat doc " {--}"))))
          doc)))))


;;; IMENU
(defvar julia-imenu-generic-expression
  ;; don't use syntax classes, screws egrep
  '(("Function (_)" "[ \t]*function[ \t]+\\(_[^ \t\n]*\\)" 1)
    ("Function" "^[ \t]*function[ \t]+\\([^_][^\t\n]*\\)" 1)
    ("Const" "[ \t]*const \\([^ \t\n]*\\)" 1)
    ("Type"  "^[ \t]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ \t\n]*\\)" 1)
    ("Require"      " *\\(\\brequire\\)(\\([^ \t\n)]*\\)" 2)
    ("Include"      " *\\(\\binclude\\)(\\([^ \t\n)]*\\)" 2)
    ))


;;; CORE
(defvar julia-customize-alist
  '((comint-use-prompt-regexp		. t)
    (ess-eldoc-function                 . 'julia-eldoc-function)
    (inferior-ess-primary-prompt	. "a> ") ;; from julia>
    (inferior-ess-secondary-prompt	. nil)
    (inferior-ess-prompt		. "\\w*> ")
    (ess-local-customize-alist		. 'julia-customize-alist)
    (inferior-ess-program		. inferior-julia-program-name)
    (inferior-ess-font-lock-defaults	. julia-font-lock-keywords)
    (ess-get-help-topics-function	. 'julia-get-help-topics)
    (ess-help-web-search-command        . "http://docs.julialang.org/en/latest/search/?q=%s")
    (ess-manual-lookup-command          . 'julia-manual-lookup-function)
    (ess-reference-lookup-command       . 'julia-reference-lookup-function)
    (ess-load-command   		. "include(\"%s\")\n")
    (ess-funargs-command                . "ESS.fun_args(\"%s\")\n")
    (ess-dump-error-re			. "in \\w* at \\(.*\\):[0-9]+")
    (ess-error-regexp			. "\\(^\\s-*at\\s-*\\(?3:.*\\):\\(?2:[0-9]+\\)\\)")
    (ess-error-regexp-alist		. julia-error-regexp-alist)
    (ess-send-string-function		. nil);'julia-send-string-function)
    (ess-imenu-generic-expression       . julia-imenu-generic-expression)
    ;; (inferior-ess-objects-command	. inferior-R-objects-command)
    ;; (inferior-ess-search-list-command	. "search()\n")
    (inferior-ess-help-command		. "ESS.help(\"%s\")\n")
    ;; (inferior-ess-help-command	. "help(\"%s\")\n")
    (ess-language			. "julia")
    (ess-dialect			. "julia")
    (ess-suffix				. "jl")
    (ess-ac-sources                     . '(ac-source-julia-objects))
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
    (ess-object-name-db-file		. "ess-jl-namedb.el" )
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

(defvar julia-versions '("julia")
  "List of partial strings for versions of Julia to access within ESS.
Each string specifies the start of a filename.  If a filename
beginning with one of these strings is found on `exec-path', a M-x
command for that version of Julia is made available.")

(defcustom inferior-julia-args ""
  "String of arguments (see 'julia --help') used when starting julia."
  ;; These arguments are currently not passed to other versions of julia that have
  ;; been created using the variable `ess-r-versions'."
  :group 'ess-julia
  :type 'string)

;;;###autoload
(defun julia-mode  (&optional proc-name)
  "Major mode for editing julia source.  See `ess-mode' for more help."
  (interactive "P")
  ;; (setq ess-customize-alist julia-customize-alist)
  (ess-mode julia-customize-alist proc-name)
  ;; for emacs >= 24
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'julia-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (set (make-local-variable 'end-of-defun-function) 'ess-end-of-function)
  ;; (local-set-key  "\t" 'julia-indent-line) ;; temp workaround
  ;; (set (make-local-variable 'indent-line-function) 'julia-indent-line)
  (set (make-local-variable 'julia-basic-offset) 4)
  (setq imenu-generic-expression julia-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu-jl")
  (run-hooks 'julia-mode-hook))

(defvar julia-mode-hook nil)
(defvar julia-post-run-hook nil
  "Functions run in process buffer after starting julia process.")

;;;###autoload
(defun julia (&optional start-args)
  "Call 'julia'.
Optional prefix (C-u) allows to set command line arguments, such as
--load=<file>.  This should be OS agnostic.
If you have certain command line arguments that should always be passed
to julia, put them in the variable `inferior-julia-args'."
  (interactive "P")
  ;; get settings, notably inferior-julia-program-name :
  (if (null inferior-julia-program-name)
      (error "'inferior-julia-program-name' does not point to 'julia-basic' executable")
    (setq ess-customize-alist julia-customize-alist)
    (ess-write-to-dribble-buffer   ;; for debugging only
     (format
      "\n(julia): ess-dialect=%s, buf=%s, start-arg=%s\n current-prefix-arg=%s\n"
      ess-dialect (current-buffer) start-args current-prefix-arg))
    (let* ((jl-start-args
	    (concat inferior-julia-args " " ; add space just in case
		    (if start-args
			(read-string
                         (concat "Starting Args"
                                 (if inferior-julia-args
                                     (concat " [other than '" inferior-julia-args "']"))
                                 " ? "))
		      nil))))
      (inferior-ess jl-start-args)

      (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
      (add-hook 'completion-at-point-functions 'julia-object-completion nil 'local)
      (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)

      (ess--tb-start)
      (set (make-local-variable 'julia-basic-offset) 4)
      ;; remove ` from julia's logo
      (goto-char (point-min))
      (while (re-search-forward "`" nil t)
        (replace-match "'"))
      (goto-char (point-max))
      (ess--inject-code-from-file (format "%sess-julia.jl" ess-etc-directory))
      (with-ess-process-buffer nil
        (run-mode-hooks 'julia-post-run-hook))
      )))

(add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode))

(provide 'ess-julia)

;;; ess-julia.el ends here
