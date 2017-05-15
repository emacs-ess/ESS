;; ess-julia.el --- ESS julia mode and inferior interaction
;;
;; Copyright (C) 2012-2015 Vitalie Spinu and the ESS Core team.
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
;;
;;  Customise inferior-julia-program-name to point to your julia binary
;;  and start the inferior with M-x julia.
;;
;;  As of Sept 2015, this file depends heavily on julia-mode.el from the Julia
;;  sources.  If you install ESS using `make', this will work fine, otherwise
;;  ensure that julia-mode.el is on your path before loading this file.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'compile)
(require 'ess-utils)
(require 'ess-r-mode)

;;;--- ALL the following only if  julia-mode is found and loaded correctly : ----------
(condition-case nil
      (progn
        (require 'julia-mode)
        (when (featurep 'julia-mode)

(eval-when-compile
  (require 'cl))

(autoload 'inferior-ess "ess-inf" "Run an ESS process.")
(autoload 'ess-mode     "ess-mode" "Edit an ESS process.")

(defun ess-julia-send-string-function (process string visibly)
  "Send the Julia STRING to the PROCESS.
VISIBLY is not currently used."
  (let ((file (concat temporary-file-directory "julia_eval_region.jl")))
    (with-temp-file file
      (insert string))
    (process-send-string process (format ess-load-command file))))


;;; HELP
(defun ess-julia-get-help-topics (&optional proc)
  (append (with-current-buffer (ess-command "ESS.all_help_topics()\n")
            (split-string (buffer-string) "\n"))
          (ess-julia--get-objects proc)))

(defun ess-julia--retrive-topics (url)
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

(defvar ess-julia--manual-topics nil)
(defun ess-julia-manual-lookup-function (&rest args) ; args are not used
  (interactive)
  "Look up topics at http://docs.julialang.org/en/latest/manual/"
  ;; <li class="toctree-l1"><a class="reference internal" href="introduction/">Introduction</a></li>
  (let* ((pages (or ess-julia--manual-topics
                    (setq ess-julia--manual-topics
                          (ess-julia--retrive-topics "http://docs.julialang.org/en/latest/manual/"))))
         (page (ess-completing-read "Lookup:" pages nil t)))
    (browse-url (get-text-property 1 :manual page))))

(defun ess-julia-input-sender (proc string)
  (save-current-buffer
    (let* ((help-?-regexp "^ *\\(?:\\(?1: *?\\? *\\)\\(?2:.+\\)\\)")
           (help-?-match (string-match help-?-regexp string)))
      (cond (help-?-match
             (ess-display-help-on-object (match-string 2 string))
             (process-send-string proc "\n"))
            (t ;; normal command
             (inferior-ess-input-sender proc string))))))

;; julia 0.3.0 doesn't provide categories. Thus we don't support this anymore.
;; (defun ess-julia-reference-lookup-function (&rest args) ; args are not used
;;   (interactive)
;;   "Look up reference topics"
;;   ;; <li class="toctree-l1"><a class="reference internal" href="introduction/">Introduction</a></li>
;;   (let* ((pages (ess-get-words-from-vector "ESS.help_categories()\n")))
;;     (ess-display-help-on-object
;;      (ess-completing-read "Category" pages nil t))))



;;; COMPLETION
(defun ess-julia-latexsub-completion ()
  "Complete latex input, and returns in a format required by `completion-at-point-functions'."
  (if (julia-latexsub) ; julia-latexsub returns nil if it performed a completion, the point otherwise
      nil
    (lambda () t) ;; bypass other completion methods
    ))

(defun ess-julia-object-completion ()
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
                  (nreverse (mapcar 'car (ess-julia--get-objects proc obj)))
                  :exclusive 'no)))
      (when (string-match "complet" (symbol-name last-command))
        (message "No ESS process of dialect %s started" ess-dialect)
        nil))))

(defun ess-julia-objects (prefix &optional proc)
  "Given PREFIX get all cached objects from PROC."
  (when prefix
    (let ((proc (or proc (ess-get-next-available-process nil t))))
      (if (string-match "\\(.*\\)\\..*$" prefix)
          (let ((module (match-string 1 prefix)))
            (mapcar (lambda (el) (concat module "." (car el)))
                    (ess-julia--get-objects proc module)))
        (ess-julia--get-objects proc)))))

(defun ess-julia--get-objects (&optional proc obj)
  "Return all available objects.
Local caching might be used. If MODULE is givven, return only
objects from that MODULE."
  (setq proc (or proc (ess-get-process)))
  (when (stringp proc)
    (setq proc (get-process proc)))
  (when (process-live-p proc)
    (let ((objects (process-get proc 'objects)))
      (if (process-get proc 'busy)
          (if obj
              (assoc obj objects)
            (process-get proc 'objects))
        (if obj
            (or (cdr (assoc obj objects))
                ;; don't cache composite objects and datatypes
                (ess-julia--get-components proc obj))
          ;; this segment is entered when user completon at top level is
          ;; requested, either Tab or AC. Hence Main is always updated.
          (let ((modules (ess-get-words-from-vector
                          "ESS.main_modules()\n" nil nil proc))
                (loc (process-get proc 'last-objects-cache))
                (lev (process-get proc 'last-eval)))
            (prog1
                (apply #'nconc
                       (mapcar
                        (lambda (mod)
                          ;; we are caching all modules, and reinit Main every
                          ;; time user enters commands
                          (copy-sequence
                           (or (and (or (not (equal mod "Main"))
                                        (ignore-errors (time-less-p lev loc)))
                                    (cdr (assoc mod objects)))
                               (ess-julia--get-components proc mod t))))
                        modules))
              (process-put proc 'last-objects-cache (current-time)))))))))

(defun ess-julia--get-components (proc obj &optional cache?)
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

(defun ess-julia-get-object-help-string (sym)
  "Help string for ac."
  (let ((proc (ess-get-next-available-process nil t)))
    (if (null proc)
        "No free ESS process found"
      (let ((buf (get-buffer-create " *ess-command-output*")))
        (with-current-buffer (process-buffer proc)
          (ess-with-current-buffer buf
            (ess--flush-help-into-current-buffer sym nil t)))
        (with-current-buffer buf
          (ess-help-underline)
          (goto-char (point-min))
          (buffer-string))))))

(defvar ac-source-ess-julia-objects
  '((prefix     . ess-symbol-start)
    (requires   . 2)
    (candidates . ess-ac-julia-objects)
    (document   . ess-julia-get-object-help-string))
  "Auto-completion source for julia objects")

(defun ess-ac-julia-objects ()
  (ess-julia-objects ac-prefix))

(defun company-ess-julia-objects (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ess-julia-objects))
    (prefix (unless (company-in-string-or-comment)
              (let ((start (ess-symbol-start)))
                (when start (buffer-substring-no-properties start (point))))))
    (candidates (let ((proc (ess-get-next-available-process)))
                  (when proc
                    (all-completions arg (mapcar (lambda (x) (or (car-safe x) x))
                                                 (ess-julia-objects arg proc))))))
    (doc-buffer (company-doc-buffer (ess-julia-get-object-help-string arg)))))


;;; ERRORS
(defvar ess-julia-error-regexp-alist '(ess-julia-in ess-julia-at ess-julia-while-load)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(ess-julia-in  "^\\s-*in [^ \t\n]* \\(at \\(.*\\):\\([0-9]+\\)\\)" 2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(ess-julia-at "^\\S-+\\s-+\\(at \\(.*\\):\\([0-9]+\\)\\)"  2 3 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(ess-julia-while-load "^\\s-*\\(while loading\\s-\\(.*\\), in .* on line +\\([0-9]+\\)\\)"  2 3 nil 2 1))


;;; ELDOC
(defun ess-julia-eldoc-function ()
  "Return the doc string, or nil.
If an ESS process is not associated with the buffer, do not try
to look up any doc strings."
  (interactive)
  (when (and ess-can-eval-in-background
             (ess-process-live-p)
             (not (ess-process-get 'busy)))
    (let ((funname (or (and ess-eldoc-show-on-symbol ;; aggressive completion
                            (symbol-name (ess-symbol-at-point)))
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
              (setq doc (concat doc " {--}")))
            doc))))))


;;; IMENU
(defvar ess-julia-imenu-generic-expression
  ;; don't use syntax classes, screws egrep
  '(("Function (_)" "[ \t]*function[ \t]+\\(_[^ \t\n]*\\)" 1)
    ("Function" "^[ \t]*function[ \t]+\\([^_][^\t\n]*\\)" 1)
    ("Const" "[ \t]*const \\([^ \t\n]*\\)" 1)
    ("Type"  "^[ \t]*[a-zA-Z0-9_]*type[a-zA-Z0-9_]* \\([^ \t\n]*\\)" 1)
    ("Require"      " *\\(\\brequire\\)(\\([^ \t\n)]*\\)" 2)
    ("Include"      " *\\(\\binclude\\)(\\([^ \t\n)]*\\)" 2)
    ))


;;; CORE
(defvar ess-julia-customize-alist
  '((comint-use-prompt-regexp      . t)
    (ess-eldoc-function            . 'ess-julia-eldoc-function)
    (inferior-ess-primary-prompt   . "a> ") ;; from julia>
    (inferior-ess-secondary-prompt . nil)
    (inferior-ess-prompt           . "\\w*> ")
    (ess-local-customize-alist     . 'ess-julia-customize-alist)
    (inferior-ess-program          . inferior-julia-program-name)
    (ess-get-help-topics-function  . 'ess-julia-get-help-topics)
    (ess-help-web-search-command   . "http://docs.julialang.org/en/latest/search/?q=%s")
    (ess-manual-lookup-command     . 'ess-julia-manual-lookup-function)
    ;; (ess-reference-lookup-command       . 'ess-julia-reference-lookup-function)
    (ess-load-command              . "include(\"%s\")\n")
    (ess-funargs-command           . "ESS.fun_args(\"%s\")\n")
    (ess-dump-error-re             . "in \\w* at \\(.*\\):[0-9]+")
    (ess-error-regexp              . "\\(^\\s-*at\\s-*\\(?3:.*\\):\\(?2:[0-9]+\\)\\)")
    (ess-error-regexp-alist        . ess-julia-error-regexp-alist)
    (ess-imenu-generic-expression  . ess-julia-imenu-generic-expression)
    (ess-mode-syntax-table         . julia-mode-syntax-table)
    (ess-mode-completion-syntax-table . ess-julia-completion-syntax-table)
    ;; (inferior-ess-objects-command    . inferior-ess-r-objects-command)
    ;; (inferior-ess-search-list-command        . "search()\n")
    (inferior-ess-help-command     . "ESS.help(\"%s\")\n")
    ;; (inferior-ess-help-command       . "help(\"%s\")\n")
    (ess-language                  . "julia")
    (ess-dialect                   . "julia")
    (ess-suffix                    . "jl")
    (ess-ac-sources                . '(ac-source-ess-julia-objects))
    (ess-company-backends          . '(company-ess-julia-objects))
    (ess-dump-filename-template    . (replace-regexp-in-string
                                      "S$" ess-suffix ; in the one from custom:
                                      ess-dump-filename-template-proto))
    (ess-mode-editing-alist        . nil)
    (ess-change-sp-regexp          . nil );ess-r-change-sp-regexp)
    (ess-help-sec-regex            . ess-help-r-sec-regex)
    (ess-help-sec-keys-alist       . ess-help-r-sec-keys-alist)
    (ess-loop-timeout              . ess-S-loop-timeout);fixme: dialect spec.
    (ess-function-pattern          . ess-r-function-pattern)
    (ess-object-name-db-file       . "ess-jl-namedb.el" )
    (ess-smart-operators           . ess-r-smart-operators)
    (inferior-ess-help-filetype    . nil)
    (inferior-ess-exit-command     . "exit()\n")
    ;;harmful for shell-mode's C-a: -- but "necessary" for ESS-help?
    (inferior-ess-start-file       . nil) ;; "~/.ess-R"
    (inferior-ess-start-args       . "")
    (inferior-ess-language-start   . nil)
    (ess-STERM                     . "iESS")
    (ess-editor                    . ess-r-editor)
    (ess-pager                     . ess-r-pager)
    (ess-getwd-command             . "pwd()\n")
    (ess-setwd-command             . "cd(expanduser(\"%s\"))\n")
    )
  "Variables to customize for Julia -- set up later than emacs initialization.")

(defcustom inferior-julia-args ""
  "String of arguments (see 'julia --help') used when starting julia."
  :group 'ess-julia
  :type 'string)

(defvar ess-julia-completion-syntax-table
  (let ((table (make-syntax-table ess-r-syntax-table)))
    (modify-syntax-entry ?. "_" table)
    ;; (modify-syntax-entry ?: "_" table)
    ;; (modify-syntax-entry ?$ "_" table)
    (modify-syntax-entry ?@ "_" table)
    table)
  "Syntax table used for completion and help symbol lookup.
It makes underscores and dots word constituent chars.")

;;;###autoload
(define-derived-mode ess-julia-mode julia-mode "ESS[julia]"
  "Major mode for editing julia source.  See `ess-mode' for more help."
  (ess-mode ess-julia-customize-alist nil t)
  ;; for emacs >= 24
  (remove-hook 'completion-at-point-functions 'ess-filename-completion 'local) ;; should be first
  (add-hook 'completion-at-point-functions 'ess-julia-object-completion nil 'local)
  (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
  (if (fboundp 'ess-add-toolbar) (ess-add-toolbar))
  (set (make-local-variable 'end-of-defun-function) 'ess-end-of-function)

  (set (make-local-variable 'ess-julia-basic-offset) 4)
  (setq imenu-generic-expression ess-julia-imenu-generic-expression)
  (imenu-add-to-menubar "Imenu-jl")
  (run-hooks 'ess-julia-mode-hook))

(defvar ess-julia-mode-hook nil)
(defvar ess-julia-post-run-hook nil
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
      (error "'inferior-julia-program-name' does not point to 'julia' or 'julia-basic' executable")
    (setq ess-customize-alist ess-julia-customize-alist)
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
      (add-hook 'completion-at-point-functions 'ess-julia-object-completion nil 'local)
      (add-hook 'completion-at-point-functions 'ess-filename-completion nil 'local)
      (add-hook 'completion-at-point-functions 'ess-julia-latexsub-completion nil 'local)
      (setq comint-input-sender 'ess-julia-input-sender)

      (ess--tb-start)
      (set (make-local-variable 'ess-julia-basic-offset) 4)
      ;; remove ` from julia's logo
      (goto-char (point-min))
      (while (re-search-forward "`" nil t)
        (replace-match "'"))
      ;; remove an offending unmatched parenthesis
      (goto-char (point-min))
      (forward-line 4)
      (when (re-search-forward "(" nil t)
        (replace-match "|"))
      (goto-char (point-max))
      ;; --> julia helpers from ../etc/ess-julia.jl :
      (ess--inject-code-from-file (format "%sess-julia.jl" ess-etc-directory))
      (with-ess-process-buffer nil
        (run-mode-hooks 'ess-julia-post-run-hook))
      )))

(add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode))

))  (error nil))
(provide 'ess-julia)
;;; ess-julia.el ends here
