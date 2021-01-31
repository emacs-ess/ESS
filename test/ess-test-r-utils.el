;; ess-test-r-utils.el --- Various utilities for ess R tests  -*- lexical-binding: t; -*-
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; A copy of the GNU General Public License is available at
;; https://www.r-project.org/Licenses/
;;
;;; Commentary:
;; Various utilities for ESS tests.

;;; Code:
(require 'ert)
(require 'etest)
(require 'ess-r-mode)
(require 'tramp)

(defvar ess-test-fixtures-directory
  (expand-file-name "fixtures"
                    (file-name-directory (or load-file-name
                                             buffer-file-name)))
  "Location of the fixtures directory.")

(defvar ess-inhibit-message-in-tests nil)

(defmacro with-ess-test-file (file &rest body)
  (declare (indent 1) (debug (&rest body)))
  `(let ((inhibit-message ess-inhibit-message-in-tests)
         (*file* ,file))
     (save-window-excursion
       (set-buffer (if *file*
                       (find-file-noselect *file*)
                     (generate-new-buffer " *with-r-file-temp*")))
       ,@body)))

(defmacro with-ess-test-r-file (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(with-ess-test-file ,file
     (R-mode)
     ,@body))

(defmacro with-ess-test-c-file (file &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(with-ess-test-file ,file
     (c-mode)
     ,@body))

;; Borrowed from org
(defmacro ess-r-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with `ess-r-mode' as the active
mode holding TEXT.  If the string \"¶\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1) (debug (form body)))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	     (ess-r-mode-hook nil))
     (with-temp-buffer
       (ess-r-mode)
       (let ((point (string-match "¶" inside-text)))
	     (if point
	         (progn
	           (insert (replace-match "" nil nil inside-text))
	           (goto-char (1+ (match-beginning 0))))
	       (insert inside-text)
	       (goto-char (point-min))))
       ,@body)))

(defmacro ess-cpp-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with `cpp-mode' as the active
mode holding TEXT. Turn on `ess-roxy-mode'. If the string \"¶\"
appears in TEXT then remove it and place the point there before
running BODY, otherwise place the point at the beginning of the
inserted text."
  (declare (indent 1) (debug (form body)))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	     (c++-mode-hook nil))
     (with-temp-buffer
       (c++-mode)
       (ess-roxy-mode)
       (let ((point (string-match "¶" inside-text)))
	     (if point
	         (progn
	           (insert (replace-match "" nil nil inside-text))
	           (goto-char (1+ (match-beginning 0))))
	       (insert inside-text)
	       (goto-char (point-min))))
       ,@body)))

(defun run-ess-test-r-vanilla ()
  "Start vanila R process and return the process object."
  (save-window-excursion
    (let ((inhibit-message ess-inhibit-message-in-tests)
          (ess-ask-for-ess-directory nil))
      (R "--vanilla"))))


(let ((inf-bufs '((output . nil)
                  (tracebug . nil))))
  (defun ess-r-test-proc-buf--init (type)
    (setf (cdr (assq type inf-bufs))
          (run-ess-test-r-vanilla))
    (let* ((inf-buf (cdr (assq type inf-bufs)))
           (inf-proc (get-buffer-process inf-buf))
           (inf-filter (pcase type
                         (`output 'inferior-ess-output-filter)
                         (`tracebug 'inferior-ess-tracebug-output-filter)
                         (_ (error "Unknown filter type")))))
      (ess-wait-for-process (get-buffer-process inf-buf))
      (set-process-filter inf-proc inf-filter)
      (with-current-buffer inf-buf
        ;; Replacing strings of " +" causes random failures, probably
        ;; because of the buffered output
        (setq-local inferior-ess-replace-long+ nil)
        (let ((inhibit-read-only t))
          (erase-buffer)))
      inf-buf))

  (defun ess-r-test-proc-buf (type)
    "Common process buffer for tests."
    (or (cdr (assq type inf-bufs))
        (ess-r-test-proc-buf--init type))))

(defun ess-send-input-to-R (input &optional type)
  "Eval INPUT and return the entire content of the REPL buffer.
TYPE can be one of 'string, 'region 'c-c or 'repl. If nil or
'string, use `ess-send-string' (lowest level primitive); if
'region use `ess-eval-region' if 'c-c use
`ess-eval-region-or-function-or-paragraph' which is by default
bound to C-c C-c; if 'repl, eval interactively at the REPL. All
prompts in the output are replaced with '> '. There is no full
proof way to test for prompts given that process output could be
split arbitrary."
  (let* ((prompt-regexp "^\\([+.>] \\)\\{2,\\}")
         (inf-buf (run-ess-test-r-vanilla))
         (inf-proc (get-buffer-process inf-buf))
         (inhibit-message ess-inhibit-message-in-tests))
    ;; just in case
    (ess-wait-for-process inf-proc)
    (ess--flush-accumulated-output inf-proc)
    (ess-test-unwind-protect inf-buf
      (with-current-buffer (process-buffer inf-proc)
        (let ((inhibit-read-only t))
          (erase-buffer)
          ;; (switch-to-buffer (current-buffer)) ; for debugging
          (cond
           ((or (null type) (eq type 'string))
            (ess-send-string inf-proc input))
           ((eq type 'repl)
            (insert input)
            (inferior-ess-send-input))
           ((or (eq type 'region)
                (eq type 'c-c))
            (with-temp-buffer
              (insert input)
              (goto-char (point-min))
              (R-mode)
              (setq ess-current-process-name (process-name inf-proc))
              (if (eq type 'region)
                  (ess-eval-region (point-min) (point-max) nil)
                (ess-eval-region-or-function-or-paragraph nil))))
           (t (error "Invalid TYPE parameter")))
          (process-send-string inf-proc "cat('END')\n")
          ;; wait till we have our end marker
          (while (not (looking-back "\n?END> " nil t))
            (sleep-for 0.01)
            (goto-char (point-max)))
          ;; remove END>
          (delete-region (match-beginning 0) (match-end 0))
          ;; (buffer-substring-no-properties (point-min) (point-max))
          (replace-regexp-in-string
           prompt-regexp "> "
           (buffer-substring-no-properties (point-min) (point-max))))))))


(defun ess-test-R-indentation (file style)
  (let ((ess-style-alist ess-test-style-alist)
        (buff (find-file-noselect file t t))
        (inhibit-message ess-inhibit-message-in-tests))
    (with-current-buffer buff
      (R-mode)
      (ess-set-style style)
      (set-buffer-modified-p nil)
      (should (not-change-on-indent buff)))))

;; !!! NB: proc functionality from now on uses inferior-ess-ordinary-filter and
;; !!! *proc* dynamic var
(defmacro with-r-running (buffer-or-file &rest body)
  "Run BODY within BUFFER-OR-FILE with attached R process.
If BUFFER-OR-FILE is a file, the file is visited first. The R
process is run with `inferior-ess-ordinary-filter' which is not
representative to the common interactive use with tracebug on."
  (declare (indent 1) (debug (form body)))
  `(let* ((inhibit-message ess-inhibit-message-in-tests)
          (buffer-or-file ,buffer-or-file)
          (r-file-buffer (cond ((bufferp buffer-or-file)
                                buffer-or-file)
                               ((stringp buffer-or-file)
                                (find-file-noselect buffer-or-file))
                               (t
                                (generate-new-buffer " *with-r-file-temp*")))))
     (save-window-excursion
       (switch-to-buffer r-file-buffer)
       (R-mode)
       (let* ((*proc* (get-buffer-process (run-ess-test-r-vanilla)))
              (ess-local-process-name (process-name *proc*))
              (*inf-buf* (process-buffer *proc*)))
         (unwind-protect
             (ess-test-unwind-protect *inf-buf*
               (setq ess-r-tests-current-output-buffer *inf-buf*)
               (let ((inhibit-read-only t))
                 (with-current-buffer ess-r-tests-current-output-buffer
                   (erase-buffer)))
               (set-process-filter *proc* 'inferior-ess-ordinary-filter)
               (prog1 (progn ,@body)
                 (ess-wait-for-process *proc*)))
           (setq ess-r-tests-current-output-buffer nil))))))

(defvar ess-r-tests-current-output-buffer nil)

;; The following retrieve the last output and clean the output
;; buffer. This is useful to continue testing without restarting a
;; fresh R session.

;; In case of other side effects than mere output, it's probably safer
;; to perform ulterior tests with a fresh R to avoid contaminating
;; them.

(defmacro output (&rest body)
  (declare (indent 1) (debug (&rest body)))
  `(progn
     (ess-wait-for-process *proc*)
     ,@body
     (ess-wait-for-process *proc*)
     (with-current-buffer ess-r-tests-current-output-buffer
       (ess-kill-last-line)
       (prog1 (buffer-substring-no-properties (point-min) (point-max))
         (erase-buffer)))))

(defmacro output= (body expected)
  (declare (indent 0) (debug (sexp sexp)))
  `(progn
     (let ((output (output ,body))
           (expected (eval ,expected)))
       (if (string= output expected)
           output
         ;; Probably a better way but this gets the job done
         (signal 'ert-test-failed (list (concat "Expected: \n" expected)
                                        (concat "Result: \n" output)))))))

(defun face-at (point)
  (save-excursion
    (if (>= point 0)
        (goto-char point)
      (forward-char point))
    (get-char-property (point) 'face)))

(defun insert-fontified (&rest args)
  (apply #'insert args)
  (font-lock-default-fontify-buffer))

(defmacro ess-test-sleep-while (test seconds timeout &optional msg)
  `(let ((_seconds ,seconds)
         (_timeout ,timeout)
         (_time-start (current-time)))
     (while ,test
       (when (time-less-p _timeout (time-subtract (current-time) _time-start))
         (error (or ,msg "Exceeded timeout")))
       (sleep-for _seconds))
     t))

;; It is safer to kill the buffer synchronously, otherwise it might be
;; reused in another test
(defmacro ess-test-unwind-protect (inf-buf &rest body)
  (declare (indent 1))
  `(unwind-protect (progn ,@body)
     (let* ((inf-buf ,inf-buf)
            (inf-proc (get-buffer-process inf-buf)))
       (when (and inf-proc (process-live-p inf-proc))
         (set-process-query-on-exit-flag inf-proc nil)
         (kill-process inf-proc)
         (ess-test-sleep-while (process-live-p inf-proc) 0.001 1
                               "Expected dead process"))
       (kill-buffer inf-buf))))

(defun ess-test-r-set-local-process (&optional type)
  (let ((proc-buf (ess-r-test-proc-buf (or type 'tracebug))))
    (setq ess-local-process-name (process-name (get-buffer-process proc-buf)))
    (setq etest-local-inferior-buffer proc-buf)))

(cl-defmacro etest-deftest-r (name args &body body)
  (declare (doc-string 3)
           (indent 2))
  (let ((etest--docstring (when (stringp (car body))
                            (list (pop body)))))
    `(etest-deftest ,name ,args
       ,@etest--docstring
       :init ((mode . r)
              (eval . (ess-test-r-set-local-process)))
       ,@body)))

;; Define a mock TRAMP method to use for testing. This code is taken from
;; `tramp-tests.el'.
(add-to-list
 'tramp-methods
 '("mock"
   (tramp-login-program        "sh")
   (tramp-login-args           (("-i")))
   (tramp-direct-async-args    (("-c")))
   (tramp-remote-shell         "/bin/sh")
   (tramp-remote-shell-args    ("-c"))
   (tramp-connection-timeout   10)))

(defun ess-test-create-remote-path (path)
  "Construct a remote path using the 'mock' TRAMP method.
Take a string PATH representing a local path, and construct a
remote path that uses the 'mock' TRAMP method."
  (let ((full-path (abbreviate-file-name (expand-file-name path))))
    (concat "/mock::" full-path)))

(provide 'ess-test-r-utils)

;;; ess-test-r-utils.el ends here
