;; ess-test-r-utils.el --- Various utilities for ess R tests
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
  (let ((prompt-regexp "^\\([+.>] \\)\\{2,\\}")
        (proc (get-buffer-process (run-ess-test-r-vanilla)))
        (inhibit-message ess-inhibit-message-in-tests))
    ;; just in case
    (ess-wait-for-process proc)
    (ess--flush-accumulated-output proc)
    (unwind-protect
        (with-current-buffer (process-buffer proc)
          (let ((inhibit-read-only t))
            (erase-buffer)
            ;; (switch-to-buffer (current-buffer)) ; for debugging
            (cond
             ((or (null type) (eq type 'string))
              (ess-send-string proc input))
             ((eq type 'repl)
              (insert input)
              (inferior-ess-send-input))
             ((or (eq type 'region)
                  (eq type 'c-c))
              (with-temp-buffer
                (insert input)
                (goto-char (point-min))
                (R-mode)
                (setq ess-current-process-name (process-name proc))
                (if (eq type 'region)
                    (ess-eval-region (point-min) (point-max) nil)
                  (ess-eval-region-or-function-or-paragraph nil))))
             (t (error "Invalid TYPE parameter")))
            (process-send-string proc "cat('END')\n")
            ;; wait till we have our end marker
            (while (not (looking-back "\n?END> " nil t))
              (sleep-for 0.01)
              (goto-char (point-max)))
            ;; remove END>
            (delete-region (match-beginning 0) (match-end 0))
            ;; (buffer-substring-no-properties (point-min) (point-max))
            (replace-regexp-in-string
             prompt-regexp "> "
             (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-process proc)
      ;; fixme: kill in sentinel; this doesn't work in batch mode
      ;; (kill-buffer (process-buffer proc))
      )))


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
             (progn
               (setq ess-r-tests-current-output-buffer *inf-buf*)
               (let ((inhibit-read-only t))
                 (with-current-buffer ess-r-tests-current-output-buffer
                   (erase-buffer)))
               (set-process-filter *proc* 'inferior-ess-ordinary-filter)
               (prog1 (progn ,@body)
                 (ess-wait-for-process *proc*)))
           (kill-process *proc*)
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


(provide 'ess-test-r-utils)

;;; ess-test-r-utils.el ends here
