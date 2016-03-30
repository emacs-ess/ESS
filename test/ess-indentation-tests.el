;; ess-tests.el --- Tests for ESS
;;
;;
;; Filename: ess-tests.el
;; Created: 07-05-2015 (ESS 15.09)
;; Keywords: tests, indentation
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
;; To run these tests:
;;   All tests: M-x ert t
;;
;; To apply styles for manual testing:
;;   M-: (let ((ess-style-alist ess-test-style-alist))
;;         (ess-set-style 'misc1))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ert)


;;; Indentation tests

(defvar ess-test-style-alist
  `((RRR
     ,@(cdr (assq 'RRR ess-style-alist)))
    (RRR+
     ,@(cdr (assq 'RRR+ ess-style-alist)))
    (RStudio-
     ,@(cdr (assq 'RStudio- ess-style-alist)))
    (C++
     ,@(cdr (assq 'C++ ess-style-alist)))
    (misc1
     (ess-indent-offset . 3)
     (ess-offset-block . open-delim)
     (ess-offset-arguments . (prev-call 5))
     (ess-offset-arguments-newline . open-delim)
     (ess-offset-continued . cascade)
     (ess-align-nested-calls . ("ifelse"))
     (ess-align-arguments-in-calls . nil)
     (ess-align-continuations-in-calls . nil)
     (ess-align-blocks . (fun-decl bare-blocks))
     (ess-indent-from-lhs . nil)
     (ess-indent-from-chain-start . nil)
     (ess-indent-with-fancy-comments . t))))

(defun ess-test-get-pos-from-undo-elt (e)
  "If E represents an edit, return a position value in E, the position
where the edit took place. Return nil if E represents no real change.
\nE is an entry in the buffer-undo-list."
  ;; stolen from goto-chg.el
  (cond
   ;; ((numberp e) e)			; num==changed position
   ;; ((atom e) nil)			; nil==command boundary
   ((numberp (car e)) (cdr e))	; (beg . end)==insertion
   ((stringp (car e)) (abs (cdr e))) ; (string . pos)==deletion
   ;; ((null (car e)) (nthcdr 4 e))	; (nil ...)==text property change
   ;; ((atom (car e)) nil)		; (t ...)==file modification time
   (t nil)))			; (marker ...)==marker moved

(defun not-change-on-indent (buffer)
  "Return t if BUFFER wasn't modified on indent."
  (with-current-buffer buffer
    (setq buffer-undo-list nil)
    (indent-region (point-min) (point-max))
    (not (buffer-modified-p buffer))))

(defun ess-test-explain-change-on-indent (buffer)
  "Explainer function for `not-change-on-indent'."
  (when (buffer-modified-p buffer)
    (with-current-buffer buffer
      (let ((bul buffer-undo-list)
            (change-pos (point-min)))
        (while (and bul
                    (null (setq change-pos (ess-test-get-pos-from-undo-elt (car bul)))))
          (pop bul))
        (let ((diff-file (concat (buffer-name) ".diff")))
          (diff-buffer-with-file buffer)
          (with-current-buffer "*Diff*"
            (write-region (point-min) (point-max) diff-file))
          `(,(buffer-name buffer) was modified on line ,(count-lines 1 change-pos)
            (diff was writen to ,diff-file)))))))

(put 'not-change-on-indent 'ert-explainer 'ess-test-explain-change-on-indent)

(defun ess-test-R-indentation (file style)
  (let ((ess-style-alist ess-test-style-alist)
        (buff (find-file-noselect file t t)))
    (with-current-buffer buff
      (R-mode)
      (ess-set-style style)
      (set-buffer-modified-p nil)
      (should (not-change-on-indent buff)))))

(ert-deftest test-ess-R-indentation-RRR ()
  (ess-test-R-indentation "styles/RRR.R" 'RRR))

(ert-deftest test-ess-R-indentation-RRR+ ()
  (ess-test-R-indentation "styles/RRR+.R" 'RRR+))

(ert-deftest test-ess-R-indentation-RStudio- ()
  (ess-test-R-indentation "styles/RStudio-.R" 'RStudio-))

(ert-deftest test-ess-R-indentation-C++ ()
  (ess-test-R-indentation "styles/C++.R" 'C++))

(ert-deftest test-ess-R-indentation-misc1 ()
  (ess-test-R-indentation "styles/misc1.R" 'misc1))
