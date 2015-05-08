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
;;         (ess-set-style 'test-misc1))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ert)


;;; Indentation tests

(defvar ess-test-style-alist
  ;; New test-RRR style containing additional settings.
  ;; Compared to RRR, it makes sure personal configuration does not
  ;; interfere with tests
  `(,(assq 'RRR ess-style-alist)
    (misc1
     (ess-indent-level . 6)
     (ess-first-continued-statement-offset . 3)
     (ess-continued-statement-offset . 0)
     (ess-brace-offset . -6)
     (ess-arg-function-offset . 0)
     (ess-arg-function-offset-new-line . 6)
     (ess-expression-offset . 0)
     (ess-else-offset . 6)
     (ess-close-brace-offset . 6)
     (ess-brace-imaginary-offset . 3)
     (ess-continued-brace-offset . 3)
     (ess-close-paren-offset . '(3)))))

(defun ess-test-R-indentation (file style)
  (let ((ess-style-alist (append ess-test-style-alist ess-style-alist)))
    (with-temp-buffer
      (insert-file-contents-literally file t)
      (let ((expected (buffer-string)))
        (R-mode)
        (setq ess-fancy-comments t)
        (ess-set-style style)
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) expected))))))

(ert-deftest test-ess-R-indentation-RRR ()
  (ess-test-R-indentation "styles/RRR.R" 'RRR))

(ert-deftest test-ess-R-indentation-misc1 ()
  (ess-test-R-indentation "styles/misc1.R" 'misc1))
