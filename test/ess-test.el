;;; ess-test.el --- ESS tests  -*- lexical-binding: t; -*-
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
;; ESS tests

(require 'ert)
(require 'ess-site)

;;; Code:

(unless (fboundp 'provided-mode-derived-p)
  ;; From dev Emacs
  (defun provided-mode-derived-p (mode &rest modes)
    (while (and (not (memq mode modes))
                (setq mode (get mode 'derived-mode-parent))))
    mode))

(ert-deftest ess-mode-inherits-prog-mode ()
  (should (unless nil (provided-mode-derived-p 'ess-mode 'prog-mode))))

;; Ensure that major modes can be invoked without errors:

(ert-deftest R-mode ()
  (should (string= 'ess-r-mode (with-temp-buffer (R-mode) major-mode))))

(ert-deftest STA-mode ()
  (should (string= 'ess-stata-mode (with-temp-buffer (STA-mode) major-mode))))

(ert-deftest ess-jags-mode ()
  (should (string= 'ess-jags-mode (with-temp-buffer (ess-jags-mode) major-mode))))

(ert-deftest ess-bugs-mode ()
  (should (string= 'ess-bugs-mode (with-temp-buffer (ess-bugs-mode) major-mode))))

(ert-deftest ess-julia-mode ()
  (should (string= 'ess-julia-mode (with-temp-buffer (ess-julia-mode) major-mode))))

(ert-deftest sas-mode ()
  (should (string= 'SAS-mode (with-temp-buffer (SAS-mode) major-mode))))

;; Various tests from e.g. ess-utils.el

(ert-deftest ess-flatten-list ()
  (should (equal (list 'a 'b 'c 'd 'e 'f 'g 'h 'i 'j)
                 (ess-flatten-list '((a . b) c (d . e) (f g h) i . j)))))

(provide 'ess-test)

;;; ess-test.el ends here
