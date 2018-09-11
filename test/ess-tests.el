
(require 'ert)

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
  (should (string= 'ess-mode (with-temp-buffer (R-mode) major-mode))))

(ert-deftest STA-mode ()
  (should (string= 'ess-mode (with-temp-buffer (STA-mode) major-mode))))

(ert-deftest S-mode ()
  (should (string= 'ess-mode (with-temp-buffer (S-mode) major-mode))))

(ert-deftest ess-jags-mode ()
  (should (string= 'ess-jags-mode (with-temp-buffer (ess-jags-mode) major-mode))))

(ert-deftest ess-bugs-mode ()
  (should (string= 'ess-bugs-mode (with-temp-buffer (ess-bugs-mode) major-mode))))

(ert-deftest ess-julia-mode ()
  (should (string= 'ess-julia-mode (with-temp-buffer (ess-julia-mode) major-mode))))

(ert-deftest sas-mode ()
  (should (string= 'ess-mode (with-temp-buffer (SAS-mode) major-mode))))
