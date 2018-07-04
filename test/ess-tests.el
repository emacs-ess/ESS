
(require 'ert)

(unless (fboundp 'provided-mode-derived-p)
  ;; From dev Emacs
  (defun provided-mode-derived-p (mode &rest modes)
    (while (and (not (memq mode modes))
                (setq mode (get mode 'derived-mode-parent))))
    mode))

(ert-deftest ess-mode-inherits-prog-mode ()
  (should (unless nil (provided-mode-derived-p 'ess-mode 'prog-mode))))
