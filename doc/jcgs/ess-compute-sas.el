(ess-sas-global-pc-keys)
(setq-default ess-sas-submit-method 'iESS)
(defun ess-revert-wisely ()
  "Revert from disk if file and buffer last modification times are different."
  (interactive)
  (revert-buffer t t))
