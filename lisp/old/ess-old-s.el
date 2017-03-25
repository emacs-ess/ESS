
(defvar ess-function-outline-file
  (concat ess-etc-directory  "/function-outline.S")
  "The file name of the ess-function outline that is to be inserted at point,
when \\[ess-insert-function-outline] is used.
Placeholders (substituted `at runtime'): $A$ for `Author', $D$ for `Date'.")

;; Use the user's own ~/S/emacs-fun.outline  if (s)he has one : ---
(let ((outline-file (concat (getenv "HOME") "/S/function-outline.S")))
  (if (file-exists-p outline-file)
      (setq ess-function-outline-file outline-file)))

(defun ess-insert-function-outline ()
  "Insert an S function definition `outline' at point.
Uses the file given by the variable `ess-function-outline-file'."
  (interactive)
  (let ((oldpos (point)))
    (save-excursion
      (insert-file-contents ess-function-outline-file)
      (if (search-forward "$A$" nil t)
          (replace-match (user-full-name) 'not-upcase 'literal))
      (goto-char oldpos)
      (if (search-forward "$D$" nil t)
          (replace-match (ess-time-string 'clock) 'not-upcase 'literal)))
    (goto-char (1+ oldpos))))
