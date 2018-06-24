
;; FIXME Emacs 25
(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-default-fontify-buffer))

(defun face-at-point ()
  (get-char-property (point) 'face))
