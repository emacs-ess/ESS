
(unless (fboundp 'font-lock-ensure)
  (defalias 'font-lock-ensure 'font-lock-default-fontify-buffer))

(defun face-at-point ()
  (get-char-property (point) 'face))

(defmacro with-ess-toggled-font-lock-keyword (keyword &rest body)
  (declare (indent 1)
           (debug (&rest form)))
  `(progn
     (ess-font-lock-toggle-keyword ,keyword)
     ,@body
     (ess-font-lock-toggle-keyword ,keyword)))
