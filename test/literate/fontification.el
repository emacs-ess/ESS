;; -*- lexical-binding: t; -*-
;; (defun face-at-point ()
;;   (get-char-property (point) 'face))

(defmacro with-ess-toggled-font-lock-keyword (enable keywords &rest body)
  (declare (indent 2)
           (debug (&rest form)))
  `(progn
     (let* ((enable ,enable)
            (keywords ,keywords)
            (keywords (if (listp keywords)
                          keywords
                        (list keywords)))
            toggled)
       (mapc (lambda (kw)
               (if (not (eq enable (cdr (assq kw ess-R-font-lock-keywords))))
                   (progn
                     (ess-font-lock-toggle-keyword kw)
                     (push kw toggled))))
             keywords)
       ,@body
       (mapc #'ess-font-lock-toggle-keyword
             toggled))))

(defmacro with-ess-disabled-font-lock-keyword (keywords &rest body)
  (declare (indent 1)
           (debug (&rest form)))
  `(with-ess-toggled-font-lock-keyword nil ,keywords ,@body))

(defmacro with-ess-enabled-font-lock-keyword (keywords &rest body)
  (declare (indent 1)
           (debug (&rest form)))
  `(with-ess-toggled-font-lock-keyword t ,keywords ,@body))
