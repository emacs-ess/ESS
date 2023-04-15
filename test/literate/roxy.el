;; -*- lexical-binding: t; -*-

(defun ess-test--faces-at-point ()
  (let ((face (get-char-property (point) 'face)))
    (if (listp face)
        face
      (list face))))
