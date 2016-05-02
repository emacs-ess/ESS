
(defun token= (type &optional value)
  "Check that the next token conforms to TYPE and VALUE.
This checks it back and forth and moves the point after the
token."
  (and (ess-jump-token type value)
       (ess-token-before= type value)))
