
(defun token= (string)
  "Check that the next token is equal to STRING.
This checks it back and forth and moves the point after the
token."
  (and (next-token= string)
       (token-before= string)))

(defun next-token= (string)
  "Check that the next token is equal to STRING.
Moves the point after the token."
  (string= (ess-token-string (ess-jump-token)) string))

(defun token-before= (string)
  "Check that the previous token is equal to STRING.
Does not move the point"
  (string= (ess-token-string (ess-token-before)) string))
