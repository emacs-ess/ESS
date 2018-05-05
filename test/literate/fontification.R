
##### Keywords

### 1 Bare function-like keywords are not fontified ------------------

¶while for if switch function return message warning stop

##! (while (not (eolp))
##>   (should (not (face-at-point)))
##>   (forward-word)
##>   (ignore-errors (forward-char)))

while for if switch function return message warning stop¶


### 2 Function-like keywords are fontified ---------------------------

¶while() for() if() switch() function() return() message() warning() stop()

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-keyword-face))
##>   (forward-word)
##>   (ignore-errors (forward-char 3)))

while() for() if() switch() function() return() message() warning() stop()¶


### 3 Simple keywords are always fontified ---------------------------

¶in else break next

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-keyword-face))
##>   (forward-word)
##>   (ignore-errors (forward-char)))

in else break next¶


### 4 Search list modifiers are not fontified if not in function position

¶library attach detach source require

##! (while (not (eolp))
##>   (should (not (face-at-point)))
##>   (forward-word)
##>   (ignore-errors (forward-char)))

library attach detach source require¶


### 5 Search list modifiers are fontified if in function position ----

¶library() attach() detach() source() require()

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-modifiers-face))
##>   (forward-word)
##>   (ignore-errors (forward-char 3)))

library() attach() detach() source() require()¶

