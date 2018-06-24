
##### Keywords

### 1 Bare function-like keywords are not fontified ------------------

¶while for if switch function return on.exit stop
tryCatch withRestarts invokeRestart recover browser

##! (while (not (eolp))
##>   (when (>= emacs-major-version 25)
##>     (should (not (face-at-point))))
##>   (forward-sexp)
##>   (ignore-errors (forward-char)))

while for if switch function return on.exit stop
tryCatch withRestarts invokeRestart recover browser¶


### 1b Bare function-like weak keywords are not fontified ------------

¶message warning signalCondition withCallingHandlers

##! (while (not (eolp))
##>   (should (not (face-at-point)))
##>   (forward-sexp)
##>   (ignore-errors (forward-char)))

message warning signalCondition withCallingHandlers¶


### 2 Function-like keywords are fontified ---------------------------

¶while() for() if() function()

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-keyword-face))
##>   (ess-forward-sexp)
##>   (should (not (face-at-point)))
##>   (ignore-errors (forward-char 3)))

while() for() if() function()¶


### 2b Function-like control flow keywords are fontified -------------

¶switch() return() on.exit() stop() tryCatch()
withRestarts() invokeRestart() recover() browser()

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-r-control-flow-keyword-face))
##>   (ess-forward-sexp)
##>   (should (not (face-at-point)))
##>   (ignore-errors (forward-char 3)))

switch() return() on.exit() stop() tryCatch()
withRestarts() invokeRestart() recover() browser()¶


### 2c Function-like signal keywords are fontified ---------------------

¶message() warning() signalCondition() withCallingHandlers()

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-r-signal-keyword-face))
##>   (ess-forward-sexp)
##>   (should (not (face-at-point)))
##>   (ignore-errors (forward-char 3)))

message() warning() signalCondition() withCallingHandlers()¶


### 3 Simple keywords are always fontified ---------------------------

¶in else break next repeat

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-keyword-face))
##>   (forward-word)
##>   (ignore-errors (forward-char)))

in else break next repeat¶


### 4 Search list modifiers are not fontified if not in function position

¶library attach detach source require

##! (while (not (eolp))
##>   (when (>= emacs-major-version 25)
##>     (should (not (face-at-point))))
##>   (forward-word)
##>   (ignore-errors (forward-char)))

library attach detach source require¶


### 5 Search list modifiers are fontified if in function position ----

¶library() attach() detach() source() require()

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-modifiers-face))
##>   (forward-word)
##>   (should (not (face-at-point)))
##>   (ignore-errors (forward-char 3)))

library() attach() detach() source() require()¶


### 6 Assignment operators are fontified -----------------------------

foo¶ <- foo <<- foo -> foo ->> foo

##! (while (not (eolp))
##>   (should (not (face-at-point)))
##>   (forward-char)
##>   (should (eq (face-at-point) 'ess-assignment-face))
##>   (skip-syntax-forward ".")
##>   (should (not (face-at-point)))
##>   (ignore-errors (forward-word)))

foo <- foo <<- foo -> foo ->> foo¶


### 7 Constants are fontified ----------------------------------------

¶TRUE FALSE NA NULL Inf NaN
NA_integer_ NA_real_ NA_complex_ NA_character_

##! (while (not (eolp))
##>   (should (eq (face-at-point) 'ess-constant-face))
##>   (ignore-errors
##>     (ess-forward-sexp)
##>     (forward-char)))

TRUE FALSE NA NULL Inf NaN
NA_integer_ NA_real_ NA_complex_ NA_character_¶


### 8 Can regenerate regexps after modifying keywords ----------------

¶foobar foobaz()

##! (setq-local ess-R-keywords (append '("foobaz") ess-R-keywords))
##> (ess-r-generate-font-lock-regexps)
##> (ess-r-mode)
##> (font-lock-ensure)
##> (should (not (face-at-point)))
##> (forward-word)
##> (forward-char)
##> (should (eq (face-at-point) 'ess-keyword-face))

foobar ¶foobaz()


### 9 Can set keywords variable to nil -------------------------------

¶stop()

##! (setq-local ess-R-control-flow-keywords nil)
##> (ess-r-generate-font-lock-regexps)
##> (ess-r-mode)
##> (font-lock-ensure)
##> (should (not (face-at-point)))

¶stop()

