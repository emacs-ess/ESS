
##### Keywords

### 1 Bare function-like keywords are not fontified ------------------

¶while ¶for ¶if ¶switch ¶function ¶return ¶on.exit ¶stop
¶tryCatch ¶withRestarts ¶invokeRestart ¶recover ¶browser

##! (when (>= emacs-major-version 25)
##>   (should (not (face-at-point))))

¶while ¶for ¶if ¶switch ¶function ¶return ¶on.exit ¶stop
¶tryCatch ¶withRestarts ¶invokeRestart ¶recover ¶browser


### 1b Bare function-like weak keywords are not fontified ------------

¶message ¶warning ¶signalCondition ¶withCallingHandlers

##! (should (not (face-at-point)))

¶message ¶warning ¶signalCondition ¶withCallingHandlers


### 2 Function-like keywords are fontified ---------------------------

¶while() ¶for() ¶if() ¶function()

##! (should (eq (face-at-point) 'ess-keyword-face))
##> (ess-forward-sexp)
##> (should (not (face-at-point)))

while¶() for¶() if¶() function¶()


### 2b Function-like control flow keywords are fontified -------------

¶switch() ¶return() ¶on.exit() ¶stop() ¶tryCatch()
¶withRestarts() ¶invokeRestart() ¶recover() ¶browser()

##! (should (eq (face-at-point) 'ess-r-control-flow-keyword-face))
##> (ess-forward-sexp)
##> (should (not (face-at-point)))

switch¶() return¶() on.exit¶() stop¶() tryCatch¶()
withRestarts¶() invokeRestart¶() recover¶() browser¶()


### 2c Function-like signal keywords are fontified ---------------------

¶message() ¶warning() ¶signalCondition() ¶withCallingHandlers()

##! (should (eq (face-at-point) 'ess-r-signal-keyword-face))
##> (ess-forward-sexp)
##> (should (not (face-at-point)))

message¶() warning¶() signalCondition¶() withCallingHandlers¶()


### 3 Simple keywords are always fontified ---------------------------

¶in ¶else ¶break ¶next ¶repeat

##! (should (eq (face-at-point) 'ess-keyword-face))

¶in ¶else ¶break ¶next ¶repeat


### 4 Search list modifiers are not fontified if not in function position

¶library ¶attach ¶detach ¶source ¶require

##! (when (>= emacs-major-version 25)
##>   (should (not (face-at-point))))

¶library ¶attach ¶detach ¶source ¶require


### 5 Search list modifiers are fontified if in function position ----

¶library() ¶attach() ¶detach() ¶source() ¶require()

##! (should (eq (face-at-point) 'ess-modifiers-face))
##> (forward-word)
##> (should (not (face-at-point)))

library¶() attach¶() detach¶() source¶() require¶()


### 6 Assignment operators are fontified -----------------------------

foo¶ <- foo¶ <<- foo¶ -> foo¶ ->> foo

##! (should (not (face-at-point)))
##> (forward-char)

foo ¶<- foo ¶<<- foo ¶-> foo ¶->> foo

##> (should (eq (face-at-point) 'ess-assignment-face))
##> (skip-syntax-forward ".")

foo <-¶ foo <<-¶ foo ->¶ foo ->>¶ foo

##> (should (not (face-at-point)))

foo <-¶ foo <<-¶ foo ->¶ foo ->>¶ foo


### 7 Constants are fontified ----------------------------------------

¶TRUE ¶FALSE ¶NA ¶NULL ¶Inf ¶NaN
¶NA_integer_ ¶NA_real_ ¶NA_complex_ ¶NA_character_

##! (should (eq (face-at-point) 'ess-constant-face))

¶TRUE ¶FALSE ¶NA ¶NULL ¶Inf ¶NaN
¶NA_integer_ ¶NA_real_ ¶NA_complex_ ¶NA_character_


### 8 Can modify keywords --------------------------------------------

¶foobar foobaz()

##! (let ((ess-R-keywords (append '("foobaz") ess-R-keywords)))
##>   (ess-r-mode)
##>   (font-lock-ensure))
##> (should (not (face-at-point)))
##> (forward-word)
##> (forward-char)

foobar ¶foobaz()

##> (should (eq (face-at-point) 'ess-keyword-face))

foobar ¶foobaz()


### 9 Can set keywords variable to nil -------------------------------

¶stop()

##! (let (ess-R-control-flow-keywords)
##>   (ess-r-mode)
##>   (font-lock-ensure))
##> (should (not (face-at-point)))

¶stop()

