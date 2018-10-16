
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
¶.Defunct()

##! (should (eq (face-at-point) 'ess-keyword-face))
##> (ess-forward-sexp)
##> (should (not (face-at-point)))

switch¶() return¶() on.exit¶() stop¶() tryCatch¶()
withRestarts¶() invokeRestart¶() recover¶() browser¶()
.Defunct¶()


### 2c Function-like signal keywords are fontified ---------------------

¶message() ¶warning() ¶signalCondition() ¶withCallingHandlers()
¶.Deprecated()

##! (should (eq (face-at-point) 'ess-modifiers-face))
##> (ess-forward-sexp)
##> (should (not (face-at-point)))

message¶() warning¶() signalCondition¶() withCallingHandlers¶()
.Deprecated¶()


### 3a Simple keywords are always fontified --------------------------

¶else ¶break ¶next ¶repeat

##! (should (eq (face-at-point) 'ess-keyword-face))

¶else ¶break ¶next ¶repeat


### 3b `in` is fontified inside `for ()` -----------------------------

for (foo ¶in bar) {}

##! (should (eq (face-at-point) 'ess-keyword-face))

for (foo ¶in bar) {}


### 3c `in` is not fontified outside `for ()` ------------------------

for foo ¶in bar {}

##! (should (not (face-at-point)))

for foo ¶in bar {}


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

##! (let (ess-R-keywords)
##>   (ess-r-mode)
##>   (font-lock-ensure))
##> (should (not (face-at-point)))

¶stop()


### 10 Can remove bare keywords from `ess-R-keywords' ----------------

¶for (foo ¶in bar) NULL

##! (let ((ess-R-keywords '("in")))
##!   (ess-r-mode)
##!   (font-lock-ensure))
##! (if (looking-at "for")
##!     (should (not (face-at-point)))
##!   (should (eq (face-at-point) 'ess-keyword-face)))

¶for (foo ¶in bar) NULL


### 11 Can disable backquoted function definition fontification ------

¶`[.foo` <- function(...) NULL
¶"[.foo" <- function(...) NULL

##! (should (eq (face-at-point) 'font-lock-function-name-face))

¶`[.foo` <- function(...) NULL
¶"[.foo" <- function(...) NULL

##! (with-ess-disabled-font-lock-keyword 'ess-R-fl-keyword:fun-defs
##!   (font-lock-ensure)
##!   (if (looking-at "\"")
##!       (should (eq (face-at-point) 'font-lock-string-face))
##!     (should (eq (face-at-point) 'default)))))

¶`[.foo` <- function(...) NULL
¶"[.foo" <- function(...) NULL


### 12 Can disable backquoted function call fontification ------------

¶`fun`()

##! (should (eq (face-at-point) 'default))

¶`fun`()

##! (with-ess-enabled-font-lock-keyword 'ess-fl-keyword:fun-calls
##!   (font-lock-ensure)
##!   (should (eq (face-at-point) 'ess-function-call-face))))

¶`fun`()


### 13 Can disable special-op fontification --------------------------

foo ¶%>% bar()

##! (should (eq (face-at-point) 'ess-%op%-face))

foo ¶%>% bar()

##! (with-ess-disabled-font-lock-keyword '(ess-fl-keyword:operators
##!                                        ess-R-fl-keyword:%op%)
##!   (font-lock-ensure)
##!   (should (eq (face-at-point) 'default))
##!   (forward-char)
##!   (should (eq (face-at-point) 'default)))

foo %¶>% bar()

##! (with-ess-disabled-font-lock-keyword 'ess-R-fl-keyword:%op%
##!   (font-lock-ensure)
##!   (should (eq (face-at-point) 'default)))

foo ¶%>% bar()


### 14 Modulo operator is fontified independently from special ops ---

foo ¶%% bar

##! (with-ess-disabled-font-lock-keyword 'ess-R-fl-keyword:%op%
##!   (with-ess-enabled-font-lock-keyword 'ess-fl-keyword:operators
##!     (font-lock-ensure)
##!     (should (eq (face-at-point) 'ess-operator-face))
##!     (forward-char)
##!     (should (eq (face-at-point) 'ess-operator-face))))

foo %¶% bar

##! (with-ess-enabled-font-lock-keyword '(ess-R-fl-keyword:%op%
##!                                       ess-fl-keyword:operators)
##!   (font-lock-ensure)
##!     (should (eq (face-at-point) 'ess-operator-face))
##!     (forward-char)
##!     (should (eq (face-at-point) 'ess-operator-face)))

foo %¶% bar

##! (with-ess-disabled-font-lock-keyword '(ess-R-fl-keyword:%op%
##!                                        ess-fl-keyword:operators)
##!   (font-lock-ensure)
##!   (should (not (face-at-point)))
##!   (forward-char)
##!   (should (not (face-at-point))))

foo %¶% bar


### 14 Backticked names are fontified with default face --------------

`¶repeat`
`¶%>%`
`¶-`

##! (with-ess-enabled-font-lock-keyword '(ess-R-fl-keyword:%op%
##!                                       ess-fl-keyword:operators)
##!   (font-lock-ensure)
##!   (should (eq (face-at-point) 'default)))

`¶repeat`
`¶%>%`
`¶-`

