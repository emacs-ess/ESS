
##### Code Filling

### 1 ----------------------------------------------------------------

fun_call(¶argument1, argument2, argument3, argument4,
         argument5)

##! (setq-local fill-column 40)
##! "M-q"

fun_call(¶argument1, argument2,
         argument3, argument4,
         argument5)

##> "M-q"

fun_call(¶argument1,
         argument2,
         argument3,
         argument4,
         argument5)

##> "M-q"

fun_call(¶argument1, argument2, argument3, argument4,
         argument5)

##> "M-q"

fun_call(¶argument1, argument2,
         argument3, argument4,
         argument5)


### 2 ----------------------------------------------------------------

fun¶_call(argument1, argument2, argument3, argument4,
         argument5)

##! (setq-local fill-column 40)
##! "M-q"

fun¶_call(argument1, argument2,
         argument3, argument4,
         argument5)


### 3 ----------------------------------------------------------------

fun_call(¶         ## comment
    argument1,     ## comment
    argument2)

##! "M-q"

fun_call(¶         ## comment
    argument1,     ## comment
    argument2)


### 4 ----------------------------------------------------------------

¶fun_call(parameter =
            "string")

##! "M-q"

¶fun_call(parameter = "string")


### 5 ----------------------------------------------------------------

`fun_call`(¶argument1, argument2)

##! "M-q"
##! "M-q"

`fun_call`(¶argument1,
           argument2)


### 6 Empty arguments ------------------------------------------------

fun_call¶(argument1, , arg2, , argument3, , argument4)

##! (setq-local fill-column 42)
##! "M-q"

fun_call¶(argument1, , arg2, , argument3, ,
         argument4)

##> "M-q"

fun_call¶(argument1,
        ,
         arg2,
        ,
         argument3,
        ,
         argument4)



##### Continuation Filling

### 1 ----------------------------------------------------------------

lm(outcome¶ ~ pred1 +
       pred2 +
       pred3 +
       pred4,
   data)

##! "M-q"

lm(outcome¶ ~ pred1 + pred2 + pred3 + pred4,
   data)

##> "M-q"

lm(outcome¶ ~
       pred1 +
       pred2 +
       pred3 +
       pred4,
   data)

##> "M-q"

lm(outcome¶ ~ pred1 +
       pred2 +
       pred3 +
       pred4,
   data)


### 2 ----------------------------------------------------------------

fun_call(¶argument +)

##! "M-q"

fun_call(¶argument +)

