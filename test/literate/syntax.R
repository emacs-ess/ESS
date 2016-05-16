
##### Statements

### 1 ----------------------------------------------------------------

(!stuff1 ||¶ stuff2)

##! (ess-climb-continuations)

(¶!stuff1 || stuff2)



##### Operators

### 1a ---------------------------------------------------------------

stuff1 =, ¶stuff2

##! (ess-climb-operator)

stuff1 =, ¶stuff2


### 1b ---------------------------------------------------------------

stuff1 =; ¶stuff2

##! (ess-climb-operator)

stuff1 =; ¶stuff2


### 1c ---------------------------------------------------------------

stuff1 := ¶stuff2

##! (ess-climb-operator)

stuff1¶ := stuff2


### 1d ---------------------------------------------------------------

stuff1 %a?a:a% ¶stuff2

##! (ess-climb-operator)

stuff1¶ %a?a:a% stuff2


### 1e ---------------------------------------------------------------

stuff1 %% ¶stuff2

##! (ess-climb-operator)

stuff1¶ %% stuff2



##### Bare blocks

### 1 ----------------------------------------------------------------

function_call()
¶

##! (ess-climb-block-prefix)

function_call()
¶

##! (ess-climb-block-prefix "function")

function_call()
¶


### 2 ----------------------------------------------------------------

    ¶if (test1)
        stuff1
    if (test2)
        stuff2

##! (ess-jump-expression)

    if (test1)
        stuff1¶
    if (test2)
        stuff2

