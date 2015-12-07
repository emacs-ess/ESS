
##### Blanks, Characters and Comments

### 1 ----------------------------------------------------------------

text¶
text

##! (ess-skip-blanks-forward t)

text
¶text


### 2 ----------------------------------------------------------------

text¶ # comment

##! (when (not (ess-skip-blanks-forward t))
##!   (insert "failure"))

text ¶# comment

##> (when (ess-skip-blanks-forward t)
##>   (insert "failure"))

text ¶# comment


### 3 ----------------------------------------------------------------

text
¶text

##! (ess-skip-blanks-backward)

text
¶text

##! (ess-skip-blanks-backward t)

text¶
text



##### Statements

### 1 ----------------------------------------------------------------

(!stuff1 ¶|| stuff2)

##! (ess-climb-continuations)

(¶!stuff1 || stuff2)



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
