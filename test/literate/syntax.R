
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
