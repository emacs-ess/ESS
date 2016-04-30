
##### Jumping/Climbing tokens

### 1 Strings and backquoted names -----------------------------------

¶`a"a"a` "a`a`a"

##! (should (token= "`a\"a\"a`"))

`a"a"a`¶ "a`a`a"

##> (should (token= "\"a`a`a\""))

`a"a"a` "a`a`a"¶


### 2 Identifiers ----------------------------------------------------

¶.a_a a10

##! (should (token= ".a_a"))

.a_a¶ a10

##> (should (token= "a10"))

.a_a a10¶


### 3 Numbers --------------------------------------------------------

¶100 1E10 1e10 1.10

##! (should (token= "100"))

100¶ 1E10 1e10 1.10

##> (should (token= "1E10"))

100 1E10¶ 1e10 1.10

##> (should (token= "1e10"))

100 1E10 1e10¶ 1.10

##> (should (token= "1.10"))

100 1E10 1e10 1.10¶


### 4 Delimiters -----------------------------------------------------

¶() a[[[]]] {}

##! (should (token= "("))

(¶) a[[[]]] {}

##> (should (token= ")"))

()¶ a[[[]]] {}

##> (ess-jump-token)
##> (should (token= "[["))

() a[[¶[]]] {}

##> (should (next-token= "["))
##> (should (token-before= "[["))

() a[[[¶]]] {}

##> (should (token= "]]"))

() a[[[]]¶] {}

##> (should (next-token= "]"))
##> (should (token-before= "]]"))

() a[[[]]]¶ {}

##> (should (token= "{"))

() a[[[]]] {¶}

##> (should (token= "}"))

() a[[[]]] {}¶



##### Jumping/Climbing operator tokens

### 1 Logical operators ----------------------------------------------

¶a & a && a &&& a | a || a ||| a

##! (ess-jump-token)
##! (should (token= "&"))

a &¶ a && a &&& a | a || a ||| a

##> (ess-jump-token)
##> (should (token= "&&"))

a & a &&¶ a &&& a | a || a ||| a

##> (ess-jump-token)
##> (should (token= "&&"))

a & a && a &&¶& a | a || a ||| a

##> (should (next-token= "&"))
##> (should (token-before= "&&"))

a & a && a &&&¶ a | a || a ||| a

##> (ess-jump-token)
##> (should (token= "|"))

a & a && a &&& a |¶ a || a ||| a

##> (ess-jump-token)
##> (should (token= "||"))

a & a && a &&& a | a ||¶ a ||| a

##> (ess-jump-token)
##> (should (token= "||"))

a & a && a &&& a | a || a ||¶| a

##> (should (next-token= "|"))
##> (should (token-before= "||"))

a & a && a &&& a | a || a |||¶ a


### 2 Equality / equal operators -------------------------------------

¶a = a := a == a === a :== a != a :!= a

##! (ess-jump-token)
##! (should (token= "="))

a =¶ a := a == a === a :== a != a :!= a

##> (ess-jump-token)
##> (should (token= ":="))

a = a :=¶ a == a === a :== a != a :!= a

##> (ess-jump-token)
##> (should (token= "=="))

a = a := a ==¶ a === a :== a != a :!= a

##> (ess-jump-token)
##> (should (token= "=="))

a = a := a == a ==¶= a :== a != a :!= a

##> (should (next-token= "="))
##> (should (token-before= "=="))

a = a := a == a ===¶ a :== a != a :!= a

##> (ess-jump-token)
##> (should (token= ":="))

a = a := a == a === a :=¶= a != a :!= a

##> (should (next-token= "="))
##> (should (token-before= "=="))

a = a := a == a === a :==¶ a != a :!= a

##> (ess-jump-token)
##> (should (token= "!="))

a = a := a == a === a :== a !=¶ a :!= a

##> (ess-jump-token)
##> (should (token= ":"))

a = a := a == a === a :== a != a :¶!= a

##> (should (token= "!="))

a = a := a == a === a :== a != a :!=¶ a


### 3 %% operators ---------------------------------------------------

¶a %>% a %a`a`a"a"a$a@a% a %% a %%% a % a

##! (ess-jump-token)
##! (should (token= "%>%"))

a %>%¶ a %a`a`a"a"a$a@a% a %% a %%% a % a

##> (ess-jump-token)
##> (should (token= "%a`a`a\"a\"a$a@a%"))

a %>% a %a`a`a"a"a$a@a%¶ a %% a %%% a % a

##> (ess-jump-token)
##> (should (token= "%%"))

a %>% a %a`a`a"a"a$a@a% a %%¶ a %%% a % a

##> (ess-jump-token)
##> (should (token= "%%"))

a %>% a %a`a`a"a"a$a@a% a %% a %%¶% a % a

##> (should (token= "% a %"))

a %>% a %a`a`a"a"a$a@a% a %% a %%% a %¶ a


### 4 Arithmetic operators -------------------------------------------

¶a + a - a - -a * a ** a ^ a ^ ++a

##! (ess-jump-token)
##! (should (token= "+"))

a +¶ a - a - -a * a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (token= "-"))

a + a -¶ a - -a * a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (token= "-"))

a + a - a -¶ -a * a ** a ^ a ^ ++a

##> (should (token= "-"))

a + a - a - -¶a * a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (token= "*"))

a + a - a - -a *¶ a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (token= "**"))

a + a - a - -a * a **¶ a ^ a ^ ++a

##> (ess-jump-token)
##> (should (token= "^"))

a + a - a - -a * a ** a ^¶ a ^ ++a

##> (ess-jump-token)
##> (should (token= "^"))

a + a - a - -a * a ** a ^ a ^¶ ++a

##> (should (token= "+"))

a + a - a - -a * a ** a ^ a ^ +¶+a

##> (should (token= "+"))

a + a - a - -a * a ** a ^ a ^ ++¶a


### 5 : operators ----------------------------------------------------

¶a:  a::  a:::  a::::  a:::=

##! (ess-jump-token)
##! (should (token= ":"))

a:¶  a::  a:::  a::::  a:::=

##> (ess-jump-token)
##> (should (token= "::"))

a:  a::¶  a:::  a::::  a:::=

##> (ess-jump-token)
##> (should (token= ":::"))

a:  a::  a:::¶  a::::  a:::=

##> (ess-jump-token)
##> (should (token= ":::"))

a:  a::  a:::  a:::¶:  a:::=

##> (should (next-token= ":"))
##> (should (token-before= ":::"))

a:  a::  a:::  a::::¶  a:::=

##> (ess-jump-token)
##> (should (token= ":::"))

a:  a::  a:::  a::::  a:::¶=


### 6 Assignment operators -------------------------------------------

¶a <-  a <<-  a -> >  a ->> a >> a

##! (ess-jump-token)
##! (should (token= "<-"))

a <-¶  a <<-  a -> >  a ->> a >> a

##> (ess-jump-token)
##> (should (token= "<<-"))

a <-  a <<-¶  a -> >  a ->> a >> a

##> (ess-jump-token)
##> (should (token= "->"))

a <-  a <<-  a ->¶ >  a ->> a >> a

##> (should (token= ">"))

a <-  a <<-  a -> >¶  a ->> a >> a

##> (ess-jump-token)
##> (should (token= "->>"))

a <-  a <<-  a -> >  a ->>¶ a >> a

##> (ess-jump-token)
##> (should (token= ">"))
##> (should (token= ">"))

a <-  a <<-  a -> >  a ->> a >>¶ a


### 7 Inequality operators -------------------------------------------

¶a < >  a >=  a > =  a <=

##! (ess-jump-token)
##! (should (token= "<"))

a <¶ >  a >=  a > =  a <=

##> (should (token= ">"))

a < >¶  a >=  a > =  a <=

##> (ess-jump-token)
##> (should (token= ">="))

a < >  a >=¶  a > =  a <=

##> (ess-jump-token)
##> (should (token= ">"))

a < >  a >=  a >¶ =  a <=

##> (should (token= "="))

a < >  a >=  a > =¶  a <=

##> (ess-jump-token)
##> (should (token= "<="))

a < >  a >=  a > =  a <=¶



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
