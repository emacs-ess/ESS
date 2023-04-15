
##### Jumping/Climbing tokens

### 1 Strings and backquoted names -----------------------------------

¶`a"a"a` "a`a`a"

##! (should (ess-test-token= "identifier" "`a\"a\"a`"))

`a"a"a`¶ "a`a`a"

##> (should (ess-test-token= "string" "\"a`a`a\""))

`a"a"a` "a`a`a"¶


### 2 Identifiers ----------------------------------------------------

¶.a_a a10

##! (should (ess-test-token= "identifier" ".a_a"))

.a_a¶ a10

##> (should (ess-test-token= "identifier" "a10"))

.a_a a10¶


### 3 Numbers --------------------------------------------------------

¶100 1E10 1e10 1.10

##! (should (ess-test-token= "number" "100"))

100¶ 1E10 1e10 1.10

##> (should (ess-test-token= "number" "1E10"))

100 1E10¶ 1e10 1.10

##> (should (ess-test-token= "number" "1e10"))

100 1E10 1e10¶ 1.10

##> (should (ess-test-token= "number" "1.10"))

100 1E10 1e10 1.10¶


### 4 Delimiters -----------------------------------------------------

¶() a[[[]]] {}

##! (should (ess-test-token= "("))

(¶) a[[[]]] {}

##> (should (ess-test-token= ")"))

()¶ a[[[]]] {}

##> (ess-jump-token)
##> (should (ess-test-token= "[["))

() a[[¶[]]] {}

##> (should (ess-jump-token "["))
##> (should (ess-token-before= "[["))

() a[[[¶]]] {}

##> (should (ess-test-token= "]]"))

() a[[[]]¶] {}

##> (should (ess-jump-token "]"))
##> (should (ess-token-before= "]]"))

() a[[[]]]¶ {}

##> (should (ess-test-token= "{"))

() a[[[]]] {¶}

##> (should (ess-test-token= "}"))

() a[[[]]] {}¶


### 5 Buffer boundaries

¶

##! (should (ess-token-before= "buffer-start"))
##! (should (ess-token-after= "buffer-end"))

¶


### 6 Punctuation

¶.; .,

##! (ess-jump-token)
##! (should (ess-test-token= ";"))

.;¶ .,

##> (ess-jump-token)
##> (should (ess-test-token= ","))

.; .,¶


### 7 Keywords

¶if if_else else function while for

##! (should (ess-test-token= "if"))
##! (should (ess-test-token= "identifier" "if_else"))
##! (should (ess-test-token= "else"))
##! (should (ess-test-token= "function"))
##! (should (ess-test-token= "while"))
##! (should (ess-test-token= "for"))

if if_else else function while for¶



##### Jumping/Climbing operator tokens

### 1 Logical operators ----------------------------------------------

¶a & a && a &&& a | a || a ||| a

##! (ess-jump-token)
##! (should (ess-test-token= "&"))

a &¶ a && a &&& a | a || a ||| a

##> (ess-jump-token)
##> (should (ess-test-token= "&&"))

a & a &&¶ a &&& a | a || a ||| a

##> (ess-jump-token)
##> (should (ess-test-token= "&&"))

a & a && a &&¶& a | a || a ||| a

##> (should (ess-jump-token "&"))
##> (should (ess-token-before= "&&"))

a & a && a &&&¶ a | a || a ||| a

##> (ess-jump-token)
##> (should (ess-test-token= "|"))

a & a && a &&& a |¶ a || a ||| a

##> (ess-jump-token)
##> (should (ess-test-token= "||"))

a & a && a &&& a | a ||¶ a ||| a

##> (ess-jump-token)
##> (should (ess-test-token= "||"))

a & a && a &&& a | a || a ||¶| a

##> (should (ess-jump-token "|"))
##> (should (ess-token-before= "||"))

a & a && a &&& a | a || a |||¶ a


### 2 Equality / equal operators -------------------------------------

¶a = a := a == a === a :== a != a :!= a

##! (ess-jump-token)
##! (should (ess-test-token= "="))

a =¶ a := a == a === a :== a != a :!= a

##> (ess-jump-token)
##> (should (ess-test-token= ":="))

a = a :=¶ a == a === a :== a != a :!= a

##> (ess-jump-token)
##> (should (ess-test-token= "=="))

a = a := a ==¶ a === a :== a != a :!= a

##> (ess-jump-token)
##> (should (ess-test-token= "=="))

a = a := a == a ==¶= a :== a != a :!= a

##> (should (ess-jump-token "="))
##> (should (ess-token-before= "=="))

a = a := a == a ===¶ a :== a != a :!= a

##> (ess-jump-token)
##> (should (ess-test-token= ":="))

a = a := a == a === a :=¶= a != a :!= a

##> (should (ess-jump-token "="))
##> (should (ess-token-before= "=="))

a = a := a == a === a :==¶ a != a :!= a

##> (ess-jump-token)
##> (should (ess-test-token= "!="))

a = a := a == a === a :== a !=¶ a :!= a

##> (ess-jump-token)
##> (should (ess-test-token= ":"))

a = a := a == a === a :== a != a :¶!= a

##> (should (ess-test-token= "!="))

a = a := a == a === a :== a != a :!=¶ a


### 3 %% operators ---------------------------------------------------

¶a %>% a %a`a`a"a"a$a@a% a %% a %%% a % a

##! (ess-jump-token)
##! (should (ess-test-token= "%infix%" "%>%"))

a %>%¶ a %a`a`a"a"a$a@a% a %% a %%% a % a

##> (ess-jump-token)
##> (should (ess-test-token= "%infix%" "%a`a`a\"a\"a$a@a%"))

a %>% a %a`a`a"a"a$a@a%¶ a %% a %%% a % a

##> (ess-jump-token)
##> (should (ess-test-token= "%%"))

a %>% a %a`a`a"a"a$a@a% a %%¶ a %%% a % a

##> (ess-jump-token)
##> (should (ess-test-token= "%%"))

a %>% a %a`a`a"a"a$a@a% a %% a %%¶% a % a

##> (should (ess-test-token= "%infix%" "% a %"))

a %>% a %a`a`a"a"a$a@a% a %% a %%% a %¶ a


### 4 Arithmetic operators -------------------------------------------

¶a + a - a - -a * a ** a ^ a ^ ++a

##! (ess-jump-token)
##! (should (ess-test-token= "+"))

a +¶ a - a - -a * a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (ess-test-token= "-"))

a + a -¶ a - -a * a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (ess-test-token= "-"))

a + a - a -¶ -a * a ** a ^ a ^ ++a

##> (should (ess-test-token= "-"))

a + a - a - -¶a * a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (ess-test-token= "*"))

a + a - a - -a *¶ a ** a ^ a ^ ++a

##> (ess-jump-token)
##> (should (ess-test-token= "**"))

a + a - a - -a * a **¶ a ^ a ^ ++a

##> (ess-jump-token)
##> (should (ess-test-token= "^"))

a + a - a - -a * a ** a ^¶ a ^ ++a

##> (ess-jump-token)
##> (should (ess-test-token= "^"))

a + a - a - -a * a ** a ^ a ^¶ ++a

##> (should (ess-test-token= "+"))

a + a - a - -a * a ** a ^ a ^ +¶+a

##> (should (ess-test-token= "+"))

a + a - a - -a * a ** a ^ a ^ ++¶a


### 5 : operators ----------------------------------------------------

¶a:  a::  a:::  a::::  a:::=

##! (ess-jump-token)
##! (should (ess-test-token= ":"))

a:¶  a::  a:::  a::::  a:::=

##> (ess-jump-token)
##> (should (ess-test-token= "::"))

a:  a::¶  a:::  a::::  a:::=

##> (ess-jump-token)
##> (should (ess-test-token= ":::"))

a:  a::  a:::¶  a::::  a:::=

##> (ess-jump-token)
##> (should (ess-test-token= ":::"))

a:  a::  a:::  a:::¶:  a:::=

##> (should (ess-jump-token ":"))
##> (should (ess-token-before= ":::"))

a:  a::  a:::  a::::¶  a:::=

##> (ess-jump-token)
##> (should (ess-test-token= ":::"))

a:  a::  a:::  a::::  a:::¶=


### 6 Assignment operators -------------------------------------------

¶a <-  a <<-  a -> >  a ->> a >> a

##! (ess-jump-token)
##! (should (ess-test-token= "<-"))

a <-¶  a <<-  a -> >  a ->> a >> a

##> (ess-jump-token)
##> (should (ess-test-token= "<<-"))

a <-  a <<-¶  a -> >  a ->> a >> a

##> (ess-jump-token)
##> (should (ess-test-token= "->"))

a <-  a <<-  a ->¶ >  a ->> a >> a

##> (should (ess-test-token= ">"))

a <-  a <<-  a -> >¶  a ->> a >> a

##> (ess-jump-token)
##> (should (ess-test-token= "->>"))

a <-  a <<-  a -> >  a ->>¶ a >> a

##> (ess-jump-token)
##> (should (ess-test-token= ">"))
##> (should (ess-test-token= ">"))

a <-  a <<-  a -> >  a ->> a >>¶ a


### 7 Inequality operators -------------------------------------------

¶a < >  a >=  a > =  a <=

##! (ess-jump-token)
##! (should (ess-test-token= "<"))

a <¶ >  a >=  a > =  a <=

##> (should (ess-test-token= ">"))

a < >¶  a >=  a > =  a <=

##> (ess-jump-token)
##> (should (ess-test-token= ">="))

a < >  a >=¶  a > =  a <=

##> (ess-jump-token)
##> (should (ess-test-token= ">"))

a < >  a >=  a >¶ =  a <=

##> (should (ess-test-token= "="))

a < >  a >=  a > =¶  a <=

##> (ess-jump-token)
##> (should (ess-test-token= "<="))

a < >  a >=  a > =  a <=¶


### 8 Special operators ----------------------------------------------

¶~a~~a

##! (should (ess-test-token= "~"))

~¶a~~a

##> (ess-jump-token)
##> (should (ess-test-token= "~"))
##> (should (ess-test-token= "~"))

~a~~¶a



##### Token refinement

### 1 Parameter assignment -------------------------------------------

call(param ¶= NULL)

##! (should (ess-test-token= "="))
##! (should (ess-refined-token= (ess-token-before) "param-assign"))

call(param =¶ NULL)


### 2 Quoted parameter names -----------------------------------------

call(¶"param" = NULL)

##! (should (ess-test-token= "string" "\"param\""))
##! (should (ess-refined-token= (ess-token-before) "identifier"))

call("param"¶ = NULL)


### 2 Quoted call names ----------------------------------------------

¶"call"()

##! (should (ess-test-token= "string" "\"call\""))
##! (should (ess-refined-token= (ess-token-before) "identifier"))

"call"¶()



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


### 4 Form Feed character --------------------------------------------

text


¶text

##! (ess-skip-blanks-backward t)

text¶


text

