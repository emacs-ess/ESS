

### Function declarations

## 1
fun <- function(argument1,
                argument2) {
    body
}

## 2
function(
    argument1,
    argument2
    )
    {
        body
    }

## 3
function(argument_fun(sub_argument1,
                      sub_argument2),
         argument) {}

## 4
function(argument1, parameter = fun_call(
                        sub_argument),
         argument2) {}



### Function calls

## 1
fun_call(argument1,
         argument2)

## 2
fun_call(
    argument1,
    argument2
    )

## 3
fun_call(parameter = (
    stuff
    ),
         argument)

## 4
fun_call(parameter = fun_argument(
             stuff
    ),
         argument)



### Blocks

## 1
function()
    {
        body
    }

## 2
fun_call({
    stuff1
},
         {
             stuff2
         }
         )

## 3
fun_call({
    stuff1
}, {
    stuff2
})

## 4
fun_call(
    parameter1 = {
        stuff1
    },
    parameter2 = {
        stuff2
    }
    )

## 5
fun_call(parameter1 = {
             stuff1
         },
         {
             stuff2
         }, parameter2 = {
             stuff3
         }, {
             stuff4
         },
         parameter3 =
             stuff5 ~
                 stuff6 +
                     stuff7)

## 6
fun <- fun_call({
    stuff1
}, {
    stuff2
},
                {
                    stuff3
                }
                )

## 7
fun_call(function(x) {
             body1
         },
         function(x) {
             body2
         })

## 8
object <-
    fun_call({
        body
    })

## 9
object <-
    fun_call(     {
                 body
             }
             )

## 10
{
    {
        stuff
    }
}

## 11
{{
     stuff
 }
}

## 12
({
    stuff
})

## 13
( {
     stuff
 }
 )

## 14
object[
       argument1,
       argument2
       ]

## 15
{
    object[
           fun_call(
               body
               ),
           argument[
                    (
                        sub_argument
                        )
                    ]
           ]
}



### Control flow

## 1
if (condition) {
    stuff1
} else {
    stuff2
}

## 2
if (condition) {
    stuff1
}
else {
    stuff2
}

## 3
if (condition)
    {
        stuff1
    } else
        {
            stuff2
        }

## 4
if (condition)
    {
        stuff1
    }
else
    {
        stuff2
    }

## 5
for (sequence)
    {
        stuff
    }

## 6
for (sequence) {
    stuff
}

## 7
if (condition)
    stuff

## 8
for (sequence)
    stuff



### Continuation lines

## 1
stuff1 %>%
    stuff2 %>%
        stuff3

## 2
{
    stuff1 %>%
        stuff2 %>%
            stuff3
} %>%
    stuff4 %>%
        stuff5

## 3
(
    stuff1 %>%
        stuff2 %>%
            stuff3
    ) %>%
    stuff4 %>%
        stuff5

## 4
[
 stuff1 %>%
     stuff2 %>%
         stuff3
 ] %>%
     stuff4 %>%
         stuff5

## 5
stuff1 %>%
    stuff2 %>%
        if (condition) {
            stuff3 %>%
                stuff4 %>%
                    stuff5
        } else {
            stuff6 %>%
                stuff7 %>%
                    for (sequence) {
                        stuff8
                    } %>%
                        stuff9 %>%
                            stuff10
        } %>%
            stuff11 %>%
                stuff12

## 6
stuff[stuff1 %>%
          stuff2 %>%
              stuff3] %>%
                  stuff4 %>%
                      stuff5

## 7
ggplot() +
    geom(lhs -
             rhs
         ) +
        geom()

## 8
{
    ggplot() +
        geom1(argument1,
              argument2 = (
                  stuff
                  )) +
            geom2()
}

## 9
stuff +
    fun_call(parameter = argument1,
             fun_call((lhs - rhs
                       argument2
                       ) /
                          argument3)
             )



### Comments

## 1
                                        # Side comment

## 2
{
    ## Hanging comment 1
    fun_call(
        {
            ## Hanging comment 2
        }
        )
}

## 3
{
### Section comment
}


### Logical operators

## 1
stuff1 &&
    stuff2 ||
        stuff3

## 2
(stuff1 &&
     stuff2 ||
         stuff3)

## 3
if (condition1 &&
    condition2 ||
    (condition3 && condition4) ||
    (condition5 &&
         condition6 &&
             condition7) ||
    condition8) {
    stuff
} && condition8 ||
    condition9 ||
        condition10
