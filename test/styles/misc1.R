

### Function declarations

## 1
{
      fun <- function(argument1,
                   argument2) {
            body
      }
}

## 2
{
      function(
               argument1,
               argument2
               )
      {
            body
      }
}

## 3
function(argument_fun(sub_argument1,
               sub_argument2),
      argument) {}

## 4
function(argument1, parameter = fun_call(
                                         sub_argument),
      argument2) {}

## 5
function()

      function() body



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
                                  argument1
                                  ),
      argument2)

## 5
fun_call(parameter = fun_argument(argument1,
                           argument2
                     )
     ,
      argument3)

## 6
`fun_call`(argument1,
      argument2)

## 7
`fun_call`(
      argument1,
      argument2
      )

## 8
fun_call(argument1
    , argument2
    , argument3,
      argument4, (
                       stuff1
                 ),
      argument5, (
                       stuff2
                 )
     ,
      argument6
)

## 9
fun_call(parameter =
            fun_argument(
                         sub_argument
                         ),
      argument
)

## 10
fun_call(parameter = fun_argument(
                                  sub_argument
                                  ),
      argument
)

## 11
{
      fun_call1(
                fun_call2 (argument1, argument2,
                      parameter = fun_call3(
                                            argument3,
                                            argument4
                                            ), function(x) {
                                                                 body
                                                           },
                      argument5,
                      fun_call3(
                                argument6
                                ),
                      argument7
                ), {
                         stuff
                   },
                argument8
                )
}



### Blocks

## 1
{
      function()
      {
            body
      }
}

## 2
{
      fun_call({
                     stuff1
               },
            {
                  stuff2
            }
      )
}

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
         stuff7,
      argument)

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
fun <- fun_call({
                      stuff
                },
             argument
       )

## 8
fun_call(function(x) {
                           body1
                     },
      function(x) {
                        body2
                  })

## 9
object <-
      fun_call({
                     stuff
               }, {
                        stuff
                  })

## 10
object <-
      fun_call(     {
                          body
                    }
      )

## 11
{
                    {
                          stuff
                    }
}

## 12
{{
      stuff
}
}

## 13
({
      stuff
})

## 14
( {
      stuff
}
)

## 15
object[
       argument1,
       argument2
       ]

## 16
object[argument1,
      argument2
]

## 17
object[(
             argument1
       )]

## 18
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

## 19
{
      object[
             argument1,
             argument2,
             by = argument3
             ][
               argument4,
               fun_call1(argument1,
                     argument2)
               argument5
               ][
                 argument6,
                 fun_call2(
                           argument1,
                           argument2
                           )
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
object[
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
                                 stuff1
                           ) -
                              stuff2) +
         geom2() +
         geom3()
}

## 9
stuff +
   fun_call(parameter = argument1,
         fun_call((stuff1 - stuff2 +
                      stuff3
                  ) /
                     stuff4)
   )

## 10
fun_call(argument1 %>%
            stuff1, argument2 %>%
                    stuff2, {
                                  stuff3 %>%
                                     stuff4
                            } %>%
                            stuff5,
      argument3
)

## 11
object1 <- object2 %>%
   fun_call1() %>%
   fun_call2()

## 12
object1 <-
      object2%>%fun_call1() %>%
      fun_call2() %>%
      fun_call3()



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
