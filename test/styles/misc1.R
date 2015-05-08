

### Function declarations

## 1
fun <- function(argument1,
                argument2) {
         body
         }

## 2
function(
      argument2,
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

## 5
fun_call(argument1,
         argument2)

## 6
fun_call(
      argument1,
      argument2
   )

## 7
fun_call(parameter = (
               stuff
   ),
         argument)

## 8
fun_call(parameter = fun_argument(
         stuff
   ),
         argument)



### Blocks

## 9
function()
{
      body
      }

## 10
fun_call(parameter1 = {
                  stuff
                  },
   {
         stuff
         }, parameter2 = {
                  stuff
                  }, {
                           stuff
                           },
         parameter3 =
            stuff1 ~
            stuff2 +
            stuff4)

## 11
fun <- fun_call({
                         stuff1
                         }, {
                                  stuff2
                                  },
          {
                stuff2
                }
   )

## 12
fun_call(function(x) {
                  body
                  },
         function(x) {
                  body
                  })

## 13
object <-
      fun_call({
                        body
                        })

## 14
object <-
      fun_call( {
                        body
                        }
         )

## 15
{
{
      stuff
      }
}

## 16
{{
          stuff
          }
 }

## 17
({
          stuff
          })

## 18
( {
          stuff
          }
   )

## 19
object[
       argument1,
       argument2
       ]

## 20
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

## 21
if (condition) {
         stuff
         } else {
                  stuff
                  }

## 22
if (condition) {
         stuff
         }
      else {
               stuff
               }

## 23
if (condition)
{
      stuff
      } else
      {
            stuff
            }

## 24
if (condition)
{
      stuff
      }
      else
      {
            stuff
            }

## 25
for (sequence)
{
      stuff
      }

## 26
for (sequence) {
         stuff
         }

## 27
if (condition)
      stuff

## 28
for (sequence)
      stuff



### Continuation lines

## 29
stuff1 %>%
   stuff2 %>%
   stuff3

## 30
{
      stuff1 %>%
         stuff2 %>%
         stuff3
      } %>%
         stuff4 %>%
         stuff5

## 31
(
               stuff1 %>%
                  stuff2 %>%
                  stuff3
   ) %>%
   stuff4 %>%
   stuff5

## 32
[
 stuff1 %>%
    stuff2 %>%
    stuff3
 ] %>%
    stuff4 %>%
    stuff5

## 33
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

## 34
stuff[stuff1 %>%
         stuff2 %>%
         stuff3] %>%
         stuff4 %>%
         stuff5

## 35
ggplot() +
   geom(lhs -
           rhs
      ) +
   geom()

## 36
{
      ggplot() +
         geom1(argument1,
               argument2 = (
                     stuff
                  )) +
         geom2()
      }

## 37
stuff +
   fun_call(parameter = argument1,
            fun_call((lhs - rhs
                      argument2
               ) /
                        argument3)
      )



### Comments

## 38
# Side comment

## 39
{
      ## Hanging comment 1
      fun_call(
      {
            ## Hanging comment 2
            }
         )
      }

## 40
{
      ### Section comment
      }


### Logical operators

## 41
stuff1 &&
   stuff2 ||
   stuff3

## 42
(stuff1 &&
    stuff2 ||
    stuff3)

## 43
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
