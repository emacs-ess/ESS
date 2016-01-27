

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

## 6a
object <- function()
{
    body
}

## 6b
object <-
    function()
    {
        body
    }

## 6c
object =
    function()
    {
        body
    }

## 6d
fun_call(argument) <-
    function()
    {
        body
    }

## 7
{
    object <- function()
    {
        body
    }
}

## 8
{
    fun_call(parameter = function()
    {
        body
    })
}

## 9
{
    fun_call(parameter = function() {
        body
    })
}

## 10
fun_call(
    function() {
    stuff
}
)

## 11
{
    fun_call1(fun_call2(argument, function() {
        stuff
    })
    )
}

## 12
{
    fun_call1(argument, fun_call2(function() {
                            stuff
                        })
              )
}

## 13
fun_call(object :=
             function()
             {
                 body
             })

## 14
fun_call(argument,
         function(x)
    stuff
    )

## 15a
`object` <- function()
{
    body
}

## 15b
"object" <- function()
{
    body
}

## 15c
'object' <- function()
{
    body
}


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

## 6b
`:=`(argument1,
     argument2)

## 7
`fun_call`(
    argument1,
    argument2
)

## 7b
`:=`(
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
        fun_call4(
            argument6
        ),
        argument7
        ), {
        stuff
    },
    argument8
    )
}

## 12
object <- fun_call(
    arg1,
    arg2
)

## 13
fun_call1(fun_call2(
    argument
))

## 14
some_function <- fun_call1(fun_call2(
    argument
))

## 15
object[, fun_call(
    argument
)]

## 16
fun_call1(argument1, fun_call2(fun_call3(
                         argument2
                     ))
          )

## 17
fun_call({
    stuff1
    stuff2

    stuff3
})

## 18
fun_call(argument1 %>%
         stuff,
         argument2)

## 19
fun_call(argument,
         )

## 20
fun_call(parameter1 = ,
         parameter2 = argument)


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
fun_call(
{
    stuff
}, {
    stuff
}
)

## 10
object <-
    fun_call({
        stuff
    }, {
        stuff
    })

## 11
object <-
    fun_call(     {
        body
    }
    )

## 12
fun_call1(
    fun_call2({
        stuff
    }
    )
)

## 13
{
    stuff1

    {
        stuff2
    }
}

## 14
{{
    stuff
}
}

## 15
({
    stuff
})

## 16
( {
    stuff
}
)

## 17
fun_call(argument, function(argument1,
                            argument2) {
    body
}
)

## 18
fun_call(
    argument,
    function(argument1,
             argument2) {
    body
}
)

## 19
fun_call1(
    fun_call2(argument, function(x) {
        body
    })
)

## 20
fun_call1({
    object1 <- fun_call2(
        argument)
    object2
})

## 21
fun_call(argument,
         function() {

    stuff
}
}

## 22
function_call()
stuff


### Bracket indexing

## 1
object[
    argument1,
    argument2
]

## 2
object[argument1,
       argument2
       ]

## 3
object[(
    argument1
)]

## 4
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

## 5
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
{
    if (condition) {
        stuff1
    } else {
        stuff2
    }
}

## 2
{
    if (condition) {
        stuff1
    }
    else {
        stuff2
    }
}

## 3
{
    if (condition)
    {
        stuff1
    } else
    {
        stuff2
    }
}

## 4
{
    if (condition)
    {
        stuff1
    }
    else
    {
        stuff2
    }
}

## 5
{
    for (sequence)
    {
        stuff
    }
}

## 6
{
    for (sequence) {
        stuff
    }
}

## 7
if (condition)
    stuff

## 8
for (sequence)
    stuff

## 9
object <-
    if (condition) {
        stuff1
    } else {
        stuff2
    }

## 10
{
    object <-
        if (condition) stuff1
        else stuff2
}

## 10
{
    object <-
        if (condition) fun_call(
                           argument1,
                           argument2
                       )
        else stuff
}

## 11
{
    fun_call(parameter =
                 if (condition)
                     stuff1
                 else
                     stuff2
             )
}

## 12
{
    if (condition1) {
        stuff1
    }
    else if (condition2)
        stuff2
    else if (condition3) {
        stuff3
    } else if (condition4)
        stuff4
    else
        stuff5
}

## 13
fun_call(
    argument,
    parameter = if (condition1) {
                    stuff1
                } else if (condition2) {
                    stuff3
                } else {
                    stuff2
                }
)

## 14
fun_call(
    argument,
    parameter =
        if (condition1)
            stuff1
        else if (condition2)
            stuff3
        else
            stuff2
)

## 15
object <- fun_call(argument,
                   parameter = if (condition1) {
                                   stuff1
                               } else if (condition2) {
                                   stuff3
                               } else {
                                   stuff2
                               }
                   )

## 16
object <- fun_call(argument, if (condition)
                                 stuff1
                             else if (condition2)
                                 stuff2
                   )

## 17
while(condition)
    stuff

## 18
if (condition1)
    stuff1
else
    if (condition2) {
        stuff2
    }

## 19
object <-
    if (condition)
        fun_call()[index]
    else
        stuff

## 20
funcall({
    if (test1)
        stuff1
    if (test2)
        stuff2
})

## 21
fun_call(argument,
         function() {

    if (cond) object1 <- object2
    else object3 <- object4
})

## 22
if (cond1)
    if (cond2)
        if (cond3)
            stuff1
        else if (cond4)
            stuff2
        else
            if (cond5)
                stuff3
        else
            stuff4
    else if (cond6)
        stuff5
    else
        if (cond7)
            stuff6
    else
        stuff7
else if (cond8)
    stuff8
else
    if (cond9)
        stuff9
else
    stuff10

## 23
if (cond1)
    if (cond2)
        for (sequence1)
            if (cond3)
                stuff1
            else
                stuff2
    else if (cond4)
        for (sequence2)
            stuff3
    else
        if (cond5)
            fun_call(
                argument
            )
    else
        stuff5
else
    stuff6

## 24
object <- if(cond)
              stuff1
          else
              stuff2

## 25
if (condition) {
    (stuff)
}

## 26
{
    if (condition1)
        stuff1
    else if (condition2) {
        stuff2
    }
    else if (condition3)
        stuff3
}

## 27
object <- if (condition) {
              stuff1
          }
          else {
              stuff2
          }

## 28
if (condition)
    object <-
        stuff

## 29
if (condition1)
    object <-
        if (condition2)
            stuff1
        else
            stuff2
else
    stuff3


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
            stuff8
    } %>%
    stuff9 %>%
    stuff10

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
             ) /
    stuff5

fun_call(arg1 +
         arg2, arg3 +
               arg4)

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
    fun_call2()%>%
    fun_call3()

## 13
{
    (stuff) %>%
        fun_call()

    {stuff} %>%
        fun_call()
}

## 14
{
    object + (
        stuff
    ) %>%
        fun_call()

    object + {
        stuff
    } %>%
        fun_call()
}

## 15
object <-
    stuff1 +
    stuff2 ~
        stuff3 +
        stuff4 :=
            stuff5 +
            stuff6 =
                stuff7 +
                stuff8

## 16
object <- stuff1 +
    stuff2 + stuff3 +
    stuff4 ~ stuff5 +
        stuff6 + stuff7 +
        stuff8 := stuff9 +
            stuff10 + stuff11 +
            stuff12 = stuff13 +
                stuff14 + stuff15 +
                stuff16

## 17
object %>%
    {
        stuff1
    } %>% object[index] %>% {stuff2} %>% fun_call1() +
    {if (condition1) stuff3 else stuff4} +
    if (condition2) {
        stuff5
    } else if (condition3) {
        stuff6
    } else {
        stuff7
    } %>%
    (fun_call2()) %>% fun_call3() %>%
    fun_call3()

## 18
`object`$`elem` <- stuff1 +
    stuff2
`object`@`elem` <- stuff1 +
    stuff2

## 19
{
    ## comment
    object1 <-
        object2
}

## 20
fun_call(stuff1 + stuff2 +
         stuff3 +
         (stuff4 + stuff5 +
          stuff6) +
         object[stuff7 +
                stuff8] +
         {stuff9 +
              stuff10})


## 21
object %>% fun_call({
               stuff1
           }) %>%
    stuff2

## 22
"string1" %>%
    'string2' %>%
    `stuff1` %>%
    stuff2

## 23
object[index] %>%
    fun_call1(
        argument1
    )[index2] %>%
    fun_call2(
        argument2
    )[[index3]] %>%
    stuff

## 24
fun_call(argument) <-
    hop

## 25
fun_call1(argument, fun_call2(
                        stuff1
                    ) +
                    stuff2)

## 26
object <-
    {
        stuff1
    } %>%
    (
        stuff2
    )

## 27
fun_call1(fun_call2(fun_call3(
    argument
))) %>%
    fun_call2()

## 28
fun_call(argument1 %>%
         stuff,
         argument2)

## 29
fun_call(stuff1 :=
             (stuff2),
         argument)

## 30
fun_call1(fun_call2(
    fun_call3()) %>%
    stuff)

## 31
fun_call(object1 + object2 ~ object3 +
             object4 + object5 := object6 +
                 object7,
         argument)

## 32
fun_call(object ~
             )

## 33
fun_call(object +
         )

## 34
fun_call(object[index1]$element[index2][index3]@attribute +
         stuff)

## 35a
fun_call(argument <-
             object)

## 35b
fun_call(argument <<-
             object)

## 36
funcall(!stuff1 ||
        stuff2)


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

## 4
fun_call(
    ## Comment
    argument
)

## 5
object %>%
    ## comment,
    ## comment
    stuff

## 6a
object <-
    function()
    {
        stuff
        ## comment
    }

## 6b
object <-
    function()
    {
        ## comment
    }

## 7
{
    fun_call(lhs +
### Comment
             rhs
             )
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

## 4
stuff1 == stuff2 ||
    condition

## 5
(stuff1 == stuff2 ||
 condition
)

## 6
(stuff1 != stuff2 ||
 condition
)

## 7
object <-
    condition1 | condition2 |
    condition3 | condition4

## 8
if (condition1 || object1 %op% object2 ||
    condition3) {
    stuff
}

## 9
any(condition1 |
    condition2) &&
    all(condition3 &
        condition4)


### Specific situations and overrides

## 1
fun_call(
    ifelse(condition1, argument1,
    ifelse(condition2, argument2,
           ifelse))
)
