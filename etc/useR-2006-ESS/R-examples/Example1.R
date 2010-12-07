

     ## Silly example

we <- function(c) {

have.a.function <- c

which.is <- function(x) {   return(have.a.function + x)   }
           # so where does this go?
  silly <- which.is(4)
    ### or this one?
  return(list(my.silly=silly,
     myfunction=which.is))
}


    
