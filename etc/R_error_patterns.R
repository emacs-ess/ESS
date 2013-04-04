## 1
Error:  chunk 7 (label = OP4)
Error in disp.Rnw:656:31: unexpected symbol
655: par(mgp = c(2.5, 1, 1), mar = c(0, 0, 0, 0),
         656:     plt= c(0.08, 0.9, 0.25, 0.p9

))                    

## 2
Browse[2]> Error in x %*% y (from models.R#46) : 
  Cholmod error 'X and/or Y have wrong dimensions' at file ../MatrixOps/cholmod_sdmult.c, line 90
                    

## 3 this one is not recognised
Error in source("~/works/protoClasses/R/funcs.R") (from hierarchy.R#6) : 
  ~/works/protoClasses/R/funcs.R:1797:5: unexpected '[['
1796:     b[[2]] <- quote(browser())
1797:     [[
         ^                             
## 4 not recognized

source("basicModel.R")
Error in source("basicModel.R") : basicModel.R:95:1: unexpected symbol
94: 
95: ixQ
   ^            
