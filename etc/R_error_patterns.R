
## 1
Error:  chunk 7 (label = OP4)
Error in disp.Rnw:656:31: unexpected symbol
655: par(mgp = c(2.5, 1, 1), mar = c(0, 0, 0, 0),
         656:     plt= c(0.08, 0.9, 0.25, 0.p9

))                    

## 2
Browse[2]> Error in x %*% y (from models.R#46) : 
  Cholmod error 'X and/or Y have wrong dimensions' at file ../MatrixOps/cholmod_sdmult.c, line 90
                    

## 3 
Error in source("~/works/protoClasses/R/funcs.R") (from hierarchy.R#6) : 
  ~/works/protoClasses/R/funcs.R:1797:5: unexpected '[['
1796:     b[[2]] <- quote(browser())
1797:     [[
         ^                             
## 4 

source("basicModel.R")
Error in source("basicModel.R") : basicModel.R:95:1: unexpected symbol
94: 
95: ixQ
   ^            

## 5.a
> + Error in source(file = "/home/vitoshka/works/pbm/R/S4.R") (from #1) : 
  /home/vitoshka/works/pbm/R/S4.R:36:62: unexpected ')'
35:                           }, list(vname = as.name(".pix_v")), 
36:                                   pname = as.name(".pix_p"))))
                                                                ^

## 5.b
> + Error in source(file = "/home/vitoshka/works/pbm/R/S4.R") (from #1) : 
  c:/home/vitoshka/works/pbm/R/S4.R:36:62: unexpected ')'
35:                           }, list(vname = as.name(".pix_v")), 
36:                                   pname = as.name(".pix_p"))))
                                                                ^
>             

                             
## 6 first line is not a pattern!
+ . + Error in base::source(file = file, echo = echo, local = local, print.eval = print.eval,  (from #95) : 
  /tmp/model_mixture.R@4:5:13: unexpected symbol
4:             Mq$DATA$ixs$clust <- data$ixQ
5:             Mq


## 7 don't highlight dates
       id              lat             lon                         obs_date  
 Min.   :  1.00   Min.   :21.57   Min.   :-179.88   01/02/1997 04:16:53:  1  
 1st Qu.: 99.25   1st Qu.:24.36   1st Qu.:-147.38   01/02/1997 05:56:25:  1  
 Median :197.50   Median :25.64   Median :-119.64   01/04/1997 17:41:54:  1  
 Mean   :197.50   Mean   :27.21   Mean   : -21.52   01/05/1997 17:20:07:  1  
 3rd Qu.:295.75   3rd Qu.:27.41   3rd Qu.: 153.66   01/06/1997 04:31:13:  1  
 Max.   :394.00   Max.   :39.84   Max.   : 179.93   01/06/1997 06:12:56:  1  
                                                    (Other)            :388  

## 8 valgrind errors
==25269== Invalid read of size 8
==25269==    at 0x9EC363C: inner_product<double const*, double const*, double> (stl_numeric.h:183)
==25269==    by 0x9EC363C: distance(RcppParallel::RMatrix<double> const&, unsigned long, unsigned long, DistType) (rwmd.cpp:21)
==25269==    by 0x9EC90C9: RelaxedWordMoverDistanceSparse::operator()(unsigned long, unsigned long) (rwmd.cpp:137)
                                                    
