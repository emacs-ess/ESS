## TODO: automate this somehow.
##
## For now manually
## 
## (progn
##     (setq-local compilation-error-regexp-alist ess-error-regexp-alist)
##     (compilation-minor-mode))
##
## and check if everything is highlighted as expected

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
==25269==    by 0x9EC363C: distance (rwmd.cpp:21)
==25269==    by 0x9EC90C9: operator()(unsigned long, unsigned long) (rwmd.cpp:137)

## 9 testhat new patterns
test_embeddings.R:20: failure: average embedding works
embed_vocab(vocab, embs) not equal to embs[, 1:N].
Attributes: < Length mismatch: comparison on first 1 components >

test_embeddings.R:59: error: average embedding works with missing values
no 'dimnames' attribute for array
1: expect_equal(e[, "dd"], e[, "ee"]) at /store/Dropbox/dev/mlvocab/tests/testthat/test_embeddings.R:59
2: quasi_label(enquo(object), label) at /tmp/Rtmp6McxD6/R.INSTALL70c948e315c6/testthat/R/expect-equality.R:51
3: eval_bare(get_expr(quo), get_env(quo)) at /tmp/Rtmp6McxD6/R.INSTALL70c948e315c6/testthat/R/expectation.R:90

# 10 rlang backtrace
Backtrace:
    █
 1. ├─global::update_orders(self, mm())
 2. │ └─self$orders(name) ~/dev/foo/bla.R:157:2
 3. │   └─purrr::keep(...) ~/dev/foo/bla.R:85:14
 4. │     └─purrr:::probe(.x, .p, ...)
 5. │       └─purrr::map_lgl(.x, .p, ...)
 6. └─purrr:::stop_bad_type(...)

# 11 "at" rlang backtrace
 10. └─shapvis::shiny_xdeps_ui(shads, name = "SHAP TS", per_page = per_page) at shiny/R/bootstrap.R:761:2
 11.   └─shapvis:::shiny_ui(...) at shapvis/R/app.R:72:2
 12.     └─shapvis:::var_groups(shad, exclude_regexp) at shapvis/R/app.R:106:2
 13.       ├─base::unique(...) at shapvis/R/app.R:33:2
 14.       └─base::colnames(shad$extra) at shapvis/R/app.R:33:2
 15.         └─base::is.data.frame(x)

# 12 testhat failure
Failure (test-kmeans.R:430:3): predict_KMeans returns the correct output if the input is a data frame AND

# 13 shiny pattern
Warning: Error in *: non-numeric argument to binary operator
  173: plot_shap_deps_internal [/home/joe/proj/R/plot.R#219]
  172: fn [/home/joe/proj/R/plot.R#184]

## but not these ranges:
> str(list(1:3))
List of 1
 $ : int [1:3] 1 2 3
>
