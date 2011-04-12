### Go inside the "preamble" section and type  C-e C-e C-r (ess-roxy-preview-Rd)
### to get an R error message about 'col(m)' --> there's a buglet somewhere

##' Computes different parameter estimates for foo bars and variations
##'
##' @title Estimation procedures for Foo Bar
##' @param x data matrix
##' @param op object to be estimated
##' @param method estimation method; can be
##'        "mle"             MLE
##'        "smle"            SMLE
##'        "dmle"            MLE based on the diagonal
##'        "mde.normal"      minimum distance estimation based on the chisq distribution and CvM distance
##'        "mde.log"         minimum distance estimation based on the Erlang distribution and CvM distance
##'        "tau.tau.mean"    averaged pairwise Kendall's tau estimator
##'        "tau.theta.mean"  average of Kendall's tau estimators
##'        "beta"            multivariate Blomqvist's beta estimator
##' @return estimated value/vector according to the chosen method
##' @author Foo Bar
estimateFoo <- function(x, op, method=c("mle", "smle", "dmle", "mde.normal", "mde.log",
                              "tau.tau.mean", "tau.theta.mean", "beta"))
{
    ....
}
