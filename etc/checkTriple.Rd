\name{checkTriple}
\title{Check Consistency of Conditional Independence for a Triple of Nodes}
\usage{
 "An Rd file exposing an ESS bug in  (ess-next-code-line) :
 Jump to '\examples{' and the first code line (= all '##...') and
 use C-c C-n  to send it to R.  It works, *but* point does not advance to
 the next line; but rather jumps backward to the 50'percent' line.
 Adding a percent 'o/o' sign here ``fixes'' this bug (so I don't do it) "

checkTriple(version.unf = c(NA, NA),
            maj.rule = FALSE, verbose = FALSE)
}
\arguments{
  \item{version.unf}{
    (integer) vector of length two: .............}
  \item{maj.rule}{logical indicating that the following majority rule
    is applied: if \code{b} is in less than
    50\% of the checked sepsets, we say that \code{b} is in \bold{no} sepset.  If
    \code{b} is in more than 50\% of the checked sepsets, we say that
    \code{b} is in \bold{all} sepsets.  If \code{b} is in exactly 50\% of the
    checked sepsets, the triple is considered \sQuote{ambiguous}.}
  \item{verbose}{Logical asking for detailed output of intermediate steps.}
}
\description{
  For each subset .........
}
\references{
  D. Colombo and M.H. Maathuis (2013).
  Order-independent constraint-based causal structure learning,
  (arXiv:1211.3295v2).
}

\examples{
##################################################
## Using Gaussian Data
##################################################
## Load predefined data
data(gmG)
n <- nrow(gmG8$x)
p <- ncol(gmG8$x)

## define independence test (partial correlations), and test level
indepTest <- gaussCItest
alpha <- 0.01
## define sufficient statistics
suffStat <- list(C = cor(gmG8$x), n = n)

## estimate CPDAG
pc.fit <- pc(suffStat, indepTest, p=p, alpha=alpha, verbose = TRUE)

if (require(Rgraphviz)) {
  ## show estimated CPDAG
  par(mfrow=c(1,2))
  plot(pc.fit, main = "Estimated CPDAG")
  plot(gmG8$g, main = "True DAG")
}

checkTriple(a = 6, b = 1, c = 8,
            nbrsA = c(1,5,7),
            nbrsC = c(1,5),
            sepsetA = pc.fit@sepset[[a]][[c]],
            sepsetC = pc.fit@sepset[[c]][[a]],
            suffStat=suffStat, indepTest=indepTest, alpha=alpha,
            version.unf = c(2,2),
            verbose = TRUE)
}
\keyword{manip}
