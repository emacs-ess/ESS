\title{chol2inv}
\name{chol2inv-methods}
\description{
  Invert a symmetric, positive definite square matrix from its Choleski
  decomposition.  Equivalently, compute \eqn{(X'X)^{-1}}{(X'X)^(-1)}
  from the (\eqn{R} part) of the QR decomposition of \eqn{X}.
}
\seealso{
  \code{\link[base]{chol2inv}} (from the \pkg{base} package),
  \code{\link{solve}}.
}
%%-- In Rd preview (after C-c C-p), "se" ([s]kip to [e]xamples):
%%-- "l" works at the 1st line, but does not go further from the 2nd
\examples{
(M <- cbind(1, 1:3, c(1,3,7)))
(cM <- chol(M)) # a "Cholesky" object...
chol2inv(cM) \%*\% M # the identity
stopifnot(all(chol2inv(cM) \%*\% M - Diagonal(nrow(M))) < 1e-10)
}
\keyword{algebra}
