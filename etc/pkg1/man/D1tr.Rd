\name{D1tr}
\alias{D1tr}
\title{Numerical Derivatives of (x,y) Data}
\description{
  Compute the numerical derivatives of \eqn{f()} given
  observations \code{(x[i], y ~= f(x[i]))}.

  \code{D1tr} is the \emph{\bold{tr}ivial} discrete first derivative
  using simple difference ratios.

  This is \bold{far} from optimal and only kept here for reference.
}
\usage{
D1tr(y, x = 1)
}
\arguments{
  \item{x,y}{numeric vectors of same length, supposedly from a model
    \code{y ~ f(x)}.  For \code{D1tr()}, \code{x} can have length one
    and then gets the meaning of \eqn{h = \Delta x}.}
}
\value{
  \code{D1tr()} returns a numeric vector of the length of \code{y}.
}
\author{Martin Maechler, in 1992 (for S).}
\seealso{\code{\link[sfsmisc]{D1D2}} which directly uses the 2nd
  derivative of the smoothing spline;  \code{\link{smooth.spline}}.
}
\examples{
set.seed(330)
x <- sort(runif(500, 0,10)) 
y <- sin(x) + rnorm(500)/100
f1 <- D1tr(x=x,y=y)
plot(x,f1, ylim = range(c(-1,1, f1)))
curve(cos(x), col=3, add= TRUE)
}
\keyword{smooth}
