\docType{package}
\name{xystat-package}
\alias{xystat}
\alias{xystat-package}
\title{The xystat package}
\description{
  The package \pkg{xystat} is a collection of methods for
  analysis and visualisation of grouped data that consist
  of an independent (\eqn{x}-) variable and a dependent
  (\eqn{y}-) variable.  Such data could be time series or
  values of a function, or generally any multivariate data.
  In the latter case, \eqn{x} is just an index variable.
  Individual data sets are supposed to share the same
  \eqn{x}-variable (this will be changed in a future
  version).
}
\section{{Data class and generic methods}
  \tabular{ll}{
   \code{\link{xydata}} \tab basic data type\cr
   \code{\link{print.xydata}} \tab print brief details of an xy-list\cr
   \code{\link{plot.xydata}} \tab plot individual members, summary function \cr
      \tab or envelopes of an xy-list\cr
 }}{

}

\section{{Manipulation and Analysis}
  \tabular{ll}{
   \code{\link{apply.xydata}} \tab apply a summary function on the \eqn{y}-values\cr
   \code{\link{studpermute.test}} \tab studentized permutation test for two samples\cr
 }}{

}
\examples{
# simulated data sets, consisting of 8 and 9
x <- seq(0, 1, .1)
y1 <- replicate(8, rnorm(length(x), mean = x, sd = .2))
y2 <- replicate(7, rnorm(length(x), mean = x*1.1, sd = .2))
y1b <- replicate(7, rnorm(length(x), mean = x, sd = .2))

xy1 <- xydata(x, y1)
xy2 <- xydata(x, y2)
xy1b <- xydata(x, y1b)

# visualize the data sets

plot(envel (xy1))
plot(xy1, add = TRUE)
plot(xy1b, add = TRUE, col = "blue")
plot(xy2, add = TRUE, col = "red")

# there should be a slightly significant difference between xyl1
# and cyl2, but not between xyl1 and xyl1b.
# However, with simulated data, the result may be unexpected...

studpermute.test(xy1, xy2)
studpermute.test(xy1, xy1b)
}
\author{
  Ute Hahn, \email{ute@imf.au.dk}
}
\references{
  Hahn, U. (2012) A studentized permutation test for the
  comparison of spatial point patterns. \emph{Journal of
  the American Statistical Association} \strong{107} (498),
  754--764.
}
\keyword{package}

