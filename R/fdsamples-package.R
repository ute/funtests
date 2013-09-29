# fdsamples package

require(plyr)


#'@import stats
#'
#'@name fdsamples-package
#'
#'@aliases fdsamples-package  fdsamples
#'@docType package
#'@title The fdsamples package
#'@description 
#'  The package \pkg{fdsamples} is a collection of methods for analysis and 
#'  visualisation of grouped data that consist of an independent (\eqn{x}-) 
#'  variable and a dependent (\eqn{y}-) variable.  Such data could be time 
#'  series or values of a function, or generally any multivariate data.  
#'  In the latter case, \eqn{x} is just an index variable.
#'    Individual data sets are supposed to share the same \eqn{x}-variable 
#'    (this   will be changed in a future version). 
#'@section {Data class and generic methods}
#'   \tabular{ll}{
#'    \code{\link{fdsample}} \tab basic data type\cr
#'    \code{\link{print.fdsample}} \tab print brief details of an fdsample\cr
#'    \code{\link{plot.fdsample}} \tab plot individual members, summary function \cr
#'       \tab or envelopes of an fdsample\cr
#'  } 
#'@section {Manipulation and Analysis}
#'   \tabular{ll}{
#'    \code{\link{apply.fdsample}} \tab apply a summary function on the function values\cr
#'    \code{\link{studpermute.test}} \tab studentized permutation test for two samples\cr
#'  } 
#'@author Ute Hahn, \email{ute@@imf.au.dk}
#'@references 
#'Hahn, U. (2012) A studentized permutation test for
#'the comparison of spatial point  patterns.
#'\emph{Journal of the American Statistical Association} \strong{107} (498), 754--764.
#'
#'@keywords package
#'@examples 
#' # simulated data sets, consisting of 8 and 9 
#' x <- seq(0, 1, .1)
#' y1 <- replicate(8, rnorm(length(x), mean = x, sd = .2))
#' y2 <- replicate(7, rnorm(length(x), mean = x*1.2, sd = .2))
#' y1b <- replicate(7, rnorm(length(x), mean = x, sd = .2))
#' 
#' xy1 <- fdsample(x, y1)
#' xy2 <- fdsample(x, y2)
#' xy1b <- fdsample(x, y1b)
#'
#'# visualize the data sets
#'
#'summaryplot(xy1, envprob=1)
#'summaryplot(xy1b, add = TRUE, col = "blue")
#'summaryplot(xy2, add = TRUE, col = "red")
#'
#'# there should be a slightly significant difference between xyl1 
#'# and xyl2, but not between xyl1 and xyl1b. 
#'# However, with simulated data, the result may be unexpected...
#'
#'studpermute.test(xy1, xy2) 
#'studpermute.test(xy1, xy1b) 
NA
