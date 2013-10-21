# Apply and generic functions for fdsample

#' Apply summary function to the y-values in an fdsample
#'
#' Applies a function to the curves function values (\eqn{y}-values) in an 
#' \code{\link{fdsample}} object, for each argument value (\eqn{x}-value).
#'
# @S3method apply fdsample
#' @param x object of type \code{\link{fdsample}}
#' @param fun the function to apply, defaults to the mean
#' @param fopt list of options to \code{fun}
#' @param ... optional, (plot) options for updating \code{options} element of the result
#' @details The function \code{fun} should return a single number or a vector of
#' fixed length. This is not checked.
#' @export
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @examples
#' data(exampledata)  
#' # look at data set fuda 
#' str(fuda)
#' # apply the summary function contained in fuda$options
#' str(apply.fdsample(fuda))
#' # apply the median  
#' str(apply.fdsample(fuda, "median"))
#' # apply median and change axis label
#' str(apply.fdsample(fuda, median, xlab = "median of y-values"))
#' # another way to get the median and change axis label
#' str(apply.fdsample(fuda, quantile, list(prob = 0.5), xlab = "50% quantile of y-values"))


apply.fdsample <- function(x, fun = mean, fopt = list(), ...)
{
  stopifnot(is.fdsample(x))
  dimsx <- length(x$dimarg)
  #argus <- c(fopt, list(...))
  newy <- do.call(aaply, c(list(.data = x$fvals, .margins = 1:dimsx, .fun = fun), fopt))
  # did fun return vectors? then newy is two dimensional
  # if (length(dim(newy)) == 2) 
  # {    
  # }
  # dimly <- length(names(xy$fvals))
  # names(newy) <- names(xy$fvals)[-dimly]
  # TODO if concatenate is defined in plutils, change here
  newx <- fdsample(x$args, as.array(newy), style(x$options,  style(...)))
  return(newx)
} 


#' @title Generic summary functions
#' @rdname summaryfunctions.fdsample
#' @aliases mean.fdsample median.fdsample quantile.fdsample
#' @description Calculate summary functions (mean, median or quantiles) of the 
#' function values pointwise (for each argument value).
#' @return an \code{\link{fdsample}} object, where \code{fvals} represents 
#' the values of the summary function(s)
#' @seealso \code{\link{mean}}, \code{\link{median}}. \code{\link{quantile}}
# @author Ute Hahn,  \email{ute@@imf.au.dk}

#'@param x an object of class \code{\link{fdsample}}
#'@param trim the fraction (0 to 0.5) of observations to be trimmed from each 
#'end of \code{x} before the mean is computed. Values of trim outside that range 
#'are taken as the nearest endpoint.
#'@param na.rm logical; if true, any NA and NaN's are removed from \code{x}
#' before the quantiles are computed.
#'@param ... further arguments passed to or from other methods.
#' @S3method mean fdsample 
#' @method mean fdsample 
#' @export
#' @examples
#' str(mean(fuda))

mean.fdsample <- function (x, trim = 0, na.rm = FALSE, ...) 
  apply.fdsample (x, mean, fopt = list(trim = trim, na.rm = na.rm, ...)) 

#' @rdname summaryfunctions.fdsample
#' @S3method median fdsample 
#' @method median fdsample 
# @usage median.fdsample (x, na.rm = FALSE)
#' @export
#@param x an object of class \code{\link{fdsample}}
#@param na.rm a logical value indicating whether NA values should be stripped 
# before the computation proceeds.
median.fdsample <- function (x, na.rm = FALSE, ...)
   apply.fdsample(x, median, fopt = list( na.rm = na.rm), ... )

#' @rdname summaryfunctions.fdsample
#' @S3method quantile fdsample 
#' @method quantile fdsample 
# @usage quantile.fdsample (x,  probs = seq(0, 1, 0.25), na.rm = FALSE,
#          names = TRUE, type = 7, ...)
#' @export
#@param x an object of class \code{\link{fdsample}}
#'@param probs numeric vector of probabilities with values in [0,1]. 
#@param na.rm logical; if true, any NA and NaN's are removed from \code{x}
# before the quantiles are computed.
#'@param names logical; if true, the result has a names attribute. 
#'@param type an integer between 1 and 9 selecting one of the nine quantile algorithms, 
#' see the documentation of \code{\link{quantile.default}}
#@param ... further arguments passed to or from other methods.

quantile.fdsample <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = FALSE, type = 7, ...)
 apply.fdsample(x, quantile, fopt = list(probs = probs,  na.rm = na.rm,  
   names = names, type = type, ...))
      