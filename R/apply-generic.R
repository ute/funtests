# Apply and generic functions for xydata

#' Apply summary function to the y-values in an xydata
#'
#' Applies a function to the \eqn{y}-values in a list, for each \eqn{x}
#'
# @S3method apply xydata
#' @param xy object of type \code{\link{xydata}}
#' @param fun the function to apply, defaults to the mean
#' @param fopt list of options to \code{fun}
#' @param ... optional, (plot) options for updating \code{options} element of the result
#' @details The function \code{fun} should return a single number or a vector of
#' fixed length. This is not checked.
#' @export
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @examples
#' data(exampledata)  
#' # look at data set xyda 
#' str(xyda)
#' # apply the summary function contained in xyda$options
#' str(apply.xydata(xyda))
#' # apply the median  
#' str(apply.xydata(xyda, "median"))
#' # apply median and change axis label
#' str(apply.xydata(xyda, median, xlab = "median of y-values"))

apply.xydata <- function(xy, fun = mean, fopt = list(), ...)
{
  stopifnot(is.xydata(xy))
  dimsx <- length(xy$nx)
  #argus <- c(fopt, list(...))
  newy <- do.call(aaply, c(list(.data = xy$y, .margins = 1:dimsx, .fun = fun), fopt))
  # did fun return vectors? then newy is two dimensional
  # if (length(dim(newy)) == 2) 
  # {    
  # }
  # dimly <- length(names(xy$y))
  # names(newy) <- names(xy$y)[-dimly]
  newxy <- xydata(xy$x, as.array(newy), xy$options,  ...)
  return(newxy)
} 


#' @title Generic summary functions
#' @rdname summaryfunctions.xydata
#' @aliases mean.xydata median.xydata quantile.xydata
#' @description Calculate summary functions (mean, median or quantiles) of the 
#' y-values pointwise (for each x-value)
#' @return an xy-list, where \code{y} represents the values of the summary function(s)
#' @seealso \code{\link{mean}}, \code{\link{median}}. \code{\link{quantile}}
# @author Ute Hahn,  \email{ute@@imf.au.dk}

#'@param x an object of class \code{\link{xydata}}
#'@param trim the fraction (0 to 0.5) of observations to be trimmed from each 
#'end of \code{x} before the mean is computed. Values of trim outside that range 
#'are taken as the nearest endpoint.
#'@param na.rm logical; if true, any NA and NaN's are removed from \code{x}
#' before the quantiles are computed.
#'@param ... further arguments passed to or from other methods.
#' @S3method mean xydata 
#' @method mean xydata 
#' @export
#' @examples
#' str(mean(xyda))

mean.xydata <- function (x, trim = 0, na.rm = FALSE, ...) 
  apply.xydata (x, mean, fopt = list(trim = trim, na.rm = na.rm, ...)) 

#' @rdname summaryfunctions.xydata
#' @S3method median xydata 
#' @method median xydata 
# @usage median.xydata (x, na.rm = FALSE)
#' @export
#@param x an object of class \code{\link{xydata}}
#@param na.rm a logical value indicating whether NA values should be stripped 
# before the computation proceeds.
median.xydata <- function (x, na.rm = FALSE)
   apply.xydata(x, median, fopt = list( na.rm = na.rm) )

#' @rdname summaryfunctions.xydata
#' @S3method quantile xydata 
#' @method quantile xydata 
# @usage quantile.xydata (x,  probs = seq(0, 1, 0.25), na.rm = FALSE,
#          names = TRUE, type = 7, ...)
#' @export
#@param x an object of class \code{\link{xydata}}
#'@param probs numeric vector of probabilities with values in [0,1]. 
#@param na.rm logical; if true, any NA and NaN's are removed from \code{x}
# before the quantiles are computed.
#'@param names logical; if true, the result has a names attribute. 
#'@param type an integer between 1 and 9 selecting one of the nine quantile algorithms, 
#' see the documentation of \code{\link{quantile.default}}
#@param ... further arguments passed to or from other methods.

quantile.xydata <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = FALSE, type = 7, ...)
 apply.xydata(x, quantile, fopt = list(probs = probs,  na.rm = na.rm,  
   names = names, type = type, ...))
      