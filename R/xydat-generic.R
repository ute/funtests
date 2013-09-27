# Generic functions for xydata

#' @title Generic summary functions
#' @rdname summaryfunctions.xydata
#' @aliases mean.xydata median.xydata quantile.xydata
#' @description Calculate summary functions (mean, median or quantiles) of the 
#' y-values pointwise (for each x-value)
#' @return an xy-list, where \code{y} represents the values of the summary function(s)
#' @S3method mean xydata 
#' @export
#' @examples
#' str(mean(xyda))
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

mean.xydata <- function (x, trim = 0, na.rm = FALSE, ...) 
  apply.xydata (x, mean, fopt = list(trim = trim, na.rm = na.rm, ...)) 

#' @rdname summaryfunctions.xydata
#' @S3method median xydata 
# @usage median.xydata (x, na.rm = FALSE)
#' @export
median.xydata <- function (x, na.rm = FALSE)
   apply.xydata(x, median, fopt = list( na.rm = na.rm) )

#' @rdname summaryfunctions.xydata
#' @S3method quantile xydata 
# @usage quantile.xydata (x,  probs = seq(0, 1, 0.25), na.rm = FALSE,
#          names = TRUE, type = 7, ...)
#' @export
quantile.xydata <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7, ...)
 apply.xydata(x, quantile, fopt = list(probs = probs,  na.rm = na.rm,  
   names = names, type = type, ...))
      