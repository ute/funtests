####################################################
#
# xydata - a data type for functional data
#
###################################################
# groups of function values are stored as arrays
# in the current version, it is required that they all are taken on the same x-values
# later we can provide a unifying function
# started 04.09.2013. Das wäre Hiltis 82. Geburtstag gewesen.
#
#
#  WANT: testing. Requires aligned functions, one x, several y
#
#  TODOS: 
#  coerce unify groups: interpolate all functions
#  klist into xydata mit nur 1 x-variable
# interpolate an array of functions
# plotting parameters are extra objects (list) xydataplotparm ? oder einfach listen
# flipp1 <- xydatapar(col="blue", default=.flippdefault) # default values
# 

require(plyr)

#' Check whether object is a function list
#' 
#' Checks whether object is of type \code{xydata}
#' @param x any R-object
#' @return \code{TRUE}, if \code{x} belongs to class \code{xydata}, otherwise \code{FALSE}
#' @export
# @author Ute Hahn,  \email{ute@@imf.au.dk}

is.xydata <- function(x) inherits(x, "xydata")

# xydata is a list with elements
#' Make an x-y-list
#' 
#' Generate an object of class \code{xydata}
#' 
#' @param x  numeric, array, the x-values
#' @param y  numeric, array of y-values, dimension: nx x ny
#' @param optlist optional list of (plot) options, see \code{\link{plot.xydata}}
#' @param ... plot options, see \code{\link{plot.xydata}}
# @param fun the summary function to be applied on the values of \code{y}, defaults to \code{mean}
#' 
#' @return a list with elements 
#' \tabular{ll}{
#'  \code{x} \tab{the x-values}\cr
#'  \code{y}  \tab{the function y-values}\cr
#'  \code{nx} \tab{integer, dimension of \code{x}}\cr
#'  \code{ny} \tab{integer, number of functions y included in the list}\cr
#'  \code{options} \tab list of plot options, see \code{\link{plot.xydata}}
#' }
#' @export
# @author Ute Hahn,  \email{ute@@imf.au.dk}


xydata <- function(x, y, optlist, ...) #fun = mean, ...)
{
  xa <- as.array(x)
  nx <- dim(xa)
  ya <- as.array(y)
  if (length(dim(xa)) == length(dim(ya))) ny <- 1 else ny <- dim(ya)[length(dim(ya))] # only one y
  if (ny == 1) my <- dim(ya) else my <- dim(ya)[-length(dim(ya))]
  if (any(nx != my)) stop("dimensions do not match")
 # argu <- list(...)
  if (missing(optlist)) optlist <- NULL
  argu <- list(...)
  
 #  options <- updateoptions(defaultoptions.xydata, optlist)
  options <- updateoptions(updateoptions(defaultoptions.xydata, optlist), argu)
#  options <- do.call(updateoptions, c(list(defaultoptions.xydata), argu))
  xy <- list(x = xa, 
             y = ya,
             nx = dim(xa),
             ny = ny,
             options = options)
  class(xy) <- "xydata"
  return(xy)
}



#' Extract or replace subset of an xydata
#' 
#' curves (rows of y-values) are extracted or replaced
#' 
#' @rdname extract.xydata
# @name extract.xydata 
#' @aliases [.xydata [<-.xydata
#' @S3method [ xydata
#' @method [ xydata
#' @export
#' @param x an object of class \code{"xydata"}. 
#' @param i subset index.
#' @param j,drop ignored.
#' @seealso \code{\link{xydata}} for details on the class.
# @author Ute Hahn,  \email{ute@@imf.au.dk}


"[.xydata" <- function(x, i, j, drop, ...) 
  {
    xx <- as.array(x$x)
    if (length(dim(xx))>1) stop("sorry, not implemented yet for higher dimensions")
    yy <- x$y[,i]
    opt <- x$options
    return(xydata(xx, yy, opt))
  }

#' @rdname extract.xydata
#' @S3method [<- xydata
#' @method [<- xydata
#' @export
#' @param value Replacement for the subset, an array or an xydata object. 
#' @details Currently only possible if x$x is one-dimensional.
#' Replacement y-values have to be of same dimension as the original.

"[<-.xydata" <- function(x, i, j, value) 
  {
    xx <- as.array(x$x)
    if (length(dim(xx))>1) stop("sorry, not implemented yet for higher dimensions")
    yy <- x$y
    if(is.array(value)) yy[, i] <- value 
    else if(is.xydata(value)) yy[, i] <- value$y
    else stop("can only replace with vectors or xydata objects")
    opt <- x$options
    return(xydata(xx, yy, opt))
  }

# range of y-values
# @param xy the data to be inspected
# @param includy anything to be included in the range
# @return numeric vector of two
# @export
#' @rdname xydata-internal
#' @keywords {internal}
# @author Ute Hahn,  \email{ute@@imf.au.dk}

yrange <- function(xy, includy=NULL) range(c( range(xy$y), includy))

#' Print brief details of an xy-list
#'
#' Gives short dimensions of the elements in the argument
#'
#' @S3method print xydata
#' @method print xydata
#' @param x object of type \code{\link{xydata}}
#' @param ... ignored
#' @export
# @author Ute Hahn,  \email{ute@@imf.au.dk}


print.xydata <- function (x, ...)
{
  dimx <- x$nx[1]
  if (length(x$nx) > 1) for (i in 2: length(x$nx)) dimx <- paste(dimx, x$nx[i], sep="x")
  
  cat("xy-list with",dimx,"x-values and", x$ny,"sets of y-values,\n",
    "x-range:", range(x$x), " y-range:", range(x$y),"\n")
}



# @rdname xydata-internal
# @keywords internal
#' @export
# @title Plotting defaults for xydata
# List of defaults for plotting \code{\link{xydata}}-objects
#' @rdname xydata-internal
#' @aliases defaultoptions.xydata
#' @docType data
defaultoptions.xydata <- list (
  xlab = "x",
  ylab = "y",
  col = "black",
  light = 0, # how to lighten up the color for the individual lines
  lwd = 1,
  lty = "solid"
)  

