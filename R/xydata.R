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
#  struktur überdenken 
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
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

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
#' \code{y}  \tab{the function y-values}\cr
#' \code{nx} \tab{integer, dimension of \code{x}}\cr
#' \code{ny} \tab{integer, number of functions y included in the list}\cr
#' \code{options} \tab list of plot options, see \code{\link{plot.xydata}}
#' }
#' @export
#' @author Ute Hahn,  \email{ute@@imf.au.dk}


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
#' @S3method [ xydata
#' @export
#' @param x an object of class \code{"xydata"}. 
#' @param i subset index.
#' @param j,drop ignored.
#' @seealso \code{\link{xydata}} for details on the class.
#' @author Ute Hahn,  \email{ute@@imf.au.dk}


"[.xydata" <- function(x, i, j, drop, ...) 
  {
    # attach typemarks to marks
    xx <- x$x
    yy <- x$y[,i]
    opt <- x$options
    return(xydata(xx, yy, opt))
  }


#' Apply summary function to the y-values in an xydata
#'
#' Applies a function to the \eqn{y}-values in a list, for each \eqn{x}
#'
#' @S3method apply xydata
#' @param xy object of type \code{\link{xydata}}
#' @param fun the function to apply, defaults to the mean
#' @param ... optional, list of (plot) options for updating \code{options} element of the result
#' @export
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
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

apply.xydata <- function(xy, fun = mean, ...)
{
  stopifnot(is.xydata(xy))
  dimsx <- length(xy$nx)
  newy <- aaply(xy$y, 1:dimsx, .fun = fun)
  dimly <- length(names(xy$y))
  names(newy) <- names(xy$y)[-dimly]
  newxy <- xydata(xy$x, newy, xy$options, sumfun = fun, ...)
  return(newxy)
} 

yrange <- function(xy, includy=NULL) range(c( range(xy$y), includy))

#' Print brief details of an xy-list
#'
#' Gives short dimensions of the elements in the argument
#'
#' @S3method print xydata
#' @param x object of type \code{\link{xydata}}
#' @param ... ignored
#' @export
#' @author Ute Hahn,  \email{ute@@imf.au.dk}


print.xydata <- function (x, ...)
{
  dimx <- x$nx[1]
  if (length(x$nx) > 1) for (i in 2: length(x$nx)) dimx <- paste(dimx, x$nx[i], sep="x")
  
  cat("xy-list with",dimx,"x-values and", x$ny,"sets of y-values,\n",
    "x-range:", range(x$x), " y-range:", range(x$y),"\n")
}



# @rdname xydata-internal
# @keywords internal
# @export
# @title Plotting defaults for xydata
# List of defaults for plotting \code{\link{xydata}}-objects
# @rdname dafaultsxydata
# @docType data
defaultoptions.xydata <- list (
  xlab = "x",
  ylab = "y",
  col = "black",
  col.indiv = NULL, # if NULL, use col for individuals - hope R no longer removes NULLs
  light.indiv = .66, # how to lighten up the color for the individual lines
  lwd.sum = 2,
  lwd.indiv = 1,
  lty.sum = "solid",
  lty.indiv = "solid",
  sumfun = "mean", # if NULL, do not plot a summary function
  envelope = NULL # if NULL, plot individual lines, 
  # otherwise one or two numbers are expected that specify pointwise envelopes
)

#' Update a list of options, e.g. for plotting
#' 
#' Compares the optional ... arguments with the list elements in \code{default}
#' and return an updated list. The function was written for setting plotting defaults
#' for xy-lists, but may be useful also for other lists.
#'
#' @param default the list to be updated
#' @param optlist optional list of updates
#' @param ... optional named pairs, updates for \code{default}
#' @return a list with same names as the elements of \code{default}
#' @export
# @seealso \code{\link{unusedoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

updateoptions <- function(default, optlist=NULL, ...)
{
  argu <- list(...)
  if(is.list(optlist)) argu <- c(argu, optlist) 
  result <- default
  if(length(argu) > 0)
  {
    namdef <- names(default)
    pmatch(names(argu), namdef) -> keys
    if (length(keys) > 0) {
      argna <- lapply(argu, function(z) if (is.null(z)) NA else z)
      resna <- lapply(result, function(z) if (is.null(z)) NA else z)
      for (ki in 1:length(keys)) if (!is.na(keys[ki])) resna[[keys[ki]]] <- argna[[ki]]
      result <- lapply(resna, function(z) if(is.function(z) || !is.na(z)) z else NULL)
      }  
  } 
  return(result)
}

#' Get options 
#' 
#' Update the options element in an \code{\link{xydata}} object or another list with an element \code{$options}, 
#' according to arguments
#' 
#' @param x list with element \code{options} to be updated
#' @param optlist optional list of updates
#' @param ... optional named pairs, updates for \code{default}
#' @return a list, copy of \code{x$options}, with updated elements
#' @details A copy of \code{x$options} is updated according to the remaining arguments, 
#' where arguments given as \code{...} have highest priority. The element names may be 
#' unambiguously abbreviated.
#' 
#' If no arguments are given, the \code{options} element of list \code{x} is returned as is.
#' @export
# @seealso \code{\link{unusedoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @examples
#' # load simulated example data
#' data(exampledata) 
#' str(xyda$options)
#' blue <- getoptions(xyda, col = "blue")
#' str(blue)
#' # abbreviating names and using a predefined list
#' bluegreen <- getoptions(xyda, blue, col.i = "green", light = .5)
#' str(bluegreen)

getoptions <- function(x, optlist=NULL, ...)
{
  return(updateoptions(updateoptions(x$options, optlist), ...))
}

#' Internal functions for class \code{xydata}
#'
#' Internal functions for dealing with objects of class \code{xydata}, and plotting options
#
# Return arguments matching a default list
# 
# Compares the optional ... arguments with the list elements in \code{default}
# and returns a list of all arguments with names that match names of elements in \code{default}. 
# The function was written for \code{defaultsoptions.xydata} but can also be 
# used for other lists.
#
# @param default the list to be updated
# @param ... named pairs, updates for \code{default}
# @return a list with elements with names that match those in \code{default}
# @export
#' @rdname xydata-internal
#' @keywords{internal}
# @seealso \code{\link{setoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

matchingoptions <- function(default = defaultoptions.xydata, optlist,...)
{
  argu <- list(...)
  namdef <- names(default)
  pmatch(names(argu), namdef) -> keys
  result <- argu
  matchkeys <- which(!is.na(keys))
  if (length(matchkeys) > 0)
    return(result[matchkeys]) else return(NULL)
}


# Return arguments not matching a default list
# 
# Compares the optional ... arguments with the list elements in \code{default}
# and returns a list of all arguments the names of which do not match names of elements in \code{default}. 
# The function was written for \code{defaultoptions.xydata} but can also be 
# used for other lists.
#
# @param default the list to be updated
# @param optlist optional list of options to be updated
# @param ... named pairs, updates for \code{default}
# @return a list with elements with names that differ from those in \code{default}
# @export
#' @rdname xydata-internal
#' @keywords internal
# @seealso \code{\link{setoptions}}
# @author Ute Hahn,  \email{ute@@imf.au.dk}

unusedoptions <- function(default = defaultoptions.xydata, optlist = NULL, ...)
{
  argu <- uniquelist(c(list(...), optlist))
  namdef <- names(default)
  pmatch(names(argu), namdef) -> keys
  result <- argu
  leftoverkeys <- which(is.na(keys))
  if (length(leftoverkeys) > 0)
    return(result[leftoverkeys]) else return(NULL)
}

# return unique elements
# @param xlist the list to be simplified
# @return a list with unique elements, priority: the first element counts
# @export
#' @rdname xydata-internal
#' @keywords{internal}
# @seealso \code{\link{setoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

uniquelist <- function(xlist) xlist[!duplicated(names(xlist))]

#
# Data documentation
#

#' @name exampledata
#' @aliases xyda xyda1 xyda2
#' @title Example data
#' @description Three data sets for use in the examples: objects of class \code{\link{xydata}},  
#' generated by simulation.
#' @docType data
# usage data(exampledata) 
#' @details Regression type data with normal distributed \eqn{y}-values. Mean values:
#' \tabular{ll}{
#' \code{xyda}  \tab{mean \eqn{y = x^2}}\cr
#' \code{xyda1} \tab{mean \eqn{y = 4 - x^2}}\cr
#' \code{xyda2} \tab{mean \eqn{y = 4.2 - 1.2 x^2 }}\cr
#' }
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
 NULL