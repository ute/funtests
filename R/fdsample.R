####################################################
#
# fdsample - a data type for functional data
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
#  klist into fdsample mit nur 1 x-variable
# interpolate an array of functions
# plotting parameters are extra objects (list) fdsampleplotparm ? oder einfach listen
# flipp1 <- fdsamplepar(col="blue", default=.flippdefault) # default values
# 

#' Check whether object is a function sample
#' 
#' Checks whether object is of type \code{fdsample}.
#' @param x any R-object
#' @return \code{TRUE}, if \code{x} belongs to class \code{fdsample}, otherwise \code{FALSE}
#' @export
# @author Ute Hahn,  \email{ute@@imf.au.dk}

is.fdsample <- function(x) inherits(x, "fdsample")

# fdsample is a list with elements
#'Make a functional data sample
#'
#'Generate an object of class \code{fdsample}.
#'
#'@param args  numeric, array, the function arguments, dimension dimarg 
#'@param fvals  numeric, array of function values, dimension: dimarg x groupsize 
#@param xlab character, x-axis label for plotting
#@param ylab character, y-axis label
#@param name character, short label, used as title when plotting
#@param descr character, description for printing
#@param optlist optional list of (plot) options, see \code{\link{plot.fdsample}}
#'@param ... predefined plot options or \code{\link{style}}s, see the details 
#'and \code{\link{plot.fdsample}}
# @param fun the summary function to be applied on the values of \code{y}, defaults to \code{mean}
#'
#'@details Predefining plot options may be particularly useful for plot 
#'annotation, i.e. \code{xlab}, \code{ylab} and \code{main}. They may be also given
#'as a \pkg{plottools}-\code{\link{style}}. Options with same name are overriden
#'from left to right, i.e., the last one given counts.
#'@return a list with elements 
#'\tabular{ll}{ 
#'\code{args} \tab{the function arguments}
#'\cr\code{fvals}  \tab{the function value samples}
#'\cr\code{dimarg} \tab{integer, dimension of \code{x}}
#'\cr\code{groupsize} \tab{integer, number of functions y included in the list}
#'\cr\code{options} \tab list of plot options, see \code{\link{plot.fdsample}}
#'}
#'@export
# @author Ute Hahn,  \email{ute@@imf.au.dk}


fdsample <- function(args, fvals, ...) #fun = mean, ...)
{
  xa <- as.array(args)
  dimarg <- dim(xa)
  ya <- as.array(fvals)
  groupsize <- ifelse (length(dim(xa)) == length(dim(ya)),
                       1, dim(ya)[length(dim(ya))])
  my <- ifelse (groupsize == 1, dim(ya), dim(ya)[-length(dim(ya))])
  if (any(dimarg != my)) stop("dimensions do not match")
  options <- style(...)
  xy <- list(args = xa, 
             fvals = ya,
             dimarg = dim(xa),
             groupsize = groupsize,
             options = options)
  class(xy) <- "fdsample"
  return(xy)
}



#' Extract or replace subset of an fdsample
#' 
#' Curves (rows of y-values) are extracted from or replaced in the sample.
#' 
#'@rdname extract.fdsample
# @name extract.fdsample 
#'@aliases [.fdsample [<-.fdsample
#'@S3method [ fdsample
#'@method [ fdsample
# @usage x[i, j, drop, ...]
#'@export
#'@param x an object of class \code{"fdsample"}. 
#'@param i subset index.
# @param j,drop,... ignored.
#'@seealso \code{\link{fdsample}} for details on the class.
# @author Ute Hahn,  \email{ute@@imf.au.dk}


"[.fdsample" <- function(x, i) #, j, drop, ...) 
  {
    xx <- as.array(x$args)
    if (length(dim(xx))>1) stop("sorry, not implemented yet for higher dimensions")
    yy <- x$fvals[,i]
    opt <- x$options
    newfd <- fdsample(xx, yy, opt)
    if (!is.null(comment(x))) 
      comment(newfd) <- c(comment(x), "\nsubset")
    return(newfd)
  }

#'@rdname extract.fdsample
#'@S3method [<- fdsample
# @method [<- fdsample
#'@usage \method{[}{fdsample} (x, i) <- value 
#'@export
#'@param value Replacement for the subset, an array or an fdsample object. 
#'@details Currently only possible if \code{x$args} is one-dimensional.
#'Replacement y-values have to be of same dimension as the original.

"[<-.fdsample" <- function(x, i, value)# j, value) 
  {
    xx <- as.array(x$args)
    if (length(dim(xx))>1) stop("sorry, not implemented yet for higher dimensions")
    yy <- x$fvals
    if(is.array(value)) yy[, i] <- value 
    else if(is.fdsample(value)) yy[, i] <- value$fvals
    else stop("can only replace with vectors or fdsample objects")
    opt <- x$options
    newfd <- fdsample(xx, yy, opt)
    if (!is.null(comment(x))) 
      comment(newfd) <- c(comment(x), "\nmodified")
    return(newfd)
  }

#'@title range of function values
#'@description Range of \code{fvals} contained in an \code{fdasample} object, 
#'purpose: convenient way to find plot y-limits.
#'@param x the data to be inspected
#'@param includy anything to be included in the range
#'@return Numeric vector of length two.
#'@export
# @rdname fdsample-internal
# @keywords {internal}
# @author Ute Hahn,  \email{ute@@imf.au.dk}

yrange <- function(x, includy = NULL) 
  range(c( range(x$fvals[is.finite(x$fvals)]), includy))

#'Print brief details of an xy-list
#'
#'Prints a very short summary of the \code{fdsample}-object in the argument
#'
#'@S3method print fdsample
#'@method print fdsample
#'@param x object of type \code{\link{fdsample}}
#'@param ... ignored
#'@export
# @author Ute Hahn,  \email{ute@@imf.au.dk}


print.fdsample <- function (x, ...)
{
  dimx <- x$dimarg[1]
  if (length(x$dimarg) > 1) for (i in 2: length(x$dimarg)) dimx <- paste(dimx, x$dimarg[i], sep="x")
  
  cat("fdsample with",dimx,"arg-values and", x$groupsize,"sets of function values,")
  if (dimx > 0) 
    cat("\narg-range:", range(x$args), " fval-range:", range(x$fvals),"\n")
  else 
    cat(" ... completely empty!\n")
  
  if (!is.null(comment(x))) cat("comment:",comment(x))
}



# @rdname fdsample-internal
# @keywords internal
#'@export
# @title Plotting defaults for fdsample
# List of defaults for plotting \code{\link{fdsample}}-objects
#'@rdname fdsample-internal
#'@aliases defaultoptions.fdsample
#'@docType data
defaultoptions.fdsample <- list (
  xlab = "t",
  ylab = "X(t)",
  main = "",
  light = 0, # how to lighten up the color for the individual lines
  col = NULL, # use default graphic parameters
  lwd = NULL,
  lty = NULL
)  

