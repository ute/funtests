# univariate real valued functions

#'@title urfunctions - univariate real valued functions
#'@description Coercing to, and checking \code{"urfunction"}s
#'@param fun a function that is to be turned into an \code{urfunction}
#'@param ... additional parameters to be stored in attribute \code{"options"}, 
#'in particular options for plotting. Can be given as \code{\link{simplist}}s.
#'@details \code{urfunction} adds additional information (plot options)
#'and the class \code{"urfunction"} to a function \code{foo}. It is assumed that the 
#'function \code{foo} takes only one real valued argument, and returns a real value.
#'To ensure this is left to the user, since it cannot be checked for primitives.
#'
#'\code{"urfunction"} objects have an attribute \code{"options"} which is a \code{simplist}
#'of options used for plotting, in particular axis labels and the title (\code{main}).
#'It contains an additional element \code{legendtxt} which is used when the function is
#'plotted as part of a \code{funsample}. This element, as well as all other elements of
#'\code{"options"}, can be set as argument (\ldots). It defaults to \code{ylab}.
#'@export
#'@examples
#'str(urfunction(sin))
#'str(urfunction(sin, legendtxt = "sinus", col = 2))
#
urfunction <- function(fun, ...) {
  stopifnot(is.function(fun))
  funame <- deparse(substitute(fun))
  funfrmls <- formals(fun)
  argname <- if (!is.null(funfrmls)) names(funfrmls)[1] else "x"
  # TODO need to find ylim for approxfuns
  options <- simplist(xlab = argname, 
                      main = "", ..., .NULL.rm = TRUE)
  ylab <- paste(funame, "(", options$xlab, ")", sep = "")
  options <- simplist(ylab = ylab, legendtxt = ylab, options)
  # TODO here need to make a copy in the case of primitives
  urfu <- fun
  firstclass(urfu) <- "urfunction"
  attr(urfu, "options") <- options
  urfu
}



#'@rdname urfunction
#'@param x an \code{R} object to be checked.
#'@description \code{is.urfunction} returns \code{TRUE} if \code{x} is an object
#'of class \code{urfunction}
#'@export
is.urfunction <- function(x) {
  inherits(x, "urfunction")
}
