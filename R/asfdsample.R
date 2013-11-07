# Convert a funsample into an fdsample

#'@title Coerce to an fdsample
#'@description Coerce a \code{\link{funsample}} object to a \code{\link{fdsample}}
#'object.
#'@details Convert a sample of functions into an \code{fdsample} by approximation
#'in the argument values. Argument values are obtained by dividing the given interval 
#'\code{argrange} into \code{nsteps} parts.
#'
#'@param x the \code{\link{funsample}} object to be converted.
#'@param ... parameters to be passed to  the generator \code{\link{fdsample}}.
#'@param argrange numeric vector of length 2, range of the argument values.
#'@param nsteps the number of intervals the argument range is divided into.
#'@export
#'@examples
#'#rather artificial example: plot sin(x), cos(x), and their mean
#'myfuns <- funsample(list(sin = sin, cos = cos), arglim = c(0, 2*pi)) 
#'myfdsample <- as.fdsample(myfuns, xlab ="x", ylab = "sin(x) and cos(x)")
#'summaryplot(myfdsample)

as.fdsample <- function(x, ..., argrange = attr(x, "arglim"), nsteps = 200) {
  stopifnot(is.funsample(x))
  argval <- seq(argrange[1], argrange[2], length.out = nsteps + 1)
  fval <- x(argval)
  fdsample(argval, fval, ...)
}
  