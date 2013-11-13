#'Plot a funsample object
#'
#'Plots the individual functions contained in an object of class
#' \code{\link{funsample}} in one plot.
#'
#'@param x the funsample to be plotted
#'@param includy optional numeric vector containing values that are to be included in the
#'\code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#'@param ... further arguments for controlling the plot.
#'@details
#'This method requires that the functions in code{x} are univariate, which however 
#'is not checked. 
#'
#'Brightness / opacity of the curves can be controlled by setting \code{alpha}
#'to a number between 0 and 1, see the description of function \code{\link{alphacol}}
#'in package \code{plottools.}
#'
#'The plot method \code{plot.fdsample} works like the function \code{\link{splot}}
#'from package \code{plottools}. In particular,  plot parameters can be given as
#'\code{"\link{simplist}"}s or separately.
#'
#@S3method plot funsample
#'@method plot funsample
#'@export plot.funsample
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#'@seealso \code{\link{alphacol}} on defining transparent colors, and
#'\code{\link{splot}} for the evaluation of plot parameters.
#'@examples
#'myfuns <- funsample(list(sin = sin, cos = cos), arglim = c(0, 2*pi)) 
#          main = "my trigo functions")
#'plot(myfuns)
#'# using a predefined list of options, from plutils package
#'require(plutils)
#'blau <- simplist(col = "blue", alpha = 0.4)
#'plot(myfuns, blau, includy = -2)
#'# plot options can also be included when a funsample is generated:
#'trigostyle <- list(sin = simplist(col = "red", lty = "dashed"), 
#'                   cos = simplist(col = "green"))
#'myfuns <- funsample(list(sin = sin, cos = cos), arglim = c(0, 2*pi), trigostyle)
#'plot(myfuns)                   

plot.funsample <- function(x, ..., includy = NULL)
{
  allopt <- simplist(attr(x,"options"), ..., .NULL.rm = TRUE)
  if(is.null(allopt$xlim)) 
    allopt$xlim <- range(attr(x, "arglim"))
  attr(x, "arglim") <- allopt$xlim
  if(is.null(allopt$ylim)) 
    allopt$ylim <- yrange(as.fdsample(x), includy)
 
  funz <- attr(x,"funs")
  lplot(funz, allopt)
#   for (foo in funz)
#   {
#     if (is.urfunction(foo))  
#       plopt <- simplist(attr(foo,"options"), allopt))
#     else 
#       plopt <- allopt
#   }
#   # adjust plot options
#   if(!is.null(allopt$alpha))
#     allopt$col <- alphacol(allopt$col, allopt$alpha)
#   
#   plopt <- matching(allopt, c(.graphparams, .plotparams, add = T))
#   do.call(matplot, c(list(x = x$args, y = x$fvals), plopt))
#   invisible()
}

# @rdname funsample-internal
#'@keywords internal
#'@export
#'@title Plotting defaults for funsample
#'List of defaults for plotting \code{\link{funsample}}-objects
#'@rdname funsample-internal
#'@aliases defaultoptions.funsample
#'@docType data
defaultoptions.funsample <- simplist (
  xlab = "t",
  ylab = "X(t)",
  main = "",
  col = NULL, # use default graphic parameters
  lwd = NULL,
  lty = NULL
)