# plotting utilities and constants

# setting plot parameters -------------------------------------------------

#.plotparx <- list(log = "", main="", sub="", xlab="", ylab="",  axes =TRUE) 
#.plotparams <- list(type = "p",  xlim = NULL, ylim = NULL,
#                    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
#                    ann = par("ann"), axes = TRUE, frame.plot = TRUE,
#                    panel.first = NULL, panel.last = NULL, asp = NA,
#                    mgp = par("mgp"))
# parameters that may cause warnings in low level plotting functions, but not in plot.default

# lighten up a colour -----------------------------------------------------

# @param col color
# @param add coefficient on rgb, a single number
# @rdname fdsample-internal
# @keywords internal
# @export
# @details works on all devices that support rgb
#
#lightcol <- function(col, light = 0){
#  RGB <- pmin(light + (1 - light) * col2rgb(col) / 255, 1) # pmin just to be on the safe side
#  rgb(RGB["red", ], RGB["green", ], RGB["blue", ])
#}  

# plot an fdsample object

#'Plot an fdsample object
#'
#'Plots the individual functions  coerced in an object of class \code{\link{fdsample}}. 
#'
#'@param x the fdsample to be plotted
#'@param includy optional numeric vector containing values that are to be included in the 
#'\code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#'@param ... further arguments for controlling the plot.
#'@details
#'This method requires that the functional data are curves, i.e. that element 
#'\code{arg} is a vector. By default, lines are plotted. This may be modified by
#'giving an argument \code{type} which is passed to the \code{\link{matplot}} function
#'used to plot the curves. Thus, \code{type = "p"} plots points, and \code{type = "b"}
#'plots both points and lines.
#'
#'Brightness / opacity of the curves can be controlled by setting \code{alpha} 
#'to a number between 0 and 1, see the description of function \code{\link{alphacol}}
#'in package \code{plottools.}
#'
#'The plot method \code{plot.fdsample} works like the function \code{\link{splot}}
#'from package \code{plottools}. In particular,  plot parameters can be given as 
#'\code{"\link{style}"} lists or separately. 
#'
#@S3method plot fdsample 
#'@method plot fdsample 
#'@export
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#'@seealso \code{\link{alphacol}} on defining transparent colors, and
#'\code{\link{splot}} for the evaluation of plot parameters.
#'@examples
#'# load example data
#'data(ExampleData)
#'plot(fuda)
#'# using a predefined list of options
#'blau <- style(col = "blue", alpha = 0.4)
#'plot(fuda, blau, includy = -2)
#'# add mean
#'plot(mean(fuda), col = "blue", lwd = 2, add = TRUE)

plot.fdsample <- function(x, ..., includy = NULL)
{
  if (length(x$dimarg) > 1) 
    stop ("sorry, plotting of higher dimensional fdsamples not yet supported")
  allopt <- style(x$options, ..., NULL.rm = TRUE)
  if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includy)
  if(is.null(allopt$xlim)) allopt$xlim <- range(x$args[is.finite(x$args)])

  # adjust plot options
  allopt <- updateJoin(c(par(c("col", "lty")), type = "l"), allopt)
  if(!is.null(allopt$alpha))
    allopt$col <- alphacol(allopt$col, allopt$alpha)
  if(is.null(allopt$type)) allopt$type <- "l"
  
  plopt <- matching(allopt, c(.graphparams, .plotparams, add = T))
  do.call(matplot, c(list(x = x$args, y = x$fvals), plopt))
  invisible()
}  
