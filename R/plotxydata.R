# plotting utilities and constants

# setting plot parameters -------------------------------------------------

#.plotparx <- list(log = "", main="", sub="", xlab="", ylab="",  axes =TRUE) 
.plotparams <- list(type = "p",  xlim = NULL, ylim = NULL,
                    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                    ann = par("ann"), axes = TRUE, frame.plot = TRUE,
                    panel.first = NULL, panel.last = NULL, asp = NA)
# parameters that may cause warnings in low level plotting functions, but not in plot.default

# lighten up a colour -----------------------------------------------------

# @param col color
# @param add coefficient on rgb, a single number
#' @rdname xydata-internal
#' @keywords internal
#' @export
# @details works on all devices that support rgb

lightcol <- function(col, light = 0){
  RGB <- pmin(light + (1 - light) * col2rgb(col) / 255, 1) # pmin just to be on the safe side
  rgb(RGB["red", ], RGB["green", ], RGB["blue", ])
}  

# plot an xydata object

#' Plot an xydata object
#'
#' Plots the individual functions  coerced in an object of class \code{\link{xydata}}. 
#' 
#' @param x the xydata to be plotted
#' @param ploptions optional list of plotting parameters, see the Details
#' @param includy optional numeric vector containing values that are to be included in the 
#' \code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#' @param add if \code{FALSE} (default), a new plot is started, if \code{TRUE}, adds to existing plot
#' @param ... further arguments
#' @details
#' Plotting parameters can be given as list \code{"ploptions"} or separately. If not
#' given explicitely, default values contained in the list \code{x$options} are used. 
#' The list of options in an \code{xydata}-object has elements
#' \tabular{ll}{
#' \code{col} \tab color for plotting the individual functions, defaults to "black"\cr
#' \code{light} \tab numeric between 0 and 1. Regulates how much the color of individual functions
#' is lightened up, 0 means no extra light. Defaults to 0. \cr
#' \code{lwd} \tab numeric, line width for individual function. Defaults to 1.\cr
#' \code{lty} \tab line type (character or numeric), defaults to "solid".\cr
#' \code{xlab, ylab} \tab character, axes labels, default to \code{"x"} and {"y"}.\cr
#' }
#' 
#' @S3method plot xydata 
#' @export
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @seealso \code{\link{getoptions}}
#' @examples
#' # load example data
#' data(exampledata)
#' plot(xyda)
#' # using a predefined list of options
#' blau <- list(col = "blue", light = .6)
#' plot(xyda, blau, main="mein blau", includey = -2)
#' # add mean
#' plot(mean(xyda), blau, light = 0, lwd = 2, add = TRUE)

plot.xydata <- function(x, ploptions = NULL, includey = NULL, add=F,  ...)
{
  if (length(x$nx) > 1) stop ("sorry, plotting of higher dimensional xydatas not yet supported")
 
  # allopt <- uniquelist(c(list(...), ploptions, x$options)) this does not allow lazy evaluation
  argu <- list(...)
  xopt <- updateoptions(updateoptions(x$options, ploptions), argu)
  allopt <- uniquelist(c(xopt, unusedoptions(xopt, argu)))
  
  if (!add) # make a plot window
  {  
    # set information for plotwindow
    if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includey)
    if(is.null(allopt$xlim)) allopt$xlim <- range(x$x)
    # Want type = "n"
    pargus <- updateoptions(.plotparams, allopt)
    pargus$type="n"
    do.call(plot.default, c(list(allopt$xlim, allopt$ylim), pargus))
  }
  # now do the plotting of envelopes and curves
 
  .grapar <- par(no.readonly = TRUE) 
  plopt <- updateoptions (.grapar, allopt)
  # plot individual lines
  if(allopt$light > 0) plopt$col <- lightcol(allopt$col, allopt$light)
    if (x$ny == 1) do.call(lines, c(list(x$x, x$y), plopt))
  else  apply(x$y, 2, function(yy) do.call(lines, c(list(x$x, yy), plopt)))
}  

#----------------------------------------------------


#' Plot summary of an xydata object
#'
#' Plots the individual functions or a pointwise envelope, as well as a summary function, 
#' of function values coerced in an object of class \code{\link{xydata}}. 
#' 
#' @param x the xydata to be plotted
#' @param ploptions optional list of plotting parameters, see the Details
#' @param includy optional numeric vector containing values that are to be included in the 
#' \code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#' @param add if \code{FALSE} (default), a new plot is started, if \code{TRUE}, adds to existing plot
#' @param ... further arguments
#' @details
#' Plotting parameters can be given as list \code{"floptions"} or seperately. If not
#' given explicitely, default values contained in the list \code{x$options} are used. 
#' The list of options has elements
#' \tabular{ll}{
#' \code{col} \tab color for plotting the summary function, defaults to "black"\cr
#' \code{col.indiv} \tab color for plotting individual functions or envelope. 
#' Defaults to NULL, then the same \code{col} is used as for the summary function\cr
#' \code{light.indiv} \tab numeric between 0 and 1. Regulates how much the color of individual functions
#' or envelope is lightened up, 0 means no extra light. Defaults to 0.66. \cr
#' \code{lwd.sum} \tab numeric, line width for summary function. Defaults to 2.\cr
#' \code{lwd.indiv} \tab numeric, line width for individual function. Defaults to 1.\cr
#' \code{lwd.sum} \tab numeric, line width for summary function. Defaults to 2.\cr
#' \code{lwd.indiv} \tab numeric, line width for individual function. Defaults to 1.\cr
#' \code{sumfun} \tab summary function, defaults to \code{NULL}\cr
#' \code{envelope} \tab defaults to \code{NULL}\cr
#' }
#' 
# The summary function given in \code{xy} is used, if not overridden by the parameter
# \code{sumfun}. By default this is the mean.
#' 
#' To suppress plotting of the summary function or the individual functions, 
#' set \code{lwd.sum=0} or \code{lwd.indiv=0}, respectively.
#' 
#' If an \code{envelope} is specified, the individual lines are not plotted. 
#' If \code{envelope} is a single number between 0 and 1, a central pointwise envelope covering
#' \code{envelope}*100 percent of the function values. If two numbers are given, they are used to
#' specify the lower and upper quantile used for the envelope.
#' @S3method plot xydata 
#' @export
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @seealso \code{\link{getoptions}}
#' @examples
#' # make a xydata-object first 
#' nx <- 5
#' ny <- 10
#' x <- seq(1, 2, len = nx)
#' # generate normal random variables with mean 4-2x and variance x^2
#' # as a matrix nx x ny
#' y <- t(sapply(x, function(s) rnorm(ny, mean = 4 - 2*s, sd = s)))
#' xyda <- xydata(x, y, sumfun = mean)
#' summaryplot(xyda)
#' 
#' # plot mean and median in a minimax envelope
#' summaryplot(xyda, envelope = 1)
#' summaryplot(xyda, envelope = NULL, sumfun = median, 
#'           add = TRUE, col = "red", lwd.indiv = 0)
#' 
#' # using a predefined list of options, 
#' # summary function will be plotted in black (default)
#' blau <- list(envelope = .9, col.indiv = "blue", light.indiv = .8)
#' summaryplot(xyda, blau)
#' # add individual lines, overriding options contained in xyli and blau
#' summaryplot(xyda, blau, env = NULL, light = 0.2, add = TRUE)

summaryplot <- function(x, ploptions = NULL, includy = NULL, add=F,  ...)
{
  if (length(x$nx) > 1) stop ("sorry, plotting of higher dimensional xydatas not yet supported")
 
  # allopt <- uniquelist(c(list(...), ploptions, x$options)) this does not allow lazy evaluation
  argu <- list(...)
  xopt <- updateoptions(updateoptions(x$options, ploptions), argu)
  allopt <- uniquelist(c(xopt, unusedoptions(xopt, argu)))
  
  if (!add) # make a plot window
  {  
    # set information for plotwindow
    if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includy)
    if(is.null(allopt$xlim)) allopt$xlim <- range(x$x)
    # Want type = "n"
    pargus <- updateoptions(.plotparams, allopt)
    pargus$type="n"
    do.call(plot.default, c(list(allopt$xlim, allopt$ylim), pargus))
  }
  # now do the plotting of envelopes and curves
 
  .grapar <- par(no.readonly = TRUE) 
  grapar <- updateoptions (.grapar, allopt)
  
  if (is.null(allopt$envelope))
  {
  # plot individual lines
    if (allopt$lwd.indiv > 0)
    {
      plopt <- grapar
      if(is.null(allopt$col.indiv)) plopt$col <- lightcol(allopt$col, allopt$light.indiv)
      else plopt$col <- lightcol(allopt$col.indiv, allopt$light.indiv)
      plopt$lwd <- allopt$lwd.indiv
      plopt$lty <- allopt$lty.indiv
          
      if (x$ny == 1) do.call(lines, c(list(x$x, x$y), plopt))
      else  apply(x$y, 2, function(yy) do.call(lines, c(list(x$x, yy), plopt)))
    }
  } else 
  {
    # envelopes wanted
    stopifnot(is.numeric(allopt$envelope))  
    if(length(allopt$envelope) == 1) quants <- c( 0.5 - allopt$envelope / 2, 0.5 + allopt$envelope / 2) 
       else quants <- range(allopt$envelope[1 : 2])
    # not much user input rubbish control here...
    yy <- apply(x$y, 1, quantile, probs =  quants)
    
    plopt <- grapar#grargu
    if(is.null(allopt$col.indiv)) plopt$col <- lightcol(allopt$col, allopt$light.indiv)
    else plopt$col <- lightcol(allopt$col.indiv, allopt$light.indiv)
    plopt$border <- NA
    do.call (polygon, c(list(c(x$x, rev(x$x)), c(yy[1, ], rev(yy[2, ]))), plopt))
  } 
  if (allopt$lwd.sum > 0)
  {
    # plot summary function
    if (!is.null(allopt$sumfun))
    {
      plopt <- grapar
      yy <- apply(x$y, 1, allopt$sumfun)
      plopt$col <- allopt$col # eigentlich unnötig, nur falls später col.sum eingeführt wird
      plopt$lwd <- allopt$lwd.sum
      plopt$lty <- allopt$lty.sum
      do.call(lines, c(list(x$x, yy), plopt))
    }
  }  
}


#' Plot an xydata object
#'
#' Plots the individual functions or a pointwise envelope, as well as a summary function, 
#' of function values coerced in an object of class \code{\link{xydata}}. 
#' 
#' @param x the xydata to be plotted
#' @param ploptions optional list of plotting parameters, see the Details
#' @param includy optional numeric vector containing values that are to be included in the 
#' \code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#' @param add if \code{FALSE} (default), a new plot is started, if \code{TRUE}, adds to existing plot
#' @param ... further arguments
#' @details
#' Plotting parameters can be given as list \code{"floptions"} or seperately. If not
#' given explicitely, default values contained in the list \code{x$options} are used. 
#' The list of options has elements
#' \tabular{ll}{
#' \code{col} \tab color for plotting the summary function, defaults to "black"\cr
#' \code{col.indiv} \tab color for plotting individual functions or envelope. 
#' Defaults to NULL, then the same \code{col} is used as for the summary function\cr
#' \code{light.indiv} \tab numeric between 0 and 1. Regulates how much the color of individual functions
#' or envelope is lightened up, 0 means no extra light. Defaults to 0.66. \cr
#' \code{lwd.sum} \tab numeric, line width for summary function. Defaults to 2.\cr
#' \code{lwd.indiv} \tab numeric, line width for individual function. Defaults to 1.\cr
#' \code{lwd.sum} \tab numeric, line width for summary function. Defaults to 2.\cr
#' \code{lwd.indiv} \tab numeric, line width for individual function. Defaults to 1.\cr
#' \code{sumfun} \tab summary function, defaults to \code{NULL}\cr
#' \code{envelope} \tab defaults to \code{NULL}\cr
#' }
#' 
# The summary function given in \code{xy} is used, if not overridden by the parameter
# \code{sumfun}. By default this is the mean.
#' 
#' To suppress plotting of the summary function or the individual functions, 
#' set \code{lwd.sum=0} or \code{lwd.indiv=0}, respectively.
#' 
#' If an \code{envelope} is specified, the individual lines are not plotted. 
#' If \code{envelope} is a single number between 0 and 1, a central pointwise envelope covering
#' \code{envelope}*100 percent of the function values. If two numbers are given, they are used to
#' specify the lower and upper quantile used for the envelope.
#' @S3method plot xydata 
#' @export
#' @seealso \code{\link{getoptions}}
#' @examples
#' # make a xydata-object first 
#' nx <- 5
#' ny <- 10
#' x <- seq(1, 2, len = nx)
#' # generate normal random variables with mean 4-2x and variance x^2
#' # as a matrix nx x ny
#' y <- t(sapply(x, function(s) rnorm(ny, mean = 4 - 2*s, sd = s)))
#' xyda <- xydata(x, y, sumfun = mean)
#' summaryplot(xyda)
#' 
#' # plot mean and median in a minimax envelope
#' summaryplot(xyda, envelope = 1)
#' summaryplot(xyda, envelope = NULL, sumfun = median, 
#'           add = TRUE, col = "red", lwd.indiv = 0)
#' 
#' # using a predefined list of options, 
#' # summary function will be plotted in black (default)
#' blau <- list(envelope = .9, col.indiv = "blue", light.indiv = .8)
#' summaryplot(xyda, blau)
#' # add individual lines, overriding options contained in xyli and blau
#' summaryplot(xyda, blau, env = NULL, light = 0.2, add = TRUE)

summaryplot <- function(x, ploptions = NULL, includy = NULL, add=F,  ...)
{
  if (length(x$nx) > 1) stop ("sorry, plotting of higher dimensional xydatas not yet supported")
 
  # allopt <- uniquelist(c(list(...), ploptions, x$options)) this does not allow lazy evaluation
  argu <- list(...)
  xopt <- updateoptions(updateoptions(x$options, ploptions), argu)
  allopt <- uniquelist(c(xopt, unusedoptions(xopt, argu)))
  
  if (!add) # make a plot window
  {  
    # set information for plotwindow
    if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includy)
    if(is.null(allopt$xlim)) allopt$xlim <- range(x$x)
    # Want type = "n"
    pargus <- updateoptions(.plotparams, allopt)
    pargus$type="n"
    do.call(plot.default, c(list(allopt$xlim, allopt$ylim), pargus))
  }
  # now do the plotting of envelopes and curves
 
  .grapar <- par(no.readonly = TRUE) 
  grapar <- updateoptions (.grapar, allopt)
  
  if (is.null(allopt$envelope))
  {
  # plot individual lines
    if (allopt$lwd.indiv > 0)
    {
      plopt <- grapar
      if(is.null(allopt$col.indiv)) plopt$col <- lightcol(allopt$col, allopt$light.indiv)
      else plopt$col <- lightcol(allopt$col.indiv, allopt$light.indiv)
      plopt$lwd <- allopt$lwd.indiv
      plopt$lty <- allopt$lty.indiv
          
      if (x$ny == 1) do.call(lines, c(list(x$x, x$y), plopt))
      else  apply(x$y, 2, function(yy) do.call(lines, c(list(x$x, yy), plopt)))
    }
  } else 
  {
    # envelopes wanted
    stopifnot(is.numeric(allopt$envelope))  
    if(length(allopt$envelope) == 1) quants <- c( 0.5 - allopt$envelope / 2, 0.5 + allopt$envelope / 2) 
       else quants <- range(allopt$envelope[1 : 2])
    # not much user input rubbish control here...
    yy <- apply(x$y, 1, quantile, probs =  quants)
    
    plopt <- grapar#grargu
    if(is.null(allopt$col.indiv)) plopt$col <- lightcol(allopt$col, allopt$light.indiv)
    else plopt$col <- lightcol(allopt$col.indiv, allopt$light.indiv)
    plopt$border <- NA
    do.call (polygon, c(list(c(x$x, rev(x$x)), c(yy[1, ], rev(yy[2, ]))), plopt))
  } 
  if (allopt$lwd.sum > 0)
  {
    # plot summary function
    if (!is.null(allopt$sumfun))
    {
      plopt <- grapar
      yy <- apply(x$y, 1, allopt$sumfun)
      plopt$col <- allopt$col # eigentlich unnötig, nur falls später col.sum eingeführt wird
      plopt$lwd <- allopt$lwd.sum
      plopt$lty <- allopt$lty.sum
      do.call(lines, c(list(x$x, yy), plopt))
    }
  }  
}
