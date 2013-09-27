# envelope of an xy data set

#'@title Calculate pointwise envelope of an xy-data set
#'
#'@description Calculates symmetric or non symmetric pointwise envelopes of 
#'the \eqn{y}-values
#'
#'@param xy object of class \code{\link{xydata}}
#'@param prob numeric, covering probability for the envelope, see Details.
#'@param multi currently ignored
#'@param lightup a number between 0 and 1, regulates brightness of color in plot.
#'@param ... arguments for updating the \code{options} list in the result.
#'@return an object of class \code{envelope}, which is essentially an \code{xydata}
#'object with own \code{plot} method.
#'
#'@details The envelope is specified as follows:
#'If \code{prob} is a single number between 0 and 1, a central pointwise 
#'envelope covering \code{prob}*100 percent of the function values is calculated. 
#'If two numbers are given, they are used to specify the lower and upper quantile 
#'used for the envelope.
#'
#'A default value for the brightness of the color is calculated from the options in \code{xy} 
#'and the argument \code{lightup}, namely
#'lightup + (1-lightup)*xy$options$light
#\code{light = ((0.1 + xy$options$light) / 1.1)^(1 - lightup)}.
#'Thus \code{lightup = 0} results in retaining original brightness, and \code{lightup = 1}
#'in white color. Can be overridden by the \code{...} arguments.
#' @export
#' @examples
#' str(envelope(xyda, 1, col = "blue"))
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
#'
envelope <- function (xy, prob = 1, multi = FALSE, lightup = .5, ...) 
{
  if(length(prob) == 1) quants <- c( 0.5 - prob / 2, 0.5 + prob / 2) 
  else quants <- range(prob)
  # not much user input rubbish control here...
  result <- quantile(xy, probs = quants)
  opt <- result$options
  opt$light = lightup + (1-lightup)*xy$options$light
  result$options <- updateoptions(opt, ...)
  class(result) <- c("envelope", class(xy))
  attr(result, "prob") <- prob
  return(result)
}  



#' Plot an envelope object
#'
#' Plots an object of class \code{\link{envelope}}. 
#' 
#' @param x the envelop to be plotted
#' @param ploptions optional list of plotting parameters, see the Details
#' @param includy optional numeric vector containing values that are to be included in the 
#' \code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#' @param add if \code{FALSE} (default), a new plot is started, if \code{TRUE}, adds to existing plot
#' @param ... further arguments
#' @details
#' Plotting parameters can be given as list \code{"ploptions"} or separately. If not
#' given explicitely, default values contained in the list \code{x$options} are used. 
#' The following elements in the list of options in the \code{envelope}-object are used:
#' \tabular{ll}{
#' \code{col} \tab color, \cr
#' \code{light} \tab numeric between 0 and 1. Regulates how much the color of 
#' is lightened up, 0 means no extra light. \cr
#' \code{xlab, ylab} \tab character, axes labels, default to \code{"x"} and {"y"}.\cr
#' }
#' @S3method plot envelope
#' @export
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @examples
#' # load example data and calculate 90 % envelope
#' data(exampledata)
#' envy <- envelope(xyda, prob = .9, lightup = .9)
#' # using a predefined list of options
#' blau <- list(col = "blue")
#' plot(envy, blau, main="mein blau", includey = -2)
#' # add lines and mean
#' plot(xyda, blau, light = 0.4, add = TRUE)
#' plot(mean(xyda), blau, light = 0, lwd = 2, add = TRUE)
#' 

plot.envelope <- function(x, ploptions = NULL, includey = NULL, add=F,  ...)
{
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
  plopt$border <- NA
  do.call (polygon, c(list(c(x$x, rev(x$x)), c(x$y[ ,1], rev(x$y[ ,2]))), plopt))
}  