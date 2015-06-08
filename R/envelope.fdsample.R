# envelope of a functional data sample

#'@title Calculate pointwise envelope of a functional data sample
#'
#'@description Calculates symmetric or non symmetric pointwise envelopes of
#'the function values in an \code{fdsample} object.
#'
#'@param x object of class \code{\link{fdsample}}
#'@param prob numeric, covering probability for the envelope, see Details.
#@param multi currently ignored
#'@param ... arguments for updating the \code{options} list in the result.
#'@param lightup a number between 0 and 1, regulates brightness of color in plot.
#'@return An object of class \code{envelope}, which is essentially an \code{fdsample}
#'object with own \code{plot} method.
#'
#'@details The envelope is specified as follows:
#'If \code{prob} is a single number between 0 and 1, a central pointwise
#'envelope covering \code{prob}*100 percent of the function values is calculated.
#'If two numbers are given, they are used to specify the lower and upper quantile
#'used for the envelope.
#'
#'A default value for the brightness of the color is calculated from any \code{alpha}
#'argument in the plot method and the argument \code{lightup}, namely the
#'\eqn{\alpha}-value is corrected to \code{(1-lightup) * alpha}. If not given, \eqn{\alpha}
#'is set to one.
#'Thus \code{lightup = 0} results in retaining original brightness, and \code{lightup = 1}
#'in white color.
# Can be overridden by the \code{...} arguments.
#' @export
#' @examples
#' data(ExampleData)
#' str(pwEnvelope(fuda, 1, col = "blue"))
# @author Ute Hahn,  \email{ute@@imf.au.dk}
pwEnvelope <- function (x, prob = 1, ..., lightup = 0.5)
{
  if(length(prob) == 1) quants <- c( 0.5 - prob / 2, 0.5 + prob / 2)
  else quants <- range(prob)
  # not much user input rubbish control here...
  result <- quantile(x, probs = quants, ..., lightup = lightup, type = 1)
  #result$options <-  simplist(result$options, ..., lightup = lightup)
  class(result) <- c("fdenvelope", class(x))
  attr(result, "prob") <- prob
  comment(result) <- c(comment(x), paste("\n",round(prob*100),"%-envelope"))
  return(result)
}



#'Plot an envelope object
#'
#'Plots an object of class \code{fdenvelope}.
#'
#'@param x the envelop to be plotted
#@param ploptions optional list of plotting parameters, see the Details
#'@param includy optional numeric vector containing values that are to be included in the
#'\code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#@param add if \code{FALSE} (default), a new plot is started, if \code{TRUE}, adds to existing plot
#'@param ... further arguments
#'@details
#'Plotting parameters can be given as \code{\link{simplist}} or separately. If not
#'given explicitely, default values contained in the list \code{x$options} are used.
#'The following elements in the list of options in the \code{envelope}-object are used:
#'\tabular{ll}{
#'\code{col} \tab color, \cr
#'\code{lightup} \tab numeric between 0 and 1. Regulates how much the color of
#'is lightened up,\cr\tab 0 means no extra light. \cr
#'\code{xlab, ylab} \tab character, axes labels, default to \code{"x"} and \code{"y"}. \cr
#'}
#@S3method plot fdenvelope
#'@method plot fdenvelope
#'@export
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#'@examples
#'# load example data and calculate 90 % envelope
#'data(ExampleData)
#'envy <- pwEnvelope(fuda, prob = .9, lightup = .5)
#'# using a predefined list of options, from plutils package
#'require(plutils)
#'blau <- simplist(col = "blue", alpha = .6)
#'plot(envy, blau, main="mein blau", includy = -2)
#'# add lines and mean
#'plot(fuda, blau, add = TRUE)
#'plot(mean(fuda), blau, alpha = 1, lwd = 2, add = TRUE)
#'

plot.fdenvelope <- function(x, ..., includy = NULL)
{
  if (length(x$dimarg) > 1)
    stop ("sorry, plotting of higher dimensional envelopes not yet supported")

  allopt <- simplist(x$options, ..., .NULL.rm = TRUE)

  if (is.null(allopt$add) || !allopt$add)
  {
    # make new plot
    if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includy)
    if(is.null(allopt$xlim)) allopt$xlim <- range(x$args[is.finite(x$args)])
    pargus <- matching(allopt, .plotparams)
    pargus$type="n"
    do.call(plot.default, c(list(allopt$xlim, allopt$ylim), pargus), quote = TRUE)
  }

  # now do the plotting of envelopes and curves
  # adjust plot options: have a colour, at least
  allopt <- updateJoin(par("col"), allopt)
  alpha <- ifelse(!is.null(allopt$alpha), allopt$alpha, 1)
  lightup <- ifelse(!is.null(allopt$lightup), allopt$lightup, 0.5)
  if (is.null(allopt$col)) allopt$col <- par("col")
  allopt$col <- alphacol(allopt$col, alpha * (1 - lightup))

  plopt <- matching(allopt, .graphparams)
  plopt$border <- NA
  do.call (polygon, c(list(c(x$args, rev(x$args)), c(x$fvals[ ,1], rev(x$fvals[ ,2]))), plopt), quote = TRUE)
}

