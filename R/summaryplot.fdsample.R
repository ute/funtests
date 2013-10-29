#----------------------------------------------------


#'Plot summary of an fdsample object
#'
#'Plots the individual functions or a pointwise envelope, as well as a summary function,
#'of function values coerced in an object of class \code{\link{fdsample}}.
#'
#'@param x the fdsample to be plotted
#@param ploptions list of optional plotting parameters, see the Details.
#'@param sumfun the summary function. If NULL, no summary function is plotted.
#'Defaults to \code{"mean"}.
#'@param fopt list of options for \code{sumfun}.
# @param sumstyle optional list of plotting parameters for the summary function, see the Details.
#'@param envprob coverage of the envelope, defaults to NULL: no envelope is plotted.
# @param envstyle optional list of plotting parameters for the envelope, see the Details.
#'@param includy optional numeric vector containing values that are to be included in the
#'\code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#'@param add if \code{FALSE} (default), a new plot is started, if \code{TRUE}, adds to existing plot
#'@param ... further arguments
#'@details
#'This function is a method to the generic \code{summaryplot} from package \code{plutils},
#'therefore it is necessary to \code{require(plutils)}.
#'
#'Plotting parameters for the individual functions can be given as a \code{simplist} 
#'(from the \code{plutils} package)
#'or separately. If not given explicitely, default values contained in the list
#'\code{x$options} are used. If these are set to \code{NULL}, the global graphic
#'parameters as reported by \code{\link{par}} are assumed.
#'
#'\emph{Individual functions} are plotted using the parameters
#'\tabular{ll}{
#'\code{col} \tab character or numeric, color,
#'\cr\code{lwd} \tab numeric, line width,
#'\cr\code{lty} \tab character or numeric, line type,
#'\cr\code{alpha} \tab numeric between 0 and 1, alpha value, controls colour opacity / brightness, defaults to 1.
#'}
#'For the \emph{summary function}, the parameters are
#'\tabular{ll}{\code{col.sum} \tab character or numeric, color, defaulting to \code{col}
#'\cr\code{lwd.sum} \tab numeric, line width, defaulting to \code{2 * lwd},
#'\cr\tab thus the summary function always plots stronger than the individual curves,
#'\cr\code{lty.sum} \tab character or numeric, line type, defaults to \code{lty}.\cr
#'}
#'Plotting of \emph{envelopes} is controlled by the parameters
#'\tabular{ll}{
#'\code{col.env} \tab character or numeric, color, defaults to \code{col}
#'\cr\code{alpha.env} \tab numeric between 0 and 1, alpha value, defaults to
#'Defaults to \code{0.5*alpha},
#'\cr\tab thus the envelope will always plot lighter than individual functions.
#'\cr
#'}
#'The summary function given in \code{x} is used, if not overridden by the parameter
#'\code{sumfun}. By default this is the mean.
#'
#'To suppress plotting of the summary function or the individual functions,
#'set \code{sumfun = NULL} or \code{lwd = 0}, respectively.
#'
#'If \code{envprob} is a single number between 0 and 1, a central pointwise envelope covering
#'\code{envprob}*100 percent of the function values is plotted. If two numbers are given, they are used to
#'specify the lower and upper quantile used for the envelope.
#'To suppress plotting of an envelope, let \code{envprob = NULL}.
#'
#@importFrom plutils summaryplot
#@S3method summaryplot fdsample
#'@method summaryplot fdsample
#'@export summaryplot.fdsample
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#@seealso \code{\link{getoptions}}
#'@examples
# # make a fdsample-object first
# n <- 5
# m <- 10
# x <- seq(1, 2, len = n)
#
# # generate normal random variables with mean 4-2x and variance x^2
# # as a matrix n x m
#
# y <- t(sapply(x, function(s) rnorm(m, mean = 4 - 2*s, sd = s)))
# fuda <- fdsample(x, y, sumfun = mean)
# summaryplot(fuda)
#
# # require plutils which contains the generic to "summaryplot"
#' require(plutils)
#'  # load data, containing an example set "fuda"
#' data(ExampleData)
#'
#' # plot mean and median in a minimax envelope
#'
#' summaryplot(fuda, envprob = 1)
#' summaryplot(fuda, envprob = NULL, sumfun = median,
#'           add = TRUE, col = "red", lwd = 0)
#'
#' # using a predefined list of options,
#' # summary function will be plotted in black (default)
#'
#' blau <- simplist(col = "blue", alpha.env = .8, col.smf = "green")
#' summaryplot(fuda, sumfun = "mean", envprob = .8, blau, alpha = .7)
#' # adds individual lines, overriding light options contained in fuda and blau



summaryplot.fdsample <- function(x, ...,
                        sumfun = "mean", fopt = list(),
                        envprob = NULL,
                        includy = NULL, add=F)
{
  if (length(x$dimarg) > 1) stop ("sorry, plotting of higher dimensional fdsamples not yet supported")

  # allopt <- uniquelist(c(list(...), ploptions, x$options)) this does not allow lazy evaluation
   dots <- simplist(...)
  allopt <- simplist(simplist(alpha = 1, col.smf = NULL, lwd.smf = NULL,
                        lty.smf = NULL, col.env = NULL, alpha.env = NULL),
                  x$options, dots, .NULL.rm = FALSE)
  if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includy)
  if(is.null(allopt$xlim)) allopt$xlim <- range(x$args[is.finite(x$args)])

  # suppress curves by setting line width to 0
  plotcurves <- TRUE
  if(!is.null(allopt$lwd))
    if (allopt$lwd == 0) {
      plotcurves <- FALSE
      allopt$lwd <- NULL
      allopt <- c(allopt, list(lwd = NULL))
      # needs to be that awkward because R kills NULLs
    }

  if (!add) # make a plot window
  {
    # Want type = "n"
    pargus <- matching(allopt, .plotparams)
    pargus$type="n"
    do.call(plot.default, c(list(allopt$xlim, allopt$ylim), pargus))
  }

  # adjust plot options: fill in necessary defaults
  allopt <- updateJoin(par("col", "lty", "lwd"), allopt)
  firstclass(allopt) <- "simplist"

  #plopt <- matching(allopt, .graphparams, .plotparams,
  #                  list(alpha = 1, add = TRUE), plot.fdsample)

  if (!is.null(envprob))
  {
    eplopt <- within(allopt, {
              if (!is.null(col.env)) col <- col.env
              if (!is.null(alpha.env)) alpha <- alpha.env
              else alpha <- 0.5*alpha
    })
    envy <- pwEnvelope(x, prob = envprob)
    plot(envy, eplopt, add = TRUE)
  }

  if (plotcurves)  plot(x, allopt, add = TRUE)

  if (!is.null(sumfun))
  {
    splopt <- within(allopt, {
              if (!is.null(col.smf)) col <- col.smf
              if (!is.null(lty.smf)) lty <- lty.smf
              if (!is.null(lwd.smf)) lwd <- lwd.smf else lwd <- 2 * lwd
              alpha = 1
    })
    sufu <- apply.fdsample(x, sumfun, fopt)
    plot(sufu, splopt, add = TRUE)
  }

}