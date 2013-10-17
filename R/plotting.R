# plotting utilities and constants

# setting plot parameters -------------------------------------------------

#.plotparx <- list(log = "", main="", sub="", xlab="", ylab="",  axes =TRUE) 
.plotparams <- list(type = "p",  xlim = NULL, ylim = NULL,
                    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
                    ann = par("ann"), axes = TRUE, frame.plot = TRUE,
                    panel.first = NULL, panel.last = NULL, asp = NA,
                    mgp = par("mgp"))
# parameters that may cause warnings in low level plotting functions, but not in plot.default

# lighten up a colour -----------------------------------------------------

# @param col color
# @param add coefficient on rgb, a single number
#' @rdname fdsample-internal
#' @keywords internal
#' @export
# @details works on all devices that support rgb

lightcol <- function(col, light = 0){
  RGB <- pmin(light + (1 - light) * col2rgb(col) / 255, 1) # pmin just to be on the safe side
  rgb(RGB["red", ], RGB["green", ], RGB["blue", ])
}  

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

#----------------------------------------------------


#'Plot summary of an fdsample object
#'
#'Plots the individual functions or a pointwise envelope, as well as a summary function, 
#'of function values coerced in an object of class \code{\link{fdsample}}. 
#'
#'@param x the fdsample to be plotted
#'@param ploptions list of optional plotting parameters, see the Details.
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
#'Plotting parameters for the individual functions can be given as list \code{"ploptions"} 
#'or separately. If not given explicitely, default values contained in the list 
#'\code{x$options} are used. If these are set to \code{NULL}, the global graphic
#'parameters as reported by \code{\link{par}} are assumed. 
#'
#'\emph{Individual functions} are plotted using the parameters
#'\tabular{ll}{
#'\code{col} \tab character or numeric, color,
#'\cr\code{lwd} \tab numeric, line width, 
#'\cr\code{lty} \tab character or numeric, line type, 
#'\cr\code{light} \tab numeric between 0 and 1, color brightness, defaults to 0.
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
#'\cr\code{light.env} \tab numeric between 0 and 1, color brightness. 
#'Defaults to \code{0.3 + 0.7*light},
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
#'@export
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#'@seealso \code{\link{getoptions}}
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
#' # load data, containing an example set "fuda"
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
#' blau <- list(col = "blue", light.env = .8, col.sum = "green")
#' summaryplot(fuda, sumfun = "mean", envprob = .8, ploptions = blau, light = .2)
#' # adds individual lines, overriding light options contained in fuda and blau


summaryplot <- function(x, ploptions = NULL,
                        sumfun = "mean", fopt = list(), 
                        envprob = NULL, 
                        includy = NULL, add=F, ...)
{
  if (length(x$dimarg) > 1) stop ("sorry, plotting of higher dimensional fdsamples not yet supported")
 
  # allopt <- uniquelist(c(list(...), ploptions, x$options)) this does not allow lazy evaluation
  argu <- list(...)
  xopt <- getoptions(x)#, ploptions, ...)
  allopt <- uniquelist(c(argu, ploptions, xopt,
            # now a list of necessary options:
          list(light = 0,
               col.sum = NULL, lwd.sum = NULL, lty.sum = NULL, 
               col.env = NULL, light.env = NULL)))
  
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
    # set information for plotwindow
    if(is.null(allopt$ylim)) allopt$ylim <- yrange(x, includy)
    if(is.null(allopt$xlim)) allopt$xlim <- range(x$args)
    # Want type = "n"
    pargus <- updateoptions(.plotparams, allopt)
    pargus$type="n"
    do.call(plot.default, c(list(allopt$xlim, allopt$ylim), pargus))
  }
  # now do the plotting of envelopes and curves
  # replace non given plot options by graphic default parameters
  plopt <- updateNULLoptions (allopt, par(no.readonly = TRUE))
 # if (is.null(plopt$light)) plopt$light <- 0
  
  if (!is.null(envprob))
  {
    eplopt <- within(plopt, {
              if (!is.null(col.env)) col <- col.env
              if (!is.null(light.env)) light <- light.env
               else light <- 0.7 + .3*light
    })
    envy <- pwEnvelope(x, prob = envprob)
    plot(envy, eplopt, add = T)
  }
  
  if (plotcurves)  plot(x, plopt, add=T)
  
  if (!is.null(sumfun)) 
  {
    splopt <- within(plopt, {
              if (!is.null(col.sum)) col <- col.sum
              if (!is.null(lty.sum)) lty <- lty.sum
              if (!is.null(lwd.sum)) lwd <- lwd.sum else lwd <- 2 * lwd
              light = 0 
    })
    sufu <- apply.fdsample(x, sumfun, fopt)
    plot(sufu, splopt, add=T)
  }
  
}

