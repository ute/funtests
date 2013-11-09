# plot urfunctions and (TODO) funsamples


#'Plots an \code{urfunction}.
#'
#'@param x the \code{urfunction} to be plotted
#'@param includy optional numeric vector containing values that are to be included in the
#'\code{ylim} extent of the \eqn{y-axis}. Can be used to always start at 0, for example.
#'@param ... further arguments for controlling the plot, see the arguments of \code{\link{curve}}.
#'@details
#'Plots the function \code{x}, using plot options contained in the \code{options} 
#'attribute of \code{x}. These options can be overridden by \code{...} arguments.
#'Plot parameters in \code{...} can be given as \code{"\link{simplist}"}s or separately.
#'
#@S3method plot urfunction
#'@method plot urfunction
#'@export plot.urfunction
# @author Ute Hahn,  \email{ute@@imf.au.dk}
#'@examples
#'urfu <- urfunction(sin, xlab = "beta")
#'plot(urfu, from = 0, to = 2*pi)
#'#compare x- and y- axis labels
#'plot(sin, from = 0, to = 2*pi)

plot.urfunction <- function(x, ..., includy = NULL)
{
  allopt <- simplist(attr(x, "options"), ..., .NULL.rm = TRUE)
  
  # need to get plot limits?
  addF <- is.null(allopt$add) || identical(allopt$add, FALSE)
  if (dev.cur() == 1L && !identical(addF, TRUE)) {
        warning("'add' will be ignored as there is no existing plot")
        addF <- TRUE
    }
  
  if ("stepfun" %in% class(x)){
    yy <- range(get("y", environment(x)))
    if(is.null(allopt$ylim)) allopt$ylim <- range(yy, includy)
    plopt <-matching(allopt, c(.graphparams, .plotparams, add = T, "plot.stepfun"))
    splot(x, plopt, .plotmethod = "plot.stepfun")
  } else {
   # all the following stuff is done because of includey. Otherwise
  # a simple splot call would do...
    if (is.null(allopt$n) || allopt$n < 2) allopt$n <- 101
    # adapted from R-function "curve":
    if (is.null(allopt$from) || is.null(allopt$to)) {
      xl <- if (!is.null(allopt$xlim)) 
        allopt$xlim
      else if (!addF) {
        pu <- par("usr")[1L:2L]
        if (par("xaxs") == "r") 
          pu <- extendrange(pu, f = -1/27)
        if (par("xlog")) 
          10^pu
        else pu
      }
      else c(0, 1)
      if (is.null(allopt$from)) 
        allopt$from <- xl[1L]
      if (is.null(allopt$to)) 
        allopt$to <- xl[2L]
    }
    lg <- if (length(allopt$log)) 
      log
    else if (!addF && par("xlog")) 
      "x"
    else ""
    if (length(lg) == 0) 
      lg <- ""
    if (grepl("x", lg, fixed = TRUE)) {
      if (allopt$from <= 0 || allopt$to <= 0) 
        stop("'from' and 'to' must be > 0 with log=\"x\"")
      xx <- exp(seq.int(log(allopt$from), log(allopt$to), length.out = allopt$n))
    }
    else xx <- seq.int(allopt$from, allopt$to, length.out = allopt$n)
    
    yy <- do.call(x, list(xx))#, c(list(xx), allopt))
    if (length(yy) != length(xx)) 
      stop(paste("A problem occured when evaluating function", deparse(substitute(x))))
    
    if(is.null(allopt$ylim)) allopt$ylim <- range(yy, includy)
    if(is.null(allopt$xlim)) allopt$xlim <- range(xx)
    
    # adjust plot options
    allopt <- updateJoin(c(par(c("col", "lty")), type = "l"), allopt)
    if(is.null(allopt$type)) allopt$type <- "l"
    allopt$add <- !addF
    
    plopt <- matching(allopt, c(.graphparams, .plotparams, add = TRUE))
    do.call(matplot, c(list(x = xx, y = yy), plopt))
  }  
  invisible()
}
