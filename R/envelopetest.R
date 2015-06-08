# global envelope test


#'@title Envelope test of goodness of fit
#'
#'@description Test if an observed curve matches a sample of simulated curves (or 
#'a group of other observations). A global envelope test is performed, with
#'p-value corresponding to the most extreme pointwise percentile the observed  
#'curve reaches among all curves. 
#'@param obs object of class \code{fdsample}, the observed curve.
#'@param sim object of class \code{fdsample}, the group of curves to which \code{obs} is
#'compared.
#'@param alternative a character string specifying the alternative hypothesis, one of
#'\code{"two.sided"} (default), \code{"less"} or \code{"greater"}. May be abbreviated.
#'@param inclprob a numerical vector of inclusion probabilities
#'of the envelopes to be plotted, for use in \code{\link{plot.envtest}}
#'@param includeobs logical, if TRUE, observed curve is also used in the envelope
#'@details The observed curve, represented by the \code{\link{fdsample}} object 
#'\code{obs} is compared to simulated curves collected 
#'in the \code{fdsample} object '\code{sim}. 
#'The two sets of curves have to share the same argument values, and \code{obs} is
#'supposed to contain only one curve.
#'
#'\code{alternative == "less"} 
#'is the one-sided alternative meaning that the observed curve
#'has (some) smaller function values than the simulated curves, and
#'\code{alternative == "greater"} is the opposite one-sided alternative.
#'
#'The test corresponds to the rank envelope test by Myllymaki et. al (2013, 2015), 
#'and to the procedure described in Davison and Hinkley (1997), Equation (4.17).
#'The  p-value is obtained by ranking the curves according to the minimum pointwise
#'rank obtained in any point of the curve -- note that the curves are actually
#'represented as vectors.
#'
#'The result of the test can be plotted, see \code{\link{plot.envtest}}. 
#' 
#'@export
#'@author Ute Hahn, \email{ute@@imf.au.dk} 
#'@references
#'M. Myllymaki, T. Mrkvicka, H. Seijo and P. Grabarnik (2013)
#'\emph{Global envelope tests for spatial processes}, 
#'\url{http://arxiv.org/abs/1307.0239v2}.
#'
#'M. Myllymaki, T. Mrkvicka, P. Grabarnik, H. Seijo and Ute Hahn (2015)
#'\emph{Global envelope tests for spatial processes}, 
#'\url{http://arxiv.org/abs/1307.0239v3}.
#'
#'Davison, A.C. and Hinkley, D.V. (1997) \emph{Bootstrap Methods and their 
#'Applications}, Cambridge University Press, Cambridge.
#'
#'
#'@examples
#'# make a sample of sinus curves
#'tt <- seq(0, 2*pi, length = 20)
#'sinsim <- replicate(5000, sin(tt) + cumsum(rnorm(20, 0, 0.01)))
#'sinobs <- sin(tt - pi/50) + cumsum(rnorm(20, 0, 0.01))
#'sim <- fdsample(tt, sinsim)
#'obs <- fdsample(tt, sinobs)
#'
#'testresult <- rankEnv.test(obs, sim)
#'print(testresult)
#'plot(testresult)
  
rankEnv.test <- 
  function(obs, sim, alternative = c("two.sided", "less", "greater"),
           inclprob = 0.95, includeobs = TRUE){
  alternative = match.arg(alternative)
  if (!is.fdsample(obs) || !is.fdsample(sim)) {
    stop("rankEnv.test currently only takes data of type fdsample")
  }
  if (obs$groupsize > 1) {
    warning("will only test the first function contained in obs")
    obs <- obs[1]
  }
  # TODO the following needs to be improved
  if (max(abs(obs$args - sim$args)) > 1e-7) 
    stop("observed and simulated data do not have the same argument values")
    
 
  # ranking in the points of the curve
  allvals <- cbind(obs$fvals, sim$fvals)
  R <- sim$groupsize + 1
  # from below, startin with 1 = lowest
  lorank <- apply(allvals, 1, rank, ties = "max") # was: "min"
  # from the top, starting with 1 = highest
  hirank <- apply(-allvals, 1, rank, ties = "max") # was: "min"
  
  if (alternative == "two.sided") {
    allrank <- pmin(lorank, hirank) # lowest achieved rank in all points of a curve
  } else {
    if (alternative == "greater") {
      allrank <- hirank
    } else {
      allrank <- lorank
    }
  } 
  
  # now use the mean minimum achieved rank as criterion to rank the curves
  
  minrank <- apply(allrank, 1, function (a) sort(as.vector(a))[1])
  obsrank <- minrank[1]
# schnickschnack
# whereHi <- hirank[1, ] == minrank[1]
#  whereLo <- lorank[1, ] == minrank[1]
# alsoHi <- sapply(whereHi, function(i) hirank[ , i] <= minrank[1])
#  alsoLo <- sapply(whereLo, function(i) lorank[ , i] <= minrank[1])
 
#   moreExtreme <- minrank[-1] < obsrank
#   if (sum(moreExtreme) > 1) 
#     EnvMore <- pwEnvelope(sim[moreExtreme], 1)
#   else EnvMore <- NULL
#   
#   asExtreme <- minrank[-1] == obsrank
#   if (sum(asExtreme) > 1) 
#     EnvSame <- pwEnvelope(sim[asExtreme], 1)
#   else EnvSame <- NULL
#   
#   lessExtreme <- minrank[-1] > obsrank
#   if (sum(lessExtreme) > 1) 
#     EnvLess <- pwEnvelope(sim[lessExtreme], 1)
#   else EnvLess <- NULL
#   
  pvalue <- c(mean(minrank < obsrank), mean(minrank <= obsrank))
  mrquant <- quantile(minrank,  1 - inclprob, type = 1)
  if (includeobs) {
    trueprob <- 1 - sapply(mrquant, function(q) mean(minrank <= q))
    allfdsamples <- fdsample(obs$args, allvals)
    envs <- lapply(mrquant, function (q) pwEnvelope(allfdsamples[minrank >= q], 1))  
  } else {
    trueprob <- 1 - sapply(mrquant, function(q) mean(minrank < q))
    envs <- lapply(mrquant, function (q) pwEnvelope(sim[minrank[-1] >= q], 1))
  }
  
  datname <- paste(deparse(substitute(obs)),
                   ", simulated:", deparse(substitute(sim)),"=",
                   sim$groupsize,"curves")
  method <-"Simple simultaneous envelope rank test for fda"                
  alternative <- "observation not from the same distribution as simulated data"
  names(obsrank) <- "minimum rank"
  erg <- list(
    statistic = obsrank,
    p.value = pvalue[2],
    p.interval = pvalue,
    alternative = alternative, 
    method = method, 
    data.name = datname,
    # for plotting
    obs = obs, 
    whereExtreme = (allrank[1,] == obsrank), 
   # envLess = EnvLess, 
  #  envSame = EnvSame,
  #  envMore = EnvMore,
    envs = envs,
    trueprob = trueprob,
    yrange = range(yrange(sim), yrange(obs)))
  class(erg) <- c("envtest", "htest")
  erg
}

#@rdname rankEnv.test
#'@title Plot the result of an envelope test
#'@description Plots simultaneous envelopes of a rank envelope test and
#'prints the exact inclusion probabilities.
#'@param x test result to be plotted, an object of type \code{envtest}
#'@param ... arguments passed to plot methods
#'@param col.obs color for plotting the observations
#'@details Objects of type \code{envtest} are generated by \code{\link{rankEnv.test}}
#'which calculates the result of a test comparing an observed curve to a family
#'of other curves (most often simulated ones).
#'
#'Method \code{plot.envtest} plots envelopes according to the inclusion probabilities 
#'given while
#'calculating the test, and adds the observed curve. The most extreme points of 
#'the observed curve, i.e. the points that are reponsible for the curve's p-value,
#'are marked by points.
#'
#'For an example, see \code{\link{rankEnv.test}}
#'@method plot envtest
#'@export

plot.envtest <- function(x, ..., col.obs = "red"){
  #if (x$minn > 1) 
  #  warning("Do not use this envelope as a graphical test, it is based on 
  #    average ranks, minn =", x$minn, "!!!@%&!\n")
  dotargs <- simplist(...)
  nenv <- length(x$envs)
  alphas <- exp(seq(log(.2), log(.6), length.out = nenv))
  for (i in seq_along(x$envs))
  {
    if (!is.null(x$envs[[i]])){
      dotargs$alpha <- alphas[i]
      plot(x$envs[[i]], ylim = x$yrange, dotargs)
       dotargs$add <- TRUE
    }  
  }
  if (length(x$trueprob) == 1) cat("\ntrue envelope inclusion probability", x$trueprob,"\n")
   else cat("\ntrue envelope inclusion probabilities", x$trueprob,"\n")
#  if (!is.null(x$envMore)){
#    plot(x$envMore, ylim = x$yrange, dotargs, alpha = .25)
#    dotargs$add <- TRUE
#  } 
#  if (!is.null(x$envSame)){
#    plot(x$envSame, ylim = x$yrange, dotargs, alpha = .4)
#    dotargs$add <- TRUE
#  } 
#  if (!is.null(x$envLess)){
#    plot(x$envLess, ylim = x$yrange, dotargs, alpha = .6)
#    dotargs$add <- TRUE
#  }
  dotargs$alpha <- NULL
  splot(x$obs, ylim = x$yrange, dotargs, col = col.obs)
  dotargs$add <- NULL
  splot(x$obs$args[x$whereExtreme], x$obs$fvals[x$whereExtreme], dotargs, 
    col = col.obs, .plotmethod = "points")
}

######### print envtest
#'@method print envtest
#'@export

print.envtest <- function (x, digits = getOption("digits"), prefix = "\t", ...) 
{
   NextMethod()
   if (!is.null(x$p.interval)) cat("p-interval:  [", format(x$p.interval[1],digits = max(1L, digits - 3L)),
       ",", format(x$p.interval[2],digits = max(1L, digits - 3L)),"]")
   invisible(x)
}
