#'@title Rank count test of goodness of fit
#'
#'@description Test if an observed curve matches a sample of simulated curves (or 
#'a group of other observations). Refines the result of the rank envelope test, but
#'does not have a direct graphical representation.
#'@param obs object of class \code{fdsample}, the observed curve.
#'@param sim object of class \code{fdsample}, the group of curves to which \code{obs} is
#'compared.
#'@param alternative a character string specifying the alternative hypothesis, one of
#'\code{"two.sided"} (default), \code{"less"} or \code{"greater"}. May be abbreviated.
#'@param inclprob a numerical vector of inclusion probabilities
#'of the envelopes to be plotted, for use in \code{\link{plot.envtest}}
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
#'The p-value is obtained by ranking the curves according to the number of 
#'minimum pointwise ranks obtained in any point of the curve (curves are actually
#'represented as vectors), see Myllymaki et al. (2015).
#'
#'@export
#'@author Ute Hahn, \email{ute@@imf.au.dk} 
#'@references
#'M. Myllymaki, T. Mrkvicka, P. Grabarnik, H. Seijo and U.Hahn (2015)
#'\emph{Global envelope tests for spatial processes}, 
#'\url{http://arxiv.org/abs/1307.0239v3}.
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
#'testresult <- rankCount.test(obs, sim)
#'print(testresult)

rankCount.test <- 
  function(obs, sim, alternative = c("two.sided", "less", "greater"),
           inclprob = c(0.95))
            {
  alternative = match.arg(alternative)
  if (!is.fdsample(obs) || !is.fdsample(sim)) {
    stop("rankCount.test currently only takes data of type fdsample")
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
  lorank <- apply(allvals, 1, rank, ties = "min")
  # from the top, starting with 1 = highest
  hirank <- apply(-allvals, 1, rank, ties = "min")
  
  if (alternative == "two.sided") {
    allrank <- pmin(lorank, hirank) # lowest achieved rank in all points of a curve
  } else {
    if (alternative == "greater") {
      allrank <- hirank
    } else {
      allrank <- lorank
    }
  } 
  
  # now rank the curves by lexical ordering
  
  # order ranks within each curve
  sortrank <- apply(allrank, 1, sort) # curves now represented as columns
  lexo <- do.call("order", split(sortrank, row(sortrank))) 
  
  # find ties
  sorted <- sortrank[ ,lexo]
  dupp <- duplicated(split(sorted, col(sorted)))  
  tied <- dupp | c(dupp[-1], F)
  
  # replace ranks of tied values by mean ranks  
  # (maybe a little bit awkward, but isntitcool)
  tie.rle <- rle(tied)  
  tie.end <- cumsum(tie.rle$lengths)     
  tie.start <- cumsum(c(1,tie.rle$lengths))
  tie.start <- tie.start[-length(tie.start)]
  rank.rle <- tie.rle
  rank.rle$values <- (tie.start + tie.end)/2
  tierank <- inverse.rle(rank.rle)  
  newrank <- 1:R
  newrank[tied] <- tierank[tied]  
  
  therank <- newrank[order(lexo)]  
   
  obsrank <- therank[1]

#   
  pvalue <- mean(therank <= obsrank) 
  
  mrquant <- quantile(therank,  1 - inclprob)
  trueprob <- 1 - sapply(mrquant, function(q) mean(therank < q))
  
  envs <- lapply(mrquant, function (q) pwEnvelope(sim[therank[-1] >= q], 1))
  
  datname <- paste(deparse(substitute(obs)),
                   ", simulated:", deparse(substitute(sim)),"=",
                   sim$groupsize,"curves")
  method <-"Rank count test for fda"                
  alternative <- "observation not from the same distribution as simulated data"
  names(obsrank) <- "extreme rank counts rank"
  erg <- list(
    statistic = obsrank,
    p.value = pvalue,
    alternative = alternative, 
    method = method, 
    data.name = datname,
    # for plotting
    obs = obs, 
    whereExtreme = (allrank[1,] == min(allrank[1, ])), 
   # envLess = EnvLess, 
  #  envSame = EnvSame,
  #  envMore = EnvMore,
   # envs = envs,
    trueprob = trueprob,
    yrange = range(yrange(sim), yrange(obs)))
  class(erg) <- "htest" #c("envtest", "htest")
  erg
}

