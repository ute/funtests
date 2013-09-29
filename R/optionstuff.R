  

#' Update a list of options, e.g. for plotting
#' 
#' Compares the optional ... arguments with the list elements in \code{default}
#' and return an updated list. The function was written for setting plotting defaults
#' for xy-lists, but may be useful also for other lists.
#'
#' @param default the list to be updated
#' @param optlist optional list of updates
#' @param ... optional named pairs, updates for \code{default}
#' @return a list with same names as the elements of \code{default}
#' @export
# @seealso \code{\link{unusedoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

updateoptions <- function(default, optlist=NULL, ...)
{
  argu <- list(...)
  if(is.list(optlist)) argu <- c(argu, optlist) 
  result <- default
  if(length(argu) > 0)
  {
    namdef <- names(default)
    pmatch(names(argu), namdef) -> keys
    if (length(keys) > 0) {
      argna <- lapply(argu, function(z) if (is.null(z)) NA else z)
      resna <- lapply(result, function(z) if (is.null(z)) NA else z)
      for (ki in 1:length(keys)) if (!is.na(keys[ki])) resna[[keys[ki]]] <- argna[[ki]]
      result <- lapply(resna, function(z) if(is.function(z) || !is.na(z)) z else NULL)
      }  
  } 
  return(result)
}

#' Get options 
#' 
#' Update the options element in an \code{\link{fdsample}} object or another list with an element \code{$options}, 
#' according to arguments
#' 
#' @param x list with element \code{options} to be updated
#' @param optlist optional list of updates
#' @param ... optional named pairs, updates for \code{default}
#' @return a list, copy of \code{x$options}, with updated elements
#' @details A copy of \code{x$options} is updated according to the remaining arguments, 
#' where arguments given as \code{...} have highest priority. The element names may be 
#' unambiguously abbreviated.
#' 
#' If no arguments are given, the \code{options} element of list \code{x} is returned as is.
#' @export
# @seealso \code{\link{unusedoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}
#' @examples
#' # load simulated example data
#' data(ExampleData) 
#' str(fuda$options)
#' blue <- getoptions(fuda, col = "blue")
#' str(blue)
#' # abbreviating names and using a predefined list
#' bluegreen <- getoptions(fuda, blue, col.i = "green", light = .5)
#' str(bluegreen)

getoptions <- function(x, optlist=NULL, ...)
{
  return(updateoptions(updateoptions(x$options, optlist), ...))
}

#' Internal functions for class \code{fdsample}
#'
#' Internal functions for dealing with objects of class \code{fdsample}, and plotting options
#
# Return arguments matching a default list
# 
# Compares the optional ... arguments with the list elements in \code{default}
# and returns a list of all arguments with names that match names of elements in \code{default}. 
# The function was written for \code{defaultsoptions.fdsample} but can also be 
# used for other lists.
#
# @param default the list to be updated
# @param ... named pairs, updates for \code{default}
# @return a list with elements with names that match those in \code{default}
# @export
#' @rdname fdsample-internal
#' @keywords {internal}
# @seealso \code{\link{setoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

matchingoptions <- function(default = defaultoptions.fdsample, optlist,...)
{
  argu <- list(...)
  namdef <- names(default)
  pmatch(names(argu), namdef) -> keys
  result <- argu
  matchkeys <- which(!is.na(keys))
  if (length(matchkeys) > 0)
    return(result[matchkeys]) else return(NULL)
}


# Return arguments not matching a default list
# 
# Compares the optional ... arguments with the list elements in \code{default}
# and returns a list of all arguments the names of which do not match names of elements in \code{default}. 
# The function was written for \code{defaultoptions.fdsample} but can also be 
# used for other lists.
#
# @param default the list to be updated
# @param optlist optional list of options to be updated
# @param ... named pairs, updates for \code{default}
# @return a list with elements with names that differ from those in \code{default}
# @export
#' @rdname fdsample-internal
#' @keywords internal
# @seealso \code{\link{setoptions}}
# @author Ute Hahn,  \email{ute@@imf.au.dk}

unusedoptions <- function(default = defaultoptions.fdsample, optlist = NULL, ...)
{
  argu <- uniquelist(c(list(...), optlist))
  namdef <- names(default)
  pmatch(names(argu), namdef) -> keys
  result <- argu
  leftoverkeys <- which(is.na(keys))
  if (length(leftoverkeys) > 0)
    return(result[leftoverkeys]) else return(NULL)
}

# return unique elements
# @param xlist the list to be simplified
# @return a list with unique elements, priority: the first element counts
# @export
#' @rdname fdsample-internal
#' @keywords {internal}
# @seealso \code{\link{setoptions}}
#' @author Ute Hahn,  \email{ute@@imf.au.dk}

uniquelist <- function(xlist) xlist[!duplicated(names(xlist))]
