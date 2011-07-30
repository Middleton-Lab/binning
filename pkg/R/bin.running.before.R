##' Aggregate Wheel Running Data Prior to a Bin
##' 
##' Aggregate wheel running data prior to a specfic bin (i.e.,
##' computer interval number).
##'
##' @title Aggregate Wheel Running Data Prior to a Bin
##' 
##' @aliases bin.running.before
##' 
##' @param bin.size Size of bin (minutes)
##' @param bins.out Number of bins to return
##' @param bin.start Bin (interval) to start
##' @param ... Additional parameters passed to \code{bin.running()}
##' 
##' @return An object of class \code{running}. See \code{\link{bin.running}}
##' for additional details.
##' 
##' @author Kevin Middleton (kmm@@csusb.edu)
##' 
##' @seealso \code{\link{bin.running}}
##' 
##' @keywords file data
##'
##' @examples
##' 
##' # Load the 2006-11-05 running data for computer A
##' data(A061105)
##' 
##' # Aggregate running data into 10 * 10 minute bins starting
##' # at interval 600
##' A <- bin.running.before(whldat = A061105, computer = "A", 
##'                         bin.start = 600,
##'                         bin.size = 10, bins.out = 10)
##' A$times
##' A$run
##' # Plot aggregated running data for wheel number 1.
##' plot(A, whlnum = 1, whichplot = "run")
##' 
bin.running.before <- function(bin.size, bins.out, bin.start, ...){
  ## Find new bin.start to pass to bin.running
  newBin1 <- bin.start - (bins.out * bin.size)
  
  ## Call bin.running with the revised first bin #
  b <- bin.running(bin.size, bins.out, bin.start = newBin1, ...)
  
  ## Strip trailing rows from times
  b$times <- b$times[1:bins.out, ]
  
  return(b)
}
