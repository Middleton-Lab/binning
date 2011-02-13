##' Aggregate by Linetype
##' 
##' Aggregate wheel running data by linetype.
##' 
##' @title Aggregate by Linetype
##' 
##' @param dat A data.frame
##' 
##' @return Data aggregated by linetype
##' 
##' @author Kevin Middleton (kmm@@csusb.edu)
##' 
##' @seealso \code{\link{lineaggr}}
##' 
##' @keywords univar
##' 
##' @examples
##' 
##' ## The function is currently defined as
##' function(dat){
##' 
##' aggregate(
##'     list(dat),
##'     by = list(dat$linetype, dat$sex),
##'     FUN = mean)
##'   }
##' 
linetypeaggr <- function(dat){
  aggregate(list(dat),
            by = list(dat$linetype, dat$sex),
            FUN = mean)
}
