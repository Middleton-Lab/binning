##' Aggregate by Line
##' 
##' Aggregate running data by line
##' 
##' @title Aggregate by Line
##' 
##' @param dat A data.frame
##' 
##' @return Data aggregated by line
##' 
##' @author Kevin Middleton (kmm@@csusb.edu)
##' 
##' @seealso \code{\link{linetypeaggr}}
##' 
##' @keywords univar
##' 
lineaggr <- function(dat){
  aggregate(
            list(dat),
            by = list(dat$line, dat$sex),
            FUN = mean)
}
