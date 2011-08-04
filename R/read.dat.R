##' Read Wheel Running Data
##' 
##' Read either a comma-separated (.DAT) or tab-delimited (.txt) wheel
##' running data file.
##' 
##' @title Read Wheel Running Data
##' 
##' @param file either a comma-separated (.DAT) or tab-delimited
##' (.txt) wheel running data file
##' @param tab.delim Logical for whether the file is tab delimited or
##' not. Default is FALSE, because unmodified DAT files are
##' comma-separated. If the file has been opened, modified, and
##' resaved, then it is probably tab delimited.
##' 
##' @return A data.frame of wheel running data.
##' 
##' @author Kevin Middleton (kmm@@csusb.edu)
##' 
##' @keywords data
##'
##' @export
##'
##' @examples
##' A <- read.dat(system.file("extdata", "061105A.DAT", package="binning"))
##' dim(A)
##' 
##' D <- read.dat(system.file("extdata", "061105D.DAT", package="binning"))
##' dim(D)
##' # D has 6 fewer columns = 2 fewer wheels.
read.dat <- function(file, tab.delim = FALSE){
  if (tab.delim) {
    whldat <- read.delim(file, header = FALSE, sep = '\t')
  } else {
    whldat <- read.csv(file, header = FALSE)}
  whldat # Return whldat
}
