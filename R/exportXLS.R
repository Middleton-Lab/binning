##' Export one or all parts of a binning class object to an Excel 
##' spreadsheet with multiple worksheets.
##' 
##' The \code{exportXLS()} requires that the WriteXLS package is 
##' installed and working correctly. Refer to the file \code{INSTALL} 
##' page in the WriteXLS package for additional details 
##' (\url{http://cran.r-project.org/web/packages/WriteXLS/INSTALL}). 
##' To make sure that your installation is complete, load the WriteXLS
##' package and execute \code{testPerl()}.
##' 
##' The "times" tab includes the beginning times for all the bins 
##' (columns) in the 1 or 4 preceeding tabs.
##' 
##' @title Export to Excel File
##'   
##' @param x An object of class binning to export.
##' @param which One of "run", "max", "rpm", "int", or "all". If "all"
##'   is selected, all of the binned parameters will be exported to 
##'   separate sheets of the file named in \code{file}.
##' @param ... Additional parameters passed to \code{WriteXLS()} 
##'   (e.g., \code{BoldHeaderRow = TRUE} to get a bold header row).
##' @param file The Excel files
##' 
##' @author Kevin Middleton (middletonk@@missouri.edu)
##'
##' @export
##'
##' @keywords data
##'
##' @seealso \code{\link{binning}}, \code{\link[WriteXLS]{WriteXLS}}
##' 
##' @examples
##' # Uncomment and run.
##' #example(bin.running)
##' #exportXLS(A.aggr, "R.xls", "all")
##' #exportXLS(A.aggr, "R.xls", "rpm")
##' 
exportXLS <- function(x, which, file, ...){
  if (!inherits(x, "binning")) {
    stop('object "x" is not of class "binning".')
  }

  choices <- c("run", "max", "rpm", "int", "all")
  if (!which %in% choices) {
    stop('"which" must be one of "run", "max", "rpm", "int", or "all".')
  }

  assign("run", x$run)
  assign("max", x$max)
  assign("rpm", x$rpm)
  assign("int", x$int)
  assign("times", x$times)

  if (which == "all"){
    WriteXLS::WriteXLS(c("run", "max", "rpm", "int", "times"),
             ExcelFileName = file, ...)
  } else {
    WriteXLS::WriteXLS(c(paste(which), "times"), 
             ExcelFileName = file,
             ...)
  }
}
