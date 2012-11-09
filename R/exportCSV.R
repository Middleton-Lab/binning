##' Export one or all parts of a binning class object to one or more 
##' csv files.
##' 
##' At least two files are always written, using the prefix specified 
##' by \code{filePrefix}. The "...times.csv" file includes the 
##' beginning times for all the bins in the other 1 or 4 files.
##' 
##' @title Export to .csv Files
##'   
##' @param x An object of class binning to export
##' @param which One of "run", "max", "rpm", "int", or "all". If "all"
##'   is selected, all of the binned parameters will be exported to 
##'   separate .csv files.
##' @param filePrefix A string indicating the prefix for the .csv 
##'   files to be written.
##'
##' @author Kevin Middleton (middletonk@@missouri.edu)
##'
##' @export
##' 
##' @seealso \code{\link{binning}}, \code{\link{write.csv}}
##' 
##' @examples
##' # Uncomment and run.
##' #example(bin.running)
##' #exportCSV(x, "R", "all")
##' #exportCSV(x, "R", "rpm")
##' 
exportCSV <- function(x, which, filePrefix = "Rout_"){
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
    write.csv(run, file = paste(filePrefix, "run.csv", sep = ""), 
              row.names = FALSE)
    write.csv(max, file = paste(filePrefix, "max.csv", sep = ""), 
              row.names = FALSE)
    write.csv(rpm, file = paste(filePrefix, "rpm.csv", sep = ""), 
              row.names = FALSE)
    write.csv(int, file = paste(filePrefix, "int.csv", sep = ""), 
              row.names = FALSE)
    write.csv(times, file = paste(filePrefix, "times.csv", sep = ""), 
              row.names = FALSE)
  } else {
    write.csv(get(which), 
              file = paste(filePrefix, which, ".csv", sep = ""), 
              row.names = FALSE)
    write.csv(times, file = paste(filePrefix, "times.csv", sep = ""), 
              row.names = FALSE)
  }
}
