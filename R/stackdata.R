##' Stack data frames on top of one another.
##' 
##' Take an arbitrary number of aggregated data.frames in and return
##' them as a single data.frame.
##'
##' @title Stack Data Frames
##' 
##' @param \dots data.frames to \code{rbind}
##' 
##' @return A data.frame with \dots stacked.
##' 
##' @author Kevin Middleton (kmm@@csusb.edu)
##' 
##' @seealso \code{\link{rbind}}
##' 
##' @keywords data
##'
##' @export
##' 
stackdata <- function(...){
  ## Need to deal with n.bins and time
  ## use match.arg()
  nms <- names(list(...))  
  stacked <- rbind(...)
  return(stacked)
}

