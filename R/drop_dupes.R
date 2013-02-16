##' Drop duplicated rows
##'
##' Remove rows with duplicated TimeStamp
##' 
##' @title Drop duplicated rows
##' 
##' @param dat data.frame from which to drop duplicated rows
##' 
##' @return data.frame without duplicated sensor recordings
##' 
##' @author Kevin Middleton
##' 
drop_dupes <- function(dat){
  deduped <- dat
  
  deduped <- deduped[!duplicated(deduped$TimeStamp), ]
  
  deduped
}
