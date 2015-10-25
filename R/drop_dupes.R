#' Drop duplicated rows
#'
#' Remove rows with duplicated TimeStamp
#' 
#' @title Drop duplicated rows
#' 
#' @param dat data.frame from which to drop duplicated rows
#' 
#' @return data.frame without duplicated sensor recordings
#' 
#' @author Kevin Middleton
#' 
drop_dupes <- function(dat){
  
  TimeStamps <- as.matrix(table(dat$TimeStamp))
  Singletons <- TimeStamps[TimeStamps[, 1] == 1, ]
  Singletons <- as.matrix(Singletons)
  
  deduped <- dat[dat$TimeStamp %in% rownames(Singletons), ]
  
  deduped
}
