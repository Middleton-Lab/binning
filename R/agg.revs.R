##' Aggregate wheel revolutions
##'
##' Aggregate revolutions by the \code{by} parameter.
##' 
##' @title Aggregate Wheel Revolutions
##' 
##' @param x data.frame with wheel revolutions
##' @param start.date Date to start aggregating
##' @param end.date Date to end aggregating
##' @param by Time period to aggregate by.
##' @return A table of bins
##' @author Kevin Middleton
agg.revs <- function(x, start.date, end.date, by){
  if (start.date == end.date){
    stop("start.date and end.date can't be the same.")
  }

  start.at <- as.POSIXlt(paste(start.date, "12:00:00"), 
                         format = "%Y-%m-%d %H:%M:%S")
  end.at <- as.POSIXlt(paste(end.date, "12:00:00"), 
                       format = "%Y-%m-%d %H:%M:%S")
  bins <- cut(x$TimeStamp, breaks = seq(from = start.at,
                                        to = end.at,
                                        by = by))
  table(bins)
}
