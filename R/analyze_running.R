#' Analyze running (new system)
#'
#' Analyze aggregated running data. Includes plots and summary
#' output.
#' 
#' @title Analyze running (new system)
#'   
#' @param file Character string giving the \code{.csv} file to 
#'   analyze. This file should have been written by 
#'   \code{\link{aggregate_running}}.
#' @param drop.days Vector of character strings giving any dates 
#'   (e.g., \code{"2011-12-31"}) that should be dropped from the 
#'   analysis.
#' 
#' @return A data.frame with the wheels and summed revolutions for
#' the last two days of running.
#' 
#' @author Kevin Middleton (middletonk@@missouri.edu)
#'
#' @keywords file data
#'
#' @export
#' 
analyze_running <- function(file, drop.days = NULL){
  d <- read.csv(file)

  rownames(d) <- NULL
  d$Date <- as.Date(d$Date, format = "%m/%d/%Y")
  
  dt <- t(d)
  colnames(dt) <- dt[1, ]
  dt <- as.matrix(dt[-1, ])
  dt <- apply(dt, c(1, 2), as.numeric)
  
  ## Melt data frame for plotting
  dm <- melt(dt)
  names(dm) <- c("Wheel", "Date", "Revs")
  dm$Date <- as.Date(dm$Date)
  
  ## Drop days we don't want
  if (!is.null(drop.days)){
    dm <- dm[!dm$Date %in% as.Date(drop.days), ]
  }
  
  dm <- dm[dm$Revs > 0, ]
  
  ## Function to pad with 0's
  pad_zero <- function(x){
    y <- as.character(x)
    y <- as.data.frame(do.call("rbind", strsplit(y, "Wheel")))
    y$V2 <- as.numeric(as.character(y$V2))
    y$padded <- ifelse(y$V2 <= 9, sprintf("%02d", y$V2), y$V2)
    y$Wheel <- paste("Wheel_", y$padded, sep = "")
    return(y$Wheel)
  }
  
  dm$Wheel <- as.factor(pad_zero(dm$Wheel))
    
  Last2 <- data.frame(Wheel = pad_zero(rownames(dt)),
                      SumLast2Nights = dt[, (ncol(dt)-1)] + dt[, ncol(dt)])
  rownames(Last2) <- NULL
  Last2 <- Last2[order(Last2$Wheel), ]
  print(Last2)
  
  return(dm)    
}

