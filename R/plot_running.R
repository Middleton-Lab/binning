## See http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
utils::globalVariables(c("Date", "Revs", "Wheel"), package = "binning")

##' Plot wheel running (new system)
##'
##' Create diagnostic plots for wheel running data,
##' 
##' @title Plot wheel running (new system)
##'   
##' @param data Data.frame returned from
##'   \code{\link{analyze_running}}.
##'   
##' @param save Boolean (default = FALSE) Should plots be saved?
##'   
##' @param start.at.0 Boolean (default = TRUE) Should y-axis start at
##'   0?
##' 
##' @author Kevin Middleton
##'
##' @export
##' 
plot_running <- function(data, save = FALSE, start.at.0 = TRUE){
    
  dm <- data
    
  p1 <- ggplot(dm, aes(x = Date, y = Revs))
  p2 <- p1 +
    geom_line(aes(group = Wheel, color = Wheel)) +
    ylab("Revolutions") +
    theme(legend.position = "none")
  p3 <- p1 +
    geom_line() +
    facet_grid(Wheel ~ .) +
    ylab("Revolutions") +
    theme(legend.position = "none")

  if (start.at.0){
    ymax <- max(dm$Revs) + 0.1 * max(dm$Revs)
    p2 <- p2 + scale_y_continuous(limits = c(0, ymax))
    p3 <- p3 + scale_y_continuous(limits = c(0, ymax))
  }
    
  if (save){
    ggsave(filename = "Running_all.pdf",
           plot = p2,
           width = 10,
           height = 7.5)
    ggsave(filename = "Running_individual.pdf",
           plot = p3,
           width = 7.5,
           height = 30)
  } else {
    print(p2)
    print(p3)
  }
}
