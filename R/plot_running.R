##' Plot wheel running (new system)
##'
##' Create diagnostic plots for wheel running data,
##' 
##' @title Plot wheel running (new system)
##' 
##' @param data Data.frame returned from \code{\link{analyze_running}}.
##' @param save Boolean (defulat = FALSE) should plots be saved?
##' 
##' @author Kevin Middleton
##'
##' @export
##' 
plot_running <- function(data, save = FALSE){
  dm <- data
  
  ymax <- max(dm$Revs) + 0.1 * max(dm$Revs)
  
  p1 <- ggplot(dm, aes(x = Date, y = Revs))
  p2 <- p1 +
    geom_line(aes(group = Wheel, color = Wheel)) +
    ylab("Revolutions") +
    ylim(0, ymax) +
    opts(legend.position = "none")
  p3 <- p1 +
    geom_line() +
    facet_grid(Wheel ~ .) +
    ylab("Revolutions") +
    ylim(0, ymax) +
    opts(legend.position = "none")

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
