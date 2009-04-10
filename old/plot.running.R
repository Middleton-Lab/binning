plot.running <- function(running, time.data, whlnum, whichplot = 'run'){

# Receive a list output from bin.running
# Options to plot run, max, int, rpm

n.bins <- nrow(time.data)

subs <- running[whlnum,]

if (whichplot == 'run') {
    dat <- t(subs[,2:(n.bins+1)])
    ylabel <- 'Revolutions per bin'
}

plot(dat, type = 'l', axes = FALSE, ylab = ylabel, xlab = 'Hour')
axis(1, at = 1:n.bins, labels = time.data$hour)
axis(2)
box()
}