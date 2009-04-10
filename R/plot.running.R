#######################################################
# function: plot.running
plot.running <- function(running, time.data, whlnum, whichplot = 'run'){

# Receive a list output from bin.running
# Options to plot run, max, int, rpm

n.bins <- nrow(time.data)

subs <- running[whlnum,]

if (whichplot == 'run') {
    dat <- t(subs[,2:(n.bins+1)])
    ylabel <- 'Revolutions per bin'
}

hrmin <- strptime(paste(time.data$hr, time.data$min, '00', sep = ':'), '%T')
hrmin <- format(hrmin, '%H:%M')

plot(dat, type = 'l', axes = FALSE, ylab = ylabel, xlab = 'Hour')
axis(1, at = seq(1, n.bins, 12), labels = hrmin[seq(1, n.bins, 12)], las = 2, cex.axis = 0.75)
axis(2)
box()
}
