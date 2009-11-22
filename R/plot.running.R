#######################################################
# function: plot.running
plot.running <- function(x, whlnum, whichplot = 'run', ...){

# Receive a object of type running
# Options to plot run, max, int, rpm

# x contains:
# whlnum, run, max, int, rpm, times, whldat, bin.size, n.bins, bin.start, 
# start.at.1pm, computer, header = header)

if (!match(whichplot, c('run', 'max', 'int', 'rpm'), nomatch = FALSE, incomparables = FALSE)){
	stop("whichplot must be one of \'run\', \'max\', \'int\', or \'rpm\'.", call. = FALSE)}

n.bins <- x$n.bins
bin.size <- x$bin.size
header <- x$header

if (x$computer == 'B') whlnum <- whlnum - 50
if (x$computer == 'C') whlnum <- whlnum - 100
if (x$computer == 'D') whlnum <- whlnum - 150

if (whichplot == 'run') {
	dat <- t(x$run[whlnum, ])
    ylabel <- paste('Revolutions per bin (', bin.size, ' min.)', sep = '')
}

if (whichplot == 'max') {
    dat <- t(x$max[whlnum, ])
    ylabel <- paste('Maximum number of revolutions per bin (', bin.size, ' min.)', sep = '')
}

if (whichplot == 'int') {
    dat <- t(x$int[whlnum, ])
    ylabel <- paste('Number of intervals per bin with >0 revolutions (', bin.size, ' min.)', sep = '')
}

if (whichplot == 'rpm') {
    dat <- t(x$rpm[whlnum, ])
    ylabel <- paste('Mean RPM per bin (', bin.size, ' min.)', sep = '')
}

hrmin <- strptime(paste(x$times$hr, x$times$min, '00', sep = ':'), '%T')
hrmin <- format(hrmin, '%H:%M')

plot(dat, type = 'l', axes = FALSE, ylab = ylabel, xlab = 'Hour', main = header, ...)
axis(1, at = seq(1, n.bins, 12), labels = hrmin[seq(1, n.bins, 12)], las = 2, cex.axis = 0.75)
axis(2)
box()
}
