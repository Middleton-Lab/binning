# test.binning.R

setwd('/Users/kmm/Documents/Dropbox/Projects/Mouse Project/binning')

source('R/read.dat.R')
source('R/stackdata.R')
source('R/plot.running.R')
source('R/bin.running.R')
source('R/lineaggr.R')
source('R/linetypeaggr.R')

A061105  <- read.dat('data/061105A.DAT')
B061105  <- read.dat('data/061105B.DAT')
C061105  <- read.dat('data/061105C.DAT')
D061105  <- read.dat('data/061105D.DAT')

#setwd('~')
#package.skeleton(list=c('read.dat', 'stackdata',"plot.running","bin.running",
#	'lineaggr', 'linetypeaggr', 'A061105', 'B061105', 'C061105', 'D061105'), 
#	name="binning")

# MANUAL
whldat <- A061105
bin.size <- 10
first.bin.size <- 10
bins.out <- 'ALL'
bin.start <- 1
start.at.1pm <- TRUE
computer <- 'A'
header <- NULL


# TEST
bin.size <- 10
aggr.A <- bin.running(A061105, computer = 'A', bin.size = 10)
aggr.A <- bin.running(A061105, whl = 1, computer = 'A', bin.size = 10)
aggr.B <- bin.running(B061105, computer = 'B', bin.size = bin.size)
aggr.C <- bin.running(C061105, computer = 'C', bin.size = bin.size)
aggr.D <- bin.running(D061105, computer = 'D', bin.size = bin.size)

running <- stackdata(aggrA[[1]], aggrB[[1]], aggrC[[1]], aggrD[[1]])
time.data <- aggrA[[2]]

plot(aggr.A, whlnum = 1, whichplot = 'run')
plot(aggr.A, whlnum = 1, whichplot = 'run', col = 'red')

plot(aggr.A, whlnum = 1, whichplot = 'max')
plot(aggr.A, whlnum = 1, whichplot = 'int')
plot(aggr.A, whlnum = 1, whichplot = 'rpm')

plot(aggr.B, whlnum = 75, whichplot = 'run')

# ask for Excel file with the whlnum, mouseid, line, linetype, sex

# Backward binning
bin.size <- 10
bin.start <- 601
bins.out <- 6
backward <- TRUE
computer <- 'A'
whldat <- A061105
aggrA <- bin.running(A, computer = 'A', bin.size = bin.size, bin.start = 601, bins.out = 6, backward = TRUE)


