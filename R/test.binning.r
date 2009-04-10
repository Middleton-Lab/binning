# test.binning.R

setwd('/Users/kmm/Documents/Dropbox/Projects/Mouse Project/binning')

source('R/read.dat.R')
source('R/stackdata.R')
source('R/plot.running.R')
source('R/bin.running.R')

A  <- read.dat('data/061105A.DAT')
B  <- read.dat('data/061105B.DAT')
C  <- read.dat('data/061105C.DAT')
D  <- read.dat('data/061105D.DAT')

# MANUAL
whldat <- A
bin.size <- 10
first.bin.size <- 10
bins.out <- 'ALL'
bin.start <- 1
start.at.1pm <- TRUE
computer <- 'A'


# TEST
bin.size <- 5
aggrA <- bin.running(A, computer = 'A', bin.size = bin.size)
aggrB <- bin.running(B, computer = 'B', bin.size = bin.size)
aggrC <- bin.running(C, computer = 'C', bin.size = bin.size)
aggrD <- bin.running(D, computer = 'D', bin.size = bin.size)

running <- stackdata(aggrA[[1]], aggrB[[1]], aggrC[[1]], aggrD[[1]])
time.data <- aggrA[[2]]

plot.running(running, time.data, whlnum = 1)

# ask for Excel file with the whlnum, mouseid, line, linetype, sex

# Backward binning
bin.size <- 10
bin.start <- 601
bins.out <- 6
backward <- TRUE
computer <- 'A'
whldat <- A061105
aggrA <- bin.running(A061105, computer = 'A', bin.size = bin.size, bin.start = 601, bins.out = 6, backward = TRUE)


