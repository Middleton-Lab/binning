# test.binning.R

source('c:/SELECT/binning/read.dat.R')
source('c:/SELECT/binning/stackdata.R')
source('c:/SELECT/binning/plot.running.R')
source('c:/SELECT/binning/binning.R')

A061105  <- read.dat('c:/SELECT/binning/061105/061105A.DAT')
B061105  <- read.dat('c:/SELECT/binning/061105/061105B.DAT')
C061105  <- read.dat('c:/SELECT/binning/061105/061105C.DAT')
D061105  <- read.dat('c:/SELECT/binning/061105/061105D.DAT')

bin.size <- 20
aggrA <- bin.running(A061105, computer = 'A', bin.size = bin.size)
aggrB <- bin.running(B061105, computer = 'B', bin.size = bin.size)
aggrC <- bin.running(C061105, computer = 'C', bin.size = bin.size)
aggrD <- bin.running(D061105, computer = 'D', bin.size = bin.size)

running <- stackdata(aggrA$running, aggrB$running, aggrC$running, aggrD$running)
time.data <- aggrA$bin.time

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






badwhl <- whldatA[,1:50]
bin.running(badwhl)

setwd('c:/SELECT/binning/')
package.skeleton(name = 'runagg', list = c('read.dat', 'bin.running', 'stackdata',
    'A061105', 'B061105', 'C061105', 'D061105'),
    force = TRUE)
