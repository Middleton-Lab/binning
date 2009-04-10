bin.running <- function(whldat,
                        bin.size = 10,
                        first.bin.size = 10,    # Currently = bin.size only
                        bins.out = 'ALL',
                        bin.start = 1,
                        start.at.1pm = TRUE,     # Not currently implemented
                        computer                 # A, B, C, D
                ){
# file: bin.running.R

# Created by Kevin Middleton (kmm@csusb.edu)
# 10 Aug 2007 - 0.00 Forked from aggr.running.R as a general function
# 15 Aug 2007 - 0.01 First working version
# 09 Apr 2009 - Resumed work
#			- 0.02

# Output:
#   Sum of revolutions per bin (run_XX)
#   Max interval per bin (max_XX)
#   Number of intervals per bin with > 0 revolutions (int_XX)
#   RPM = Sum / Int (rpm_XX)

##########################################
# PRELIMINARIES
##########################################

# Check wheel running data. There should be either 157 or 151 columns corresponding
#   to either 50 (A-C) or 48 wheels (D)
if (ncol(whldat) != 157){
    if (ncol(whldat) != 151){
        stop("The wheel running file does not have the correct number of columns.", call. = FALSE)
    }
}

# If no first bin size is specified, then set to bin size
if (!exists('first.bin.size')) first.bin.size <- bin.size

##########################################
# END PRELIMINARIES
##########################################

# Calculate the number of wheels (50 or 48)
# The first 7 columns are time/date information
numwhl <- (ncol(whldat)-7)/3

# Remove columns we don't need:
#	time (other than hour and min), date
#	The "seq" parameters remove the forward and backward columns,
#		leaving only the sum column
whldat.str <- whldat[ , -c(3:7, seq(8, ncol(whldat), 3), seq(9, ncol(whldat), 3))]

# Rename the remaining columns: hr, min, and XX for wheel number
names(whldat.str)[1] <- "hr"
names(whldat.str)[2] <- "min"
names(whldat.str)[3:(numwhl+2)] <- paste(1:numwhl, sep = "")

# Add a column for the interval number
whldat.str <- cbind(whldat.str, interval = 1:nrow(whldat))

#if (start.at.1pm){
    # Adjust bin.start to reflect removed bins
    # Drop any intervals before 13:00
#    whldat.str <- subset(whldat.str, hr != 12)
#}

# If binning does not start at bin 1
if (bin.start != 1){
    whldat.zero <- whldat.str[(bin.start + 1):nrow(whldat.str),]
} else {
    whldat.zero <- whldat.str
}

# Set up binning
n.bins <- nrow(whldat.zero)%/%bin.size      # Number of bins
leftover <- nrow(whldat.zero)%%bin.size     # # of rows in incomplete last bin

if (bins.out == 'ALL') bins.out <- n.bins
if (n.bins < bins.out){
    stop("bins.out is too large. Not enough bins in data.", call. = FALSE)
}

# Remove rows in incomplete last bin (if incomplete)
if (leftover > 0) whldat.zero <- whldat.zero[1:(nrow(whldat.zero)-leftover),]

# Column used for aggregating
minbin <- rep(1:n.bins, each = bin.size)

# Split off hour & minute columns
times <- data.frame(hr = whldat.zero$hr, minute = whldat.zero$min, minbin) 
whldat.zero <- whldat.zero[,-(1:2)]

# Aggregate wheel running sum revolutions per bin
running <- matrix(data = NA, nrow = n.bins, ncol = numwhl)
for (i in 1:numwhl){
    agr.run <- aggregate(
        list(run = whldat.zero[,i]),
        by = list(bin = minbin),
        FUN = sum)
    running[,i] <- agr.run[,2]
}

# Aggregate maximum interval per bin
maxint <- matrix(data = NA, nrow = n.bins, ncol = numwhl)
for (i in 1:numwhl){
    agr.max <- aggregate(
        list(run = whldat.zero[,i]),
        by = list(bin = minbin),
        FUN = max)
    maxint[,i] <- agr.max[,2]
}

# Aggregate number of intervals per bin with > 0 revolution
intsum <- matrix(data = NA, nrow = n.bins, ncol = numwhl)
for (i in 1:numwhl){
    agr.int <- aggregate(
        list(run = whldat.zero[,i]),
        by = list(bin = minbin),
        FUN = function(x){sum(x > 0)})
    intsum[,i] <- agr.int[,2]
}

# Calculate RPM. Use ifelse to avoid NaNs resulting from 0/0.
rpm <- ifelse(running == 0, 0, running / intsum)

# Truncate matrices to only return only rows 1:bins.out
running.trunc <- running[1:bins.out, ]
maxint.trunc <- maxint[1:bins.out, ]
intsum.trunc <- intsum[1:bins.out, ]
rpm.trunc <- rpm[1:bins.out, ]

# Set up output column names.
binnum <- 1:bins.out
Run <- paste("run", binnum, sep = "_")
Max <- paste("max", binnum, sep = "_")
Int <- paste("int", binnum, sep = "_")
RPM <- paste("rpm", binnum, sep = "_")

# Transpose to get wheels as rows and bins as columns
t.running <- data.frame(t(running.trunc))
names(t.running) <- Run
t.maxint <- data.frame(t(maxint.trunc))
names(t.maxint) <- Max
t.intsum <- data.frame(t(intsum.trunc))
names(t.intsum) <- Int
t.rpm <- data.frame(t(rpm.trunc))
names(t.rpm) <- RPM

# Set wheel numbers based on computer
if (computer == 'A') whlnum <-   1:50
if (computer == 'B') whlnum <-  51:100
if (computer == 'C') whlnum <- 101:150
if (computer == 'D') whlnum <- 151:198

aggr <- cbind(whlnum, t.running, t.maxint, t.intsum, t.rpm)

# Aggregate hours and minutes
# xx Not sure =what this is doing
bin.times <- aggregate(
	list(times), 
	by = list(bin = minbin),
	FUN = min)[, -4]

# Return aggregated data
list(aggr, bin.times)
}