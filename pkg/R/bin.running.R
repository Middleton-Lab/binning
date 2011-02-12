bin.running <- function(whldat,
#			whl = 'ALL',
                        bin.size,
                        bins.out = 'ALL',
                        bin.start = 1,
                        start.at.1pm = TRUE,
                        computer,
                        header = NULL
                ){
# file: bin.running.R

# Created by Kevin Middleton (kmm@csusb.edu)
# 10 Aug 2007 - 0.00 Forked from aggr.running.R as a general function
# 15 Aug 2007 - 0.01 First working version
# 09 Apr 2009 - Resumed work
#			  - 0.02 added to github; work on documentation
# 11 Apr 2009 - 0.03 Conversion to S3

# Output:
#	List of:
#		aggr:
#			whlnum specific to computer A-D
#   		Sum of revolutions per bin (run_XX)
#   		Max interval per bin (max_XX)
#   		Number of intervals per bin with > 0 revolutions (int_XX)
#   		RPM = Sum / Int (rpm_XX)
#		bin.times:
#			bin: bin number
#			hour: hour that the bin starts
#			minute: minute that the bin starts

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

# Check the computer is A-D
if (!match(computer, c('A', 'B', 'C', 'D'), nomatch = FALSE, incomparables = FALSE)){
	stop("computer must be one of \'A\', \'B\', \'C\', or \'D\'.", call. = FALSE)}

# If no first bin size is specified, then set to bin size
if (!exists('first.bin.size')) first.bin.size <- bin.size

# Currently can only handle first.bin.size = bin.size
if (first.bin.size != bin.size){
    stop("Option not yet implemented: first.bin.size must be the same as bin.size.", call. = FALSE)
}

# xx Add a check that whl is within the range of computer


##########################################
# END PRELIMINARIES
##########################################

# Calculate the number of wheels (50 or 48)
# The first 7 columns are time/date information
numwhl <- (ncol(whldat)-7)/3

# Check for the right number of wheels for the chosen computer.
if (computer == 'A' & numwhl != 50){
	stop("computer A should have 50 wheels", call. = FALSE)}

if (computer == 'B' & numwhl != 50){
	stop("computer B should have 50 wheels", call. = FALSE)}

if (computer == 'C' & numwhl != 50){
	stop("computer C should have 50 wheels", call. = FALSE)}

if (computer == 'D' & numwhl != 48){
	stop("computer D should have 48 wheels", call. = FALSE)}

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

if (start.at.1pm){
    # Adjust bin.start to reflect removed bins
    # Drop any intervals before 13:00
    # xx this line gets an error in R CMD check
    hr <- NULL; rm(hr)
    whldat.str <- subset(whldat.str, hr != 12)
}

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

# xxx
#if (whl != 'ALL'){
#	whldat.zero <- whldat.str[, whl]
#	}

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


# Aggregate hours and minutes
bin.times <- aggregate(
	list(times), 
	by = list(bin = minbin),
	FUN = min)[, -4]

# Return aggregated data
aggrdat <- list(whlnum = whlnum, run = t.running, max = t.maxint, int = t.intsum, rpm = t.rpm, 
	times = bin.times, whldat = whldat, bin.size = bin.size, n.bins = n.bins, 
	bin.start = bin.start, start.at.1pm = start.at.1pm, computer = computer, header = header)
class(aggrdat) <- 'running'
aggrdat
}
