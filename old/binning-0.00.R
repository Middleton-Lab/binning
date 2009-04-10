bin.running <- function(whldat,
                        bin.size = 10,
                        first.bin.size,
                        bins.out,
                        bin.start = 1,
                        start.at.1pm = TRUE){
# file: binning.R

# Created by Kevin Middleton (kmm@csusb.edu)
# 10 Aug 2007 - Forked from aggr.running.R as a general function

# Output:
#   Sum of revolutions per bin
#   Max interval per bin
#   Number of intervals per bin with > 0 revolutions
#   RPM = Sum / Int

# Bin from start of file (default) or an arbitrary point (bin.start)
#   If arbitrary, then bin.start not included.
# Delete incomplete last bin
# Pass size of bins (bin.size)
# Size of first bin (first.bin.size); if null, then set to bin.size.
# How many bins to return (bins.out)


#    # assign(paste("q",i,sep=""),with(dta,eval(parse(text=e1))))
#    for (j in 0:12){
#    assign(paste('aggr$run', j, '[aggr$whlnum == ', i, ']', sep = ''), aggr.sum$run[j+1])
#    }

# Check wheel running data. There should be either 157 or 151 columns corresponding
#   to either 50 (A-C) or 48 wheels (D)
if (ncol(whldat) != 157){
    if (ncol(whldat) != 151){
        stop("The wheel running file does not have the correct number of columns.", call. = FALSE)
    }
}

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

if (start.at.1pm){
    # Drop any intervals before 13:00
    whldat.str <- subset(whldat.str, hr != 12)
}

# Set up data frame to store aggregated data
aggr <- data.frame(whlnum = batch$whlnum,
    run0  = numeric(num.mice),
    run1  = numeric(num.mice),
    run2  = numeric(num.mice),
    run3  = numeric(num.mice),
    run4  = numeric(num.mice),
    run5  = numeric(num.mice),
    run6  = numeric(num.mice),
    run7  = numeric(num.mice),
    run8  = numeric(num.mice),
    run9  = numeric(num.mice),
    run10 = numeric(num.mice),
    run11 = numeric(num.mice),
    run12 = numeric(num.mice),
    max0  = numeric(num.mice),
    max1  = numeric(num.mice),
    max2  = numeric(num.mice),
    max3  = numeric(num.mice),
    max4  = numeric(num.mice),
    max5  = numeric(num.mice),
    max6  = numeric(num.mice),
    max7  = numeric(num.mice),
    max8  = numeric(num.mice),
    max9  = numeric(num.mice),
    max10 = numeric(num.mice),
    max11 = numeric(num.mice),
    max12 = numeric(num.mice),
    int0  = numeric(num.mice),
    int1  = numeric(num.mice),
    int2  = numeric(num.mice),
    int3  = numeric(num.mice),
    int4  = numeric(num.mice),
    int5  = numeric(num.mice),
    int6  = numeric(num.mice),
    int7  = numeric(num.mice),
    int8  = numeric(num.mice),
    int9  = numeric(num.mice),
    int10 = numeric(num.mice),
    int11 = numeric(num.mice),
    int12 = numeric(num.mice),
    rpm0  = numeric(num.mice),
    rpm1  = numeric(num.mice),
    rpm2  = numeric(num.mice),
    rpm3  = numeric(num.mice),
    rpm4  = numeric(num.mice),
    rpm5  = numeric(num.mice),
    rpm6  = numeric(num.mice),
    rpm7  = numeric(num.mice),
    rpm8  = numeric(num.mice),
    rpm9  = numeric(num.mice),
    rpm10 = numeric(num.mice),
    rpm11 = numeric(num.mice),
    rpm12 = numeric(num.mice))

for (i in batchA$whlnum){
    # Get only revs & interval for the wheel number
    revs <- whldat[,c(i, "interval")]

    # Interval where mouse back on wheel
    inj.int <- batchA[batchA$whlnum == i, inj+2]

    # Reset interval to be 0
    revs$interval <- revs$interval - inj.int + 1

    # Remove bins before mouse back on
    revs <- subset(revs, interval >= 0)

    # Make a sequence to use for aggregating
    minbin <- rep(0:(max(revs$interval)%/%10), each = 10, length = nrow(revs))
    revs <- cbind(revs, minbin)

    # Aggregate total revolutions per bin
    aggr.sum <- aggregate(
	    list(run = revs[,1]),
	        by = list(bin = minbin),
	    FUN = sum)

    # Store aggregated wheel running data to temporary data frame
    aggr$run0[aggr$whlnum == i] <- aggr.sum$run[1]
    aggr$run1[aggr$whlnum == i] <- aggr.sum$run[2]
    aggr$run2[aggr$whlnum == i] <- aggr.sum$run[3]
    aggr$run3[aggr$whlnum == i] <- aggr.sum$run[4]
    aggr$run4[aggr$whlnum == i] <- aggr.sum$run[5]
    aggr$run5[aggr$whlnum == i] <- aggr.sum$run[6]
    aggr$run6[aggr$whlnum == i] <- aggr.sum$run[7]
    aggr$run7[aggr$whlnum == i] <- aggr.sum$run[8]
    aggr$run8[aggr$whlnum == i] <- aggr.sum$run[9]
    aggr$run9[aggr$whlnum == i] <- aggr.sum$run[10]
    aggr$run10[aggr$whlnum == i] <- aggr.sum$run[11]
    aggr$run11[aggr$whlnum == i] <- aggr.sum$run[12]
    aggr$run12[aggr$whlnum == i] <- aggr.sum$run[13]

    # Aggregate max revolutions in bin
    aggr.max <- aggregate(
	    list(maxes = revs[,1]),
	        by = list(bin = minbin),
	    FUN = max)
    aggr$max0[aggr$whlnum == i] <- aggr.max$maxes[1]
    aggr$max1[aggr$whlnum == i] <- aggr.max$maxes[2]
    aggr$max2[aggr$whlnum == i] <- aggr.max$maxes[3]
    aggr$max3[aggr$whlnum == i] <- aggr.max$maxes[4]
    aggr$max4[aggr$whlnum == i] <- aggr.max$maxes[5]
    aggr$max5[aggr$whlnum == i] <- aggr.max$maxes[6]
    aggr$max6[aggr$whlnum == i] <- aggr.max$maxes[7]
    aggr$max7[aggr$whlnum == i] <- aggr.max$maxes[8]
    aggr$max8[aggr$whlnum == i] <- aggr.max$maxes[9]
    aggr$max9[aggr$whlnum == i] <- aggr.max$maxes[10]
    aggr$max10[aggr$whlnum == i] <- aggr.max$maxes[11]
    aggr$max11[aggr$whlnum == i] <- aggr.max$maxes[12]
    aggr$max12[aggr$whlnum == i] <- aggr.max$maxes[13]

    # Count number of intervals with >0 revs in bin
    # Need to define a function to count intervals. This works because
    #   sum(toagr > 0) is coerced to numeric (0/1)
    calc.int <- function(toagr){sum(toagr > 0)}
    aggr.int <- aggregate(
	    list(ints = revs[,1]),
	        by = list(bin = minbin),
	    FUN = calc.int)
    aggr$int0[aggr$whlnum == i] <- aggr.int$ints[1]
    aggr$int1[aggr$whlnum == i] <- aggr.int$ints[2]
    aggr$int2[aggr$whlnum == i] <- aggr.int$ints[3]
    aggr$int3[aggr$whlnum == i] <- aggr.int$ints[4]
    aggr$int4[aggr$whlnum == i] <- aggr.int$ints[5]
    aggr$int5[aggr$whlnum == i] <- aggr.int$ints[6]
    aggr$int6[aggr$whlnum == i] <- aggr.int$ints[7]
    aggr$int7[aggr$whlnum == i] <- aggr.int$ints[8]
    aggr$int8[aggr$whlnum == i] <- aggr.int$ints[9]
    aggr$int9[aggr$whlnum == i] <- aggr.int$ints[10]
    aggr$int10[aggr$whlnum == i] <- aggr.int$ints[11]
    aggr$int11[aggr$whlnum == i] <- aggr.int$ints[12]
    aggr$int12[aggr$whlnum == i] <- aggr.int$ints[13]

    # Calculate RPM from total revs and ints
    aggr$rpm0[aggr$whlnum == i] <- aggr.sum$run[1] / aggr.int$ints[1]
    aggr$rpm1[aggr$whlnum == i] <- aggr.sum$run[2] / aggr.int$ints[2]
    aggr$rpm2[aggr$whlnum == i] <- aggr.sum$run[3] / aggr.int$ints[3]
    aggr$rpm3[aggr$whlnum == i] <- aggr.sum$run[4] / aggr.int$ints[4]
    aggr$rpm4[aggr$whlnum == i] <- aggr.sum$run[5] / aggr.int$ints[5]
    aggr$rpm5[aggr$whlnum == i] <- aggr.sum$run[6] / aggr.int$ints[6]
    aggr$rpm6[aggr$whlnum == i] <- aggr.sum$run[7] / aggr.int$ints[7]
    aggr$rpm7[aggr$whlnum == i] <- aggr.sum$run[8] / aggr.int$ints[8]
    aggr$rpm8[aggr$whlnum == i] <- aggr.sum$run[9] / aggr.int$ints[9]
    aggr$rpm9[aggr$whlnum == i] <- aggr.sum$run[10] / aggr.int$ints[10]
    aggr$rpm10[aggr$whlnum == i] <- aggr.sum$run[11] / aggr.int$ints[11]
    aggr$rpm11[aggr$whlnum == i] <- aggr.sum$run[12] / aggr.int$ints[12]
    aggr$rpm12[aggr$whlnum == i] <- aggr.sum$run[13] / aggr.int$ints[13]
}

# Replace NA rpms (results of div by zero) with 0
aggr <- data.frame(sapply(aggr, function(x) {x[is.na(x)] <- 0; x}))

# Send back the aggregated data frame
aggr
}