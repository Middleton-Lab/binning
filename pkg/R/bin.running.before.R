bin.running.before <- function(whldat,
                        bin.size,
                        bin.start,
                        computer){

##########################################
# BEGIN COPIED FROM bin.running.R
if (ncol(whldat) != 157){
    if (ncol(whldat) != 151){
        stop("The wheel running file does not have the correct number of columns.", call. = FALSE)
    }
}
if (!match(computer, c('A', 'B', 'C', 'D'), nomatch = FALSE, incomparables = FALSE)){
	stop("computer must be one of \'A\', \'B\', \'C\', or \'D\'.", call. = FALSE)}
numwhl <- (ncol(whldat)-7)/3
if (computer == 'A' & numwhl != 50){
	stop("computer A should have 50 wheels", call. = FALSE)}
if (computer == 'B' & numwhl != 50){
	stop("computer B should have 50 wheels", call. = FALSE)}
if (computer == 'C' & numwhl != 50){
	stop("computer C should have 50 wheels", call. = FALSE)}
if (computer == 'D' & numwhl != 48){
	stop("computer D should have 48 wheels", call. = FALSE)}
whldat.str <- whldat[ , -c(3:7, seq(8, ncol(whldat), 3), seq(9, ncol(whldat), 3))]
names(whldat.str)[1] <- "hr"
names(whldat.str)[2] <- "min"
names(whldat.str)[3:(numwhl+2)] <- paste(1:numwhl, sep = "")
whldat.str <- cbind(whldat.str, interval = 1:nrow(whldat))
##########################################
# END COPIED FROM bin.running.R

# Reset interval to be 0
interval <- NULL; rm(interval)
whldat.str$interval <- whldat.str$interval - (bin.start - bin.size - 1)

# Keep only intervals 1-30
revs <- subset(whldat.str, interval > 0 & interval <= 30)[, -c(1, 2, ncol(whldat.str))]

run <- apply(revs, 2, sum)
max <- apply(revs, 2, max)
int <- apply(revs, 2, FUN = function(x){sum(x > 0)})
rpm <- ifelse(run == 0, 0, run / int)

list(run = run, max = max, int = int, rpm = rpm)

}

