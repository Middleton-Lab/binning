#######################################################
# function lineaggr
# Aggregate by line
lineaggr <- function(dat){

aggregate(
    list(dat),
    by = list(dat$line, dat$sex),
    FUN = mean)
}
