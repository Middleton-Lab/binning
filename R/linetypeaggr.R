#######################################################
# function linetypeaggr
# Aggregate by linetype
linetypeaggr <- function(dat){

aggregate(
    list(dat),
    by = list(dat$linetype, dat$sex),
    FUN = mean)
}
