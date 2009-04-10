#######################################################
# function: read.dat
read.dat <- function(file, tab.delim = FALSE){

# function to read .DAT files from wheel running computers
# Created by Kevin Middleton (kmm@csusb.edu)
# 10 Aug 2007 - First working version

# file: path to DAT file
# tab.delim: T/F for whether the file is tab delimited or not. Default is FALSE,
#   becuse unmodified DAT files are comma-separated.
# Returns a matrix whldat

if (tab.delim) {
    whldat <- read.delim(file, header = FALSE, sep = '\t')
    } else {
    whldat <- read.csv(file, header = FALSE)}
whldat
}
