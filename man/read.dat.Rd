\name{read.dat}
\Rdversion{1.1}
\alias{read.dat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.dat(file, tab.delim = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
}
  \item{tab.delim}{
%%     ~~Describe \code{tab.delim} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(file, tab.delim = FALSE){

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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
