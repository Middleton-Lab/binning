\name{plot.running}
\Rdversion{1.1}
\alias{plot.running}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot.running(running, time.data, whlnum, whichplot = "run")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{running}{
%%     ~~Describe \code{running} here~~
}
  \item{time.data}{
%%     ~~Describe \code{time.data} here~~
}
  \item{whlnum}{
%%     ~~Describe \code{whlnum} here~~
}
  \item{whichplot}{
%%     ~~Describe \code{whichplot} here~~
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
function(running, time.data, whlnum, whichplot = 'run'){

# Receive a list output from bin.running
# Options to plot run, max, int, rpm

n.bins <- nrow(time.data)

subs <- running[whlnum,]

if (whichplot == 'run') {
    dat <- t(subs[,2:(n.bins+1)])
    ylabel <- 'Revolutions per bin'
  }

hrmin <- strptime(paste(time.data$hr, time.data$min, '00', sep = ':'), '\%T')
hrmin <- format(hrmin, '\%H:\%M')

plot(dat, type = 'l', axes = FALSE, ylab = ylabel, xlab = 'Hour')
axis(1, at = seq(1, n.bins, 12), labels = hrmin[seq(1, n.bins, 12)], las = 2, cex.axis = 0.75)
axis(2)
box()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
