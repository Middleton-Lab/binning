\name{plot.running}
\Rdversion{0.02}
\alias{plot.running}
\title{Plot Aggregated Wheel Running Data}
\description{
Plot aggregated wheel running data for a single wheel.
}
\usage{
plot.running(running, time.data, whlnum, whichplot = "run")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{running}{data.frame. aggregated wheel running data from \code{bin.running}.}
  \item{time.data}{data.frame. time information for \code{running} from \code{bin.running}.}
  \item{whlnum}{number of wheel to plot running data for. Currently only one number is accepted.}
  \item{whichplot}{which set of variables will be plotted. Currently only \code{run} is implemented. Future support for \code{max}, \code{int}, and \code{rpm} are planned.}
}
%%\details{
%%  ~~ If necessary, more details than the description above ~~
%%}
%%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%%}
%%\references{
%% ~put references to the literature/web site here ~
%%}
\author{
Kevin Middleton (kmm@csusb.edu)
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.dat}}, \code{\link{bin.running}}
}
\examples{
# Load the 2006-11-05 running data for computer A
data(A061105)

# Aggregate running data into 20 minute bins
A.aggr <- bin.running(A061105, computer = 'A', bin.size = 20)

# Plot aggregated data for wheel number 1.
plot.running(A.aggr[[1]], A.aggr[[2]], whlnum = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ aplot }
