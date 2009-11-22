\name{plot.running}
\alias{plot.running}
\title{Plot Aggregated Wheel Running Data}
\description{
This function plots aggregated wheel running data.
}

\usage{
\method{plot}{running}(x, whlnum, whichplot = "run", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{an object of class \code{running}, most likely returned by \code{bin.running}.}
  \item{whlnum}{number of wheel to plot running data for. Currently only one number is accepted.}
  \item{whichplot}{which set of aggregated data will be plotted: \code{run}, \code{max}, \code{int}, and \code{rpm} are valid.}
  \item{...}{further arguments passed to \code{plot}}
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
plot(A.aggr, whlnum = 1, whichplot = 'run', col = 'red')
plot(A.aggr, whlnum = 1, whichplot = 'max')
plot(A.aggr, whlnum = 1, whichplot = 'int')
plot(A.aggr, whlnum = 1, whichplot = 'rpm')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ data }
\keyword{ aplot }
