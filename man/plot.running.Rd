\name{plot.running}
\alias{plot.running}
\title{Plot Aggregated Wheel Running Data}
\usage{
  \method{plot}{running} (x, whlnum, whichplot = "run",
  ...)
}
\arguments{
  \item{x}{an object of class \code{running}, most likely
  returned by \code{bin.running}.}

  \item{whlnum}{number of wheel to plot running data for.
  Currently only one number is accepted.}

  \item{whichplot}{which set of aggregated data will be
  plotted: \code{run}, \code{max}, \code{int}, and
  \code{rpm} are valid.}

  \item{\dots}{further arguments passed to \code{plot}}
}
\description{
  Plot Aggregated Wheel Running Data
}
\details{
  This function plots aggregated wheel running data.
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
\author{
  Kevin Middleton (kmm@csusb.edu)
}
\seealso{
  \code{\link{read.dat}}, \code{\link{bin.running}}
}
\keyword{data}

