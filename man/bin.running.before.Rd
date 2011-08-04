\name{bin.running.before}
\alias{bin.running.before}
\title{Aggregate Wheel Running Data Prior to a Bin}
\usage{
  bin.running.before(bin.size, bins.out, bin.start, reverse
  = FALSE, ...)
}
\arguments{
  \item{bin.size}{Size of bin (minutes)}

  \item{bins.out}{Number of bins to return}

  \item{bin.start}{Bin (interval) to start}

  \item{reverse}{Should the resulting dat be reversed
  before being returned? Reversed data will go from most
  recent to oldest.}

  \item{...}{Additional parameters passed to
  \code{bin.running()}}
}
\value{
  An object of class \code{running}. See
  \code{\link{bin.running}} for additional details.
}
\description{
  Aggregate Wheel Running Data Prior to a Bin
}
\details{
  Aggregate wheel running data prior to a specfic bin
  (i.e., computer interval number).
}
\examples{
# Load the 2006-11-05 running data for computer A
data(A061105)

# Aggregate running data into 10 * 10 minute bins starting
# at interval 600
A <- bin.running.before(whldat = A061105, computer = "A",
bin.start = 600,
bin.size = 10, bins.out = 10)
A$times
A$run
# Plot aggregated running data for wheel number 1.
plot(A, whlnum = 1, whichplot = "run")
}
\author{
  Kevin Middleton (kmm@csusb.edu)
}
\seealso{
  \code{\link{bin.running}}
}
\keyword{data}
\keyword{file}

