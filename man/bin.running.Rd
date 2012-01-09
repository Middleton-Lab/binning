\name{bin.running}
\alias{bin.running}
\title{Aggregate Wheel Running Data}
\usage{
  bin.running(whldat, bin.size, bins.out = "ALL",
    bin.start = 1, start.at.1pm = TRUE, computer,
    header = NULL)
}
\arguments{
  \item{whldat}{an \code{n} intervals by 157 (computers
  A-C) or 151 (computer D) column data.frame, most often
  read in using \code{\link{read.dat}.}}

  \item{bin.size}{the size (in minutes) of the bins to
  return. Defaults to 10 minute bins.}

  \item{bins.out}{number of bins to return. Defaults to
  \code{'ALL'} -- Start at the first row and bin all the
  way to the end of the file, removing an incomplete last
  bin (if necessary).}

  \item{bin.start}{the interval number where the binning
  should start.  Defaults to 1.}

  \item{start.at.1pm}{logical. if \code{TRUE} (default),
  \code{bin.running} will delete all intervals before 1:00
  PM.}

  \item{computer}{text string code for which computer
  recorded \code{whldata}.}

  \item{header}{optional text string that can be used to
  label plots made with \code{\link{plot.running}}}
}
\value{
  \code{bin.running} returns an object of class
  \code{running}.  This list contains \item{whlnum}{A
  vector of wheel numbers appropriate for \code{computer}}
  \item{run}{Aggregated total revolutions per bin}
  \item{max}{Maximum number of revolutions per interval in
  a bin} \item{int}{Number of intervals in a bin with >0
  revolutions} \item{rpm}{Mean RPM in a bin}
  \item{times}{\code{data.frame} with columns for
  \code{bin}, \code{hour}, and \code{minute} denoting the
  time at the beginning of each bin} \item{whldat}{Original
  raw wheel running data} \item{bin.size}{Length in minutes
  of the bins} \item{n.bins}{Number of bins into which data
  was aggregated} \item{bin.start}{Interval where binning
  started} \item{start.at.1pm}{logical. If \code{TRUE}, all
  intervals before 1:00 PM were truncated}
  \item{computer}{Computer from which the data were
  recorded} \item{header}{Optional text string header
  identifying the data}
}
\description{
  Aggregate Wheel Running Data
}
\details{
  Given an arbitrary bin size in minutes and an arbitrary
  interval number from which to start aggregating,
  \code{bin.running} takes a .DAT (comma-separated) or .txt
  (tab-delimited) wheel running data file read in using
  \code{\link{read.dat}} and aggregates wheel revolutions,
  the maximum interval per bin, the number of intervals per
  bin with >0 revolutions, and the mean RPM per bin.
}
\examples{
# Load the 2006-11-05 running data for computer A
data(A061105)

# Aggregate running data into 20 minute bins
A.aggr <- bin.running(A061105, computer = "A", bin.size = 20)

# Plot aggregated running data for wheel number 1.
plot(A.aggr, whlnum = 1, whichplot = "run")
}
\author{
  Kevin Middleton (kmm@csusb.edu)
}
\seealso{
  \code{\link{read.dat}}, \code{\link{plot.running}}
}
\keyword{data}
\keyword{file}

