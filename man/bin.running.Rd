\name{bin.running}
\Rdversion{0.02}
\alias{bin.running}
\title{Aggregate Wheel Running Data}
\description{
Given an arbitrary bin size in minutes and an arbitrary interval number from which to start aggregating, \code{bin.running} takes a .DAT (comma-separated) or .txt (tab-delimited) wheel running data file read in using \code{\link{read.dat}} and aggregates wheel revolutions, the maximum interval per bin, the number of intervals per bin with >0 revolutions, and the mean RPM per bin.
}
\usage{
bin.running(whldat, bin.size, bins.out = "ALL", bin.start = 1, start.at.1pm = TRUE, computer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{whldat}{an \code{n} intervals by 157 (computers A-C) or 151 (computer D) column data.frame, most often read in using \code{\link{read.dat}.}}
  \item{bin.size}{the size (in minutes) of the bins to return. Defaults to 10 minute bins.}
  \item{bins.out}{number of bins to return. Defaults to \code{\dQuote{ALL}} -- Start at the first row and bin all the way to the end of the file, removing an incomplete last bin (if necessary).}
  \item{bin.start}{the interval number where the binning should start. Defaults to 1.}
  \item{start.at.1pm}{logical. if \code{TRUE} (default), \code{bin.running} will delete all intervals before 1:00 PM.}
  \item{computer}{text string code for which computer recorded \code{whldata}.}
}
%%\details{
%%Wheel running data files can be read using \code{\link{read.dat}}. Aggregating proceeds 
%%}
\value{
\code{bin.running} returns a list of two elements:
\item{aggr}{the aggregated wheel running data. The dimensions are \code{[bins.out, 1+4*bins.out]}. Contains sum of revolutions per bin (\code{run_XX}), the maximum number of revolutions in a single interval per bin (\code{max_XX}), number of intervals per bin with >0 revolutions (\code{int_XX}), and the mean number of revolutions per minute for a bin (\code{rpm_XX}).}
\item{bin.times}{\code{data.frame} with columns for \code{bin}, \code{hour}, and \code{minute}.}
}
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
\code{\link{read.dat}}, \code{\link{plot.running}}
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
\keyword{ file }
\keyword{ data }
