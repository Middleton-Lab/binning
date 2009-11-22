\name{bin.running}
\alias{bin.running}
\title{Aggregate Wheel Running Data}
\description{
Given an arbitrary bin size in minutes and an arbitrary interval number from which to start aggregating, \code{bin.running} takes a .DAT (comma-separated) or .txt (tab-delimited) wheel running data file read in using \code{\link{read.dat}} and aggregates wheel revolutions, the maximum interval per bin, the number of intervals per bin with >0 revolutions, and the mean RPM per bin.
}
\usage{
bin.running(whldat, bin.size, bins.out = "ALL", bin.start = 1, start.at.1pm = TRUE, computer, header = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{whldat}{an \code{n} intervals by 157 (computers A-C) or 151 (computer D) column data.frame, most often read in using \code{\link{read.dat}.}}
  \item{bin.size}{the size (in minutes) of the bins to return. Defaults to 10 minute bins.}
  \item{bins.out}{number of bins to return. Defaults to \code{'ALL'} -- Start at the first row and bin all the way to the end of the file, removing an incomplete last bin (if necessary).}
  \item{bin.start}{the interval number where the binning should start. Defaults to 1.}
  \item{start.at.1pm}{logical. if \code{TRUE} (default), \code{bin.running} will delete all intervals before 1:00 PM.}
  \item{computer}{text string code for which computer recorded \code{whldata}.}
  \item{header}{optional text string that can be used to label plots made with \code{\link{plot.running}}}
}
%%\details{
%%Wheel running data files can be read using \code{\link{read.dat}}. Aggregating proceeds 
%%}
\value{
\code{bin.running} returns an object of type \code{running}.
\item{whlnum}{a vector of wheel numbers appropriate for \code{computer}}
\item{run}{aggregated total revolutions per bin}
\item{max}{maximum number of revolutions per interval in a bin}
\item{int}{number of intervals in a bin with >0 revolutions}
\item{rpm}{mean RPM in a bin.}
\item{times}{\code{data.frame} with columns for \code{bin}, \code{hour}, and \code{minute} denoting the time at the beginning of each bin.}
\item{whldat}{the original raw wheel running data}
\item{bin.size}{the length in minutes of the bins}
\item{n.bins}{the number of bins into which data was aggregated}
\item{bin.start}{the interval where binning started}
\item{start.at.1pm}{logical. If \code{TRUE}, all intervals before 1:00 PM were truncated}
\item{computer}{the computer from which the data were recorded}
\item{header}{an optional text string header identifying the data}
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

# Plot aggregated running data for wheel number 1.
plot(A.aggr, whlnum = 1, whichplot = 'run')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ file }
\keyword{ data }
