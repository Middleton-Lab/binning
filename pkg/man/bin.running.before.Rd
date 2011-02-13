\name{bin.running.before}
\alias{bin.running.before}
\title{Aggregate Wheel Running Data Prior to a Bin}
\usage{bin.running.before(whldat, bin.size, bin.start, computer)
}
\description{Aggregate Wheel Running Data Prior to a Bin}
\details{Aggregate wheel running data prior to a specfic bin}
\alias{bin.running.before}
\value{A list containing
\item{run}{Aggregated total revolutions per bin}
\item{max}{Maximum number of revolutions per interval in a bin}
\item{int}{Number of intervals in a bin with >0 revolutions}
\item{rpm}{Mean RPM in a bin}}
\author{Kevin Middleton}
\arguments{\item{whldat}{A wheel data file}
\item{bin.size}{Size of bin (minutes)}
\item{bin.start}{Bin to start}
\item{computer}{Name of computer (one of 'A', 'B', 'C', 'D', with quotes)}
}

