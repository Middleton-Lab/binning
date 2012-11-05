\name{analyze_running}
\alias{analyze_running}
\title{Analyze running (new system)}
\usage{
  analyze_running(file, drop.days = NULL)
}
\arguments{
  \item{file}{Character string giving the \code{.csv} file
  to analyze. This file should have been written by
  \code{\link{aggregate_running}}.}

  \item{drop.days}{Vector of character strings giving any
  dates (e.g., \code{"2011-12-31"}) that should be dropped
  from the analysis.}
}
\value{
  A data.frame with the wheels and summed revolutions for
  the last two days of running.
}
\description{
  Analyze running (new system)
}
\details{
  Analyze aggregated running data. Includes plots and
  summary output.
}
\author{
  Kevin Middleton (middletonk@missouri.edu)
}
\keyword{data}
\keyword{file}

