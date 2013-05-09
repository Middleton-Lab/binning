\name{aggregate_running}
\alias{aggregate_running}
\title{Aggregate running (new system)}
\usage{
  aggregate_running(all = FALSE, by = "day",
    analyze = FALSE, ...)
}
\arguments{
  \item{all}{boolean: If \code{TRUE} (default), all
  \code{.dat} files in the working directory will be
  processed. Note that if \code{all = TRUE}, then all dates
  must be represented by at least one \code{.dat} file.}

  \item{by}{The period of time for aggregation. This
  parameter gets passed to \code{seq.POSIXt()} and should
  be one of: \code{"day"} (default), \code{"min"},
  \code{"hour"}, etc. See \code{\link{seq.POSIXt}} for more
  options.}

  \item{analyze}{boolean: If \code{TRUE} (default is
  \code{FALSE}), also run \code{analyze_runnung()}.}

  \item{...}{If \code{all = TRUE}, then \code{start} and
  \code{end} should also be supplied as a quoted string in
  the format \code{"2011-12-31"}.}
}
\description{
  Aggregate running (new system)
}
\details{
  Aggregate running data from new system. Writes a file
  that includes the aggregated data.
}
\author{
  Kevin Middleton {middletonk@missouri.edu}
}
\seealso{
  \code{\link{analyze_running}}
}
\keyword{data}
\keyword{file}

