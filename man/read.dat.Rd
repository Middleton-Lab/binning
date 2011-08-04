\name{read.dat}
\alias{read.dat}
\title{Read Wheel Running Data}
\usage{
  read.dat(file, tab.delim = FALSE)
}
\arguments{
  \item{file}{either a comma-separated (.DAT) or
  tab-delimited (.txt) wheel running data file}

  \item{tab.delim}{Logical for whether the file is tab
  delimited or not. Default is FALSE, because unmodified
  DAT files are comma-separated. If the file has been
  opened, modified, and resaved, then it is probably tab
  delimited.}
}
\value{
  A data.frame of wheel running data.
}
\description{
  Read Wheel Running Data
}
\details{
  Read either a comma-separated (.DAT) or tab-delimited
  (.txt) wheel running data file.
}
\author{
  Kevin Middleton (kmm@csusb.edu)
}
\keyword{data}

