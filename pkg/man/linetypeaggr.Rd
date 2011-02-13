\name{linetypeaggr}
\alias{linetypeaggr}
\title{Aggregate by Linetype}
\usage{linetypeaggr(dat)
}
\description{Aggregate by Linetype}
\details{Aggregate wheel running data by linetype.}
\value{Data aggregated by linetype}
\author{Kevin Middleton (kmm@csusb.edu)}
\seealso{\code{\link{lineaggr}}}
\keyword{univar}
\arguments{\item{dat}{A data.frame}
}
\examples{## The function is currently defined as
function(dat){

aggregate(
list(dat),
by = list(dat$linetype, dat$sex),
FUN = mean)
}}

