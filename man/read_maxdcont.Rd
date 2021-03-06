% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_maxdcont.R
\name{read_maxdcont}
\alias{read_maxdcont}
\title{Read AERMOD MAXDCONT Output Files}
\usage{
read_maxdcont(infile)
}
\arguments{
\item{infile}{Input maxdcont file to read.}
}
\description{
Read a table of AERMOD pollutant contributions from the MAXDCONT option
into a \code{\link{data.frame}}.
}
\examples{
read_maxdcont(project/pm25_maxdcont.TXT)
}
\keyword{maxdcont}
