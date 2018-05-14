\name{orientation}
\alias{orientation}
\title{Orientation storage}
\description{To determine if data is stored in Radiological or
  Neurological order.}
\usage{orientation(L)}
\arguments{\item{L}{a NIFTI header list}
}
\value{-1 for Radiological and 1 for Neurological.}
\examples{
L <- f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
orientation(L)
}
\keyword{utilities}



