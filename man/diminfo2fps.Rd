\name{diminfo2fps}
\alias{diminfo2fps}
\title{diminfo2fps}
\description{Extract freq.dim, phase.dim and slice.dim fields from the one
  byte dim.info field of a NIFTI header file.}
\usage{diminfo2fps(dim.info)}
\arguments{\item{dim.info}{dim.info field of a NIFTI header file}}
\value{A list containing freq.dim, phase.dim and slice.dim fields.}
\seealso{
  \code{\link{fps2diminfo}}
}
\examples{
dim.info <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$dim.info
diminfo2fps(dim.info)
}
\keyword{utilities}



