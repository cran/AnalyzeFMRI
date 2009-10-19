\name{st2xyzt}
\alias{st2xyzt}
\title{st2xyzt}
\description{Encode space and time dimensions fields into the one byte xyzt.units field of a NIFTI header file.}
\usage{st2xyzt(space,time)}
\arguments{\item{space}{space field of a NIFTI file}
  \item{time}{time field of a NIFTI file}
}
\value{A list containing xyzt.units field.}
\seealso{
  \code{\link{xyzt2st}}
}
\examples{
xyzt.units <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$xyzt.units
mylist <- xyzt2st(xyzt.units)
st2xyzt(mylist$space,mylist$time)
}
\keyword{utilities}



