\name{xyzt2st}
\alias{xyzt2st}
\title{xyzt2st}
\description{Extract space and time dimension codes from the one byte xyzt.units field of a NIFTI header file.}
\usage{xyzt2st(xyzt.units)}
\arguments{\item{xyzt.units}{xyzt.units field of a NIFTI header file}}
\value{A list containing space and time fields.

See also the Value Section of the help file of function st2xyzt().
  
}
\seealso{
  \code{\link{st2xyzt}}
}
\examples{
xyzt.units <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$xyzt.units
xyzt2st(xyzt.units)
}
\keyword{utilities}



