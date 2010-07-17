\name{st2xyzt}
\alias{st2xyzt}
\title{st2xyzt}
\description{Encode and assemble a space code with a time code dimension into the combined one byte xyzt.units field of a NIFTI header file.}
\usage{st2xyzt(space,time)}
\arguments{\item{space}{space field of a NIFTI file}
  \item{time}{time field of a NIFTI file}
}
\value{A list containing xyzt.units field.

   Bits 0..2 of xyzt.units specify the units of pixdim[2..4]
    (e.g., spatial units are values 0,1,2,...,7).
   Bits 3..5 of xyzt.units specify the units of pixdim[5]
    (e.g., temporal units are multiples of 8: 0,8,16,24,32,40,48,56).

   This compression of 2 distinct concepts into 1 byte is due to the
   limited space available in the 348 byte ANALYZE 7.5 header.

   Some NIFTI codes: 0 (unspecified units), 1 (meters), 2 (millimeters), 
   3 (micrometers), 8 (seconds), 16 (milliseconds), 24 (microseconds), 
   32 (Hertz), 40 (ppm, part per million) and 48 (radians per second).

}
\seealso{
  \code{\link{xyzt2st}}
}
\examples{
xyzt.units <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$xyzt.units
mylist <- xyzt2st(xyzt.units)
st2xyzt(mylist$space,mylist$time)
}
\keyword{utilities}



