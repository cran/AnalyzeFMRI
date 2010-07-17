\name{xyzt2st}
\alias{xyzt2st}
\title{xyzt2st}
\description{Extract space and time dimension codes from the one byte xyzt.units field of a NIFTI header file.}
\usage{xyzt2st(xyzt.units)}
\arguments{\item{xyzt.units}{xyzt.units field of a NIFTI header file}}
\value{A list containing space and time fields.

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
  \code{\link{st2xyzt}}
}
\examples{
xyzt.units <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$xyzt.units
xyzt2st(xyzt.units)
}
\keyword{utilities}



