\name{diminfo2fps}
\alias{diminfo2fps}
\title{diminfo2fps}
\description{Extract freq.dim, phase.dim and slice.dim fields from the one
  byte dim.info field of a NIFTI header file.}
\usage{diminfo2fps(dim.info)}
\arguments{\item{dim.info}{dim.info field of a NIFTI header file}}
\value{A list containing freq.dim, phase.dim and slice.dim fields.

   A few fields are provided to store some extra information
   that is sometimes important when storing the image data
   from an FMRI time series experiment.  (After processing such
   data into statistical images, these fields are not likely
   to be useful.)

   freq_dim   = These fields encode which spatial dimension (1,2, or 3)
   phase_dim  = corresponds to which acquisition dimension for MRI data.
   slice_dim  =
    Examples:
      Rectangular scan multi-slice EPI:
        freq_dim = 1  phase_dim = 2  slice_dim = 3  (or some permutation)
      Spiral scan multi-slice EPI:
        freq_dim = phase_dim = 0  slice_dim = 3
        since the concepts of frequency- and phase-encoding directions
        don't apply to spiral scan

  The fields freq_dim, phase_dim, slice_dim are all squished into the single
  byte field dim_info (2 bits each, since the values for each field are
  limited to the range 0..3).  This unpleasantness is due to lack of space
  in the 348 byte allowance.
}
\seealso{
  \code{\link{fps2diminfo}}
}
\examples{
dim.info <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$dim.info
diminfo2fps(dim.info)
}
\keyword{utilities}



