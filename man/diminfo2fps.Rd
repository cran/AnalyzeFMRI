\name{diminfo2fps}
\alias{diminfo2fps}
\title{diminfo2fps}
\description{Extract freq.dim, phase.dim and slice.dim fields from the one
  byte dim.info field of a NIFTI header file.}
\usage{diminfo2fps(dim.info)}
\arguments{\item{dim.info}{dim.info field of a NIFTI header file}}
\value{A list containing \code{freq.dim}, \code{phase.dim} and \code{slice.dim} fields.

   These are provided to store some extra information
   that is sometimes important when storing the image data
   from an FMRI time series experiment.  (After processing such
   data into statistical images, these fields are not likely
   to be useful.) These fields encode which spatial dimension (1,2, or
   3) corresponds to which acquisition dimension for MRI data.

    Examples:\cr
      Rectangular scan multi-slice EPI:\cr
        \code{freq_dim} = 1  \code{phase_dim} = 2  \code{slice_dim} = 3  (or some permutation)\cr
      Spiral scan multi-slice EPI:\cr
        \code{freq_dim} = \code{phase_dim} = 0  \code{slice_dim} = 3
        since the concepts of frequency- and phase-encoding directions
        don't apply to spiral scan.
      
      
  The fields \code{freq.dim}, \code{phase.dim}, \code{slice.dim} are all
  squished into the single byte field \code{dim.info} (2 bits each, since the
  values for each field are limited to the range 0..3).  This
  unpleasantness is due to lack of space in the 348 byte allowance.
}
\seealso{
  \code{\link{fps2diminfo}}
}
\examples{
dim.info <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$dim.info
diminfo2fps(dim.info)
}
\keyword{utilities}



