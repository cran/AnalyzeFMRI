\name{f.read.volume}
\alias{f.read.volume}
\title{read whole image file}
\description{Reads the ANALYZE or NIFTI image format image file into an array. Autodetects format type.}
\usage{f.read.volume(file)}
\arguments{\item{file}{The location of the image file to be read}}
\value{An array with the appropriate dimensions containing the image
  volume. A print out of the file information is also given. The
  function assumes that the corresponding .hdr file is in the same
  directory as the .img file. (but if it is a .nii file)}
\seealso{
  \code{\link{f.read.nifti.slice}},
  \code{\link{f.read.nifti.slice.at.all.timepoints}},
  \code{\link{f.read.nifti.ts}}
}
\examples{
a <- f.read.volume(system.file("example-nifti.img", package = "AnalyzeFMRI"))
dim(a)
}
\keyword{utilities}

