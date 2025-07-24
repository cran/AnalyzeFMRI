\name{f.read.nifti.volume}
\alias{f.read.nifti.volume}
\title{read whole image file}
\description{Reads the NIFTI image file into an array.}
\usage{f.read.nifti.volume(file)}
\arguments{\item{file}{The location of the image file to be read}}
\value{An array with the appropriate dimensions containing the image
  volume. A print out of the file information is also given. The
  function assumes that the corresponding .hdr file is in the same
  directory as the .img file (but if a .nii file is provided). }
\seealso{
  \code{\link{f.read.nifti.slice}},
  \code{\link{f.read.nifti.slice.at.all.timepoints}},
  \code{\link{f.read.nifti.ts}}
}
\examples{
a <- f.read.nifti.volume(system.file("example-nifti.img", package = "AnalyzeFMRI"))
dim(a)
}
\keyword{utilities}

