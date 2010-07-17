\name{f.read.nifti.slice}
\alias{f.read.nifti.slice}
\title{read one slice from a .img or .nii file in NIFTI format}
\description{Reads in a specific slice from a NIFTI .img or .nii image format
  file into an array.}
\usage{f.read.nifti.slice(file, slice, tpt)}
\arguments{
  \item{file}{The .img file to be read from}
  \item{slice}{The number of the slice (assumed to be the 3rd dimension)}
  \item{tpt}{The number of the scan that the slice is to be taken from}}
\details{The entire dataset is assumed to be 4D and a slice is extracted
  that is referenced by specifying the last two dimensions of the
  dataset i.e.slice and tpt.}
\value{An array containing the slice}
\seealso{
  \code{\link{f.read.nifti.slice.at.all.timepoints}},
  \code{\link{f.read.nifti.ts}},
  \code{\link{f.read.nifti.volume}}
 
}
\examples{
a<-f.read.nifti.slice(system.file("example-nifti.img", package="AnalyzeFMRI"),10,1)
dim(a)
}
\keyword{utilities}
