\name{f.read.nifti.slice.at.all.timepoints}
\alias{f.read.nifti.slice.at.all.timepoints}

\title{reads a slice at all time points from a NIFTI .img or .nii file}
\description{Reads in a slice of a .img or .nii file at all time points into an array}
\usage{f.read.nifti.slice.at.all.timepoints(file, slice)
}
\arguments{
  \item{file}{\code{file} The location of the .img file}
  \item{slice}{\code{slice} The number of the slice to be read in}
}
\value{An array containing the slice at all time points
}
\seealso{
  \code{\link{f.read.nifti.slice}},
  \code{\link{f.read.nifti.ts}},
  \code{\link{f.read.nifti.volume}}   

}
\examples{
a<-f.read.nifti.slice.at.all.timepoints(system.file("example-nifti.img", package="AnalyzeFMRI"),10)
dim(a)
}
\keyword{utilities}
