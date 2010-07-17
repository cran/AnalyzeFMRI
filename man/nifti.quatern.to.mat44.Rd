\name{nifti.quatern.to.mat44}
\alias{nifti.quatern.to.mat44}
\title{Quaternion (etc..) to affine 4x4 matrix}
\description{Generate a 4x4 affine matrix from a NIFIT header list.}
\usage{nifti.quatern.to.mat44(L)}
\arguments{\item{L}{a NIFTI header list}
}
\value{The 4x4 affine matrix.}
\seealso{
  \code{\link{R2Q}}
  \code{\link{Q2R}}
}
\examples{
L <- f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
nifti.quatern.to.mat44(L)
}
\keyword{utilities}



