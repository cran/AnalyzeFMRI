\name{Q2R}
\alias{Q2R}
\title{Quaternion to rotation}
\description{Generate a (proper) rotation matrix from a quaternion.}
\usage{Q2R(Q,qfac)}
\arguments{\item{Q}{quaternion vector}
  \item{qfac}{qfac nifti field. It is pixdim[1]}
}
\value{The rotation.}
\seealso{
  \code{\link{R2Q}}
}
\examples{
L <- f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
Q <- c(L$quatern.b,L$quatern.c,L$quatern.d)
Q2R(Q,L$pixdim[1])
}
\keyword{utilities}



