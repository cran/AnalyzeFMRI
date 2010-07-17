\name{R2Q}
\alias{R2Q}
\title{Rotation to quaternion}
\description{Convert from (proper) rotation matrix to quaternion form.}
\usage{R2Q(R,qfac=NULL)}
\arguments{\item{R}{Rotation matrix}
  \item{qfac}{qfac nifti field. It is pixdim[1]. If NULL, R is transformed to have determinant 1}
}
\value{The quaternion.}
\seealso{
  \code{\link{Q2R}}
}
\examples{
L <- f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
Q <- c(L$quatern.b,L$quatern.c,L$quatern.d)
R <- Q2R(Q,L$pixdim[1])
Q
R2Q(R)
}
\keyword{utilities}



