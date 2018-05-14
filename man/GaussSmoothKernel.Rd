\name{GaussSmoothKernel}
\alias{GaussSmoothKernel}
\alias{gaussfilter1}
\alias{gaussfilter2}

\title{Calculates a discrete Gaussian smoothing kernel.}
\description{Calculates a simple, discrete Gaussian smoothing kernel of
  a specfic size given the covariance matrix of the Gaussian.
}
\usage{
GaussSmoothKernel(voxdim=c(1, 1, 1), ksize=5, sigma=diag(3, 3))
}
\arguments{
  \item{voxdim}{Dimensions of each voxel.}
  \item{ksize}{Dimensions of the discrete kernel size.}
  \item{sigma}{The covariance matrix of the Gaussian kernel.}
}
\value{An array of dimension (ksize,ksize,ksize) containing the
  smoothing kernel.}
\author{J. L. Marchini}


\examples{
a <- GaussSmoothKernel(voxdim=c(1,1,1), ksize=5, sigma=diag(1,3))
}
\keyword{utilities}
