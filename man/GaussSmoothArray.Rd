\name{GaussSmoothArray}
\alias{GaussSmoothArray}

\title{Spatially smooth an array with Gaussian kernel.}
\description{
  Applies a stationary Gaussian spatial smoothing kernel to a 3D or 4D array.
}
\usage{
GaussSmoothArray(x, voxdim=c(1, 1, 1), ksize=5, sigma=diag(3, 3), mask=NULL, var.norm=FALSE)
}
\arguments{
  \item{x}{The array to be smoothed.}
  \item{voxdim}{The dimensions of the \emph{vol}ume \emph{el}ements
    (voxel) that make up the array.}
  \item{ksize}{The dimensions (in number of voxels) of the 3D discrete
    smoothing kernel used to smooth the array.}
  \item{sigma}{The covariance matrix of the 3D Gaussian smoothing
    kernel. This matrix doesn't have to be non-singular; zero on the diagonal of sigma indicate no smoothing in that direction.
  }
  \item{mask}{A 3D 0-1 mask that delimits where the smoothing occurs.}
  \item{var.norm}{Logical flag indicating whether to normalize the variance of the smoothed array.}
}
\value{The smoothed array is returned.}
\author{J. L. Msrchini}

\seealso{\code{\link{GaussSmoothKernel}}}

\examples{
d <- c(10, 10, 10, 20)
mat <- array(rnorm(cumprod(d)[length(d)]), dim = d)
mat[, , 6:10, ] <- mat[, , 6:10, ] + 3
mask <- array(0, dim = d[1:3])
mask[3:8, 3:8, 3:8] <- 1
b <- GaussSmoothArray(mat, mask = mask, voxdim = c(1, 1, 1), ksize = 5, sigma = diag(1, 3))      
}
\keyword{utilities}
