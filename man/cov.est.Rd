\name{cov.est}
\alias{cov.est}
\title{Estimates the covariance between neighbouring voxels}
\description{Estimates the covariance between neighbouring voxels using
   a specified neighbourhood system.
}
\usage{
cov.est(mat, mask, nmat)
}
\arguments{
  \item{mat}{3D array of voxel values.}
  \item{mask}{Array with sam dimension as mat that is 1/0 for voxels to
    be included/excluded.}
  \item{nmat}{Neighbourhood matrix.}
}
\value{The estimated covariance
}
\author{J. L. Marchini}

\examples{
ksize <- 9
d <- c(64, 64, 21)
FWHM <- 9
sigma <- diag(FWHM^2, 3) / (8 * log(2))
voxdim <- c(2, 2, 4)

filtermat <- GaussSmoothKernel(voxdim, ksize, sigma)

mask <- array(1, dim = d)
num.vox <- sum(mask)

mat <- Sim.3D.GRF(d = d, voxdim = voxdim, sigma = sigma, ksize = ksize, mask = mask, type = "field")$mat

nmat <- expand.grid(-1:1, -1:1, -1:1)
nmat4 <- nmat[c(11, 13, 15, 17), ]

cov <- cov.est(mat, mask, nmat4)
}
\keyword{utilities}
