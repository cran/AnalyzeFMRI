\name{Sim.3D.GammaRF}
\alias{Sim.3D.GammaRF}
\title{Simulate Gamma distributed Random Field}
\description{Simulates a Gamma distributed random field by simulating a
  Gaussian Random Field and transforming it to be Gamma distributed.
}
\usage{
Sim.3D.GammaRF(d, voxdim, sigma, ksize, mask, shape, rate)
}
\arguments{
  \item{d}{A vector specifying the dimensions of a 3D or 4D array.}
  \item{voxdim}{The dimensions of each voxel.}
  \item{sigma}{The 3D covariance matrix of the field.}
  \item{ksize}{The size (in voxels) of the kernel with which to filter
    the independent field.}
  \item{mask}{A 3D mask for the field.}
  \item{shape}{The shape parameter of the Gamma distribution.}
  \item{rate}{The rate parameter of the Gamma distribution.}
}
\value{A 3D array containing the simulated field
}
\author{J. L. Marchini}
\examples{

d <- c(64, 64, 21)
FWHM <- 9
sigma <- diag(FWHM^2, 3) / (8 * log(2))
voxdim <- c(2, 2, 4)
m <- array(1, dim = d)

a <- Sim.3D.GammaRF(d = d, voxdim = voxdim, sigma = sigma, 
                    ksize = 9, mask = m, shape = 6, rate = 1)

}
\keyword{utilities}
