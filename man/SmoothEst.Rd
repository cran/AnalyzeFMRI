\name{SmoothEst}
\alias{SmoothEst}
\title{Estimate the variance-covariance matrix of a Gaussian random field}
\description{Estimate the variance-covariance matrix of a Gaussian random field
}
\usage{
SmoothEst(mat, mask, voxdim, method = "Forman")
}
\arguments{
  \item{mat}{3D array that is the Gaussian Random Field.}
  \item{mask}{3D mask array.}
  \item{voxdim}{Vector of length 3 containing the voxel dimensions.}
  \item{method}{The estimator to use. method = "Forman" (the default) uses the
    estimator proposed in [1]. method = "Friston" uses the estimator
    proposed in [2, 3], but tis can be biased when the amount of smoothing
    is small compared to the size of each voxel (see [1] for more
    details and example below)}
}
\details{Calculates the varaince-covariance matrix using the variance
  covariance matrix of partial derivatives.
}
\value{A (3x3) diagonal matrix.
}
\author{J. L. Marchini}

\references{[1] Stephen D. Forman et al. (1995) Improved
  assessment of significant activation in functional magnetic resonance
  imaging (fMRI): Use of a cluster-size threshold. Magnetic Resonance in Medicine, 33:636-647.

  [2] Karl J. Friston et al. (1991) Comparing functional (PET) images: the
  assessment of significant change. J. Cereb. Blood Flow
  Metab. 11:690-699. 

  [3] Stefan J. Kiebel et al. (1999) Robust smoothness estimation in
  statistical parametric maps using standardized residuals from the
  general linear model. NeuroImage, 10:756-766.
}


\examples{

###############
## EXAMPLE 1 ##
###############
## example that illustrates the bias of the Friston
## method when smoothing is small compared to voxel size
## NB. The presence of bias becomes clearer if the
##     simulations below are run about 100 times and
##     the results averaged

ksize <- 13
d <- c(64, 64, 64)
voxdim <- c(1, 1, 1)
FWHM <- 2 ## using a small value of FWHM (=2) compared to voxel size (=1)
sigma <- diag(FWHM^2, 3) / (8 * log(2))
mask <- array(1, dim = d)
num.vox <- sum(mask)

grf <- Sim.3D.GRF(d = d, voxdim = voxdim, sigma = sigma,
                  ksize = ksize, mask = mask, type = "field")$mat
               
sigma
SmoothEst(grf, mask, voxdim, method = "Friston")
SmoothEst(grf, mask, voxdim, method = "Forman") ## compared to sigma
##the Forman estimator is better (on average) than the Friston estimator

###############
## EXAMPLE 2 ##
###############
## increasing the amount of smoothing decreases the bias of the Friston estimator

ksize <- 13
d <- c(64, 64, 64)
voxdim <- c(1, 1, 1)
FWHM <- 5 ## using a large value of FWHM (=5) compared to voxel size (=1)
sigma <- diag(FWHM^2, 3) / (8 * log(2))
mask <- array(1, dim = d)
num.vox <- sum(mask)

grf <- Sim.3D.GRF(d = d, voxdim = voxdim, sigma = sigma,
                  ksize = ksize, mask = mask, type = "field")$mat

SmoothEst(grf, mask, voxdim, method = "Friston")
SmoothEst(grf, mask, voxdim, method = "Forman")                
sigma

}
\keyword{utilities}
