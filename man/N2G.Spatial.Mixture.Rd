\name{N2G.Spatial.Mixture}
\alias{N2G.Spatial.Mixture}
\title{fMRI Spatial Mixture Modelling}
\description{Fits the spatial mixture model of Hartvig and Jensen (2000)
}
\usage{
N2G.Spatial.Mixture(data, par.start = c(4, 2, 4, 2, 0.9, 0.05), ksize, ktype = c("2D", "3D"), mask = NULL)
}
\arguments{
  \item{data}{The dataset (usually a vector)}
  \item{par.start}{Starting values for N2G model}
  \item{ksize}{Kernel size (see paper)}
  \item{ktype}{Format of kernel "2D" or "3D"}
  \item{mask}{Mask for dataset.}
}

\value{p.map = a1, par = fit$par, lims = fit$lims
  Returns a list with following components
  \item{p.map}{Posterior Probability Map of activation}
  \item{par}{Fitted parameters of the underlying N2G model}
  \item{lims}{Normal component interval for fitted model}
}
\references{Hartvig and Jensen (2000) Spatial Mixture Modelling of fMRI Data}
\author{J. L. Marchini}

\seealso{\code{\link{N2G.Class.Probability}},  \code{\link{N2G.Likelihood.Ratio}}, 
 \code{\link{N2G.Density}} ,         \code{\link{N2G.Likelihood}} ,   \code{\link{N2G.Transform}},
 \code{\link{N2G.Fit}}    ,         \code{\link{N2G}} ,          
 \code{\link{N2G.Inverse}}   ,         \code{\link{N2G.Region}}
}
\examples{

## simulate image
d <- c(100, 100, 1)
y <- array(0, dim = d)
m <- y
m[, , ] <- 1

z.init <- 2 * m
z.init[20:40, 20:40, 1] <- 1
z.init[50:70, 50:70, 1] <- 3

y[z.init == 1] <- -rgamma(sum(z.init == 1), 4, 1)
y[z.init == 2] <- rnorm(sum(z.init == 2))
y[z.init == 3] <- rgamma(sum(z.init == 3), 4, 1)

mask <- 1 * (y < 1000)

## fit spatial mixture model
ans <- N2G.Spatial.Mixture(y, par.start = c(4, 2, 4, 2, 0.9, 0.05), ksize = 3, ktype = "2D", mask = m) 

## plot original image, standard mixture model estimate and spatial mixture
## model estimate

par(mfrow = c(1, 3))
image(y[, , 1])
image(y[, , 1] > ans$lims[1]) ## this line plots the results of a Non-Spatial Mixture Model
image(ans$p.map[, , 1] > 0.5) ## this line plots the results of the Spatial Mixture Model


}
\keyword{utilities}
