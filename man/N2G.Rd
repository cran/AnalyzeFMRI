\name{N2G}
\alias{N2G}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Fits the N2G model}
\description{Fits the N2G model (1 Normal and 2 Gamma's mixture model)
  to a dataset using Maximum Likelihhod.
}
\usage{
N2G(data, par.start = c(4, 2, 4, 2, 0.9, 0.05))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{data}{The dataset.}
  \item{par.start}{The starting values for the optimization to maximize
    the likelihood. The parameters of the model are ordered in the
    vector par.start in the following way (refer to the model below)

    c(a, b, c, d, p1, p2)}
}
\details{The mixture model considered is a mixture of a standard normal
  distribution and two Gamma functions. This model is denoted N2G.
  
  x ~ p1 * N(0, 1) + p2 * Gamma(a, b) + (1 - p1 - p2) * -Gamma(c, d)
}
\value{
  A list with components
  \item{par}{The fitted parameter values.}
  \item{lims}{The upper and lower thresholds for the Normal component of
  the fitted model}
}
\author{J. L. Marchini}

\seealso{\code{\link{N2G.Class.Probability}},  \code{\link{N2G.Likelihood.Ratio}},  \code{\link{N2G.Spatial.Mixture}},
 \code{\link{N2G.Density}} ,         \code{\link{N2G.Likelihood}} ,   \code{\link{N2G.Transform}},
 \code{\link{N2G.Fit}}    ,             
 \code{\link{N2G.Inverse}}   ,         \code{\link{N2G.Region}}
}
\examples{
par <- c(3, 2, 3, 2, .3, .4)
data <- c(rnorm(10000), rgamma(2000, 10, 1), -rgamma(1400, 10, 1))
hist(data, n = 100, freq = FALSE)

q <- N2G.Fit(data, par, maxit = 10000, method = "BFGS")
p <- seq(-50, 50, .1)
lines(p, N2G.Density(p, q), col = 2)
}
\keyword{utilities}
