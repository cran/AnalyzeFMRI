
\name{N2G.Fit}
\alias{N2G.Fit}

\title{Optimzation function for N2G model}
\description{Function that carries out the likelihood optimzation for
  the N2G model.
}
\usage{
N2G.Fit(data, par.start, maxit, method)
}
\arguments{
  \item{data}{The dataset (usually a vector)}
  \item{par.start}{Starting values for the parameters}
  \item{maxit}{Maximum number of iterations}
  \item{method}{Optimzation method (passed to optim)}
}
\details{Numerical optimization of the N2G model likelihood.
}
\value{Returns the optimized model parameters.
}
\author{J. L. Marchini}

\seealso{\code{\link{N2G.Class.Probability}},  \code{\link{N2G.Likelihood.Ratio}},  \code{\link{N2G.Spatial.Mixture}},
         \code{\link{N2G.Likelihood}} ,   \code{\link{N2G.Transform}},
	 \code{\link{N2G.Density}}    ,         \code{\link{N2G}} ,          
 \code{\link{N2G.Inverse}}   ,         \code{\link{N2G.Region}}
}


\keyword{utilities}
