\name{N2G.Likelihood.Ratio}
\alias{N2G.Likelihood.Ratio}
\title{N2G Likelihood Ratio's
    }
\description{Calculates the ratio of the likelihood that data came from the positive Gamma distribution (activation) to the likelihood that data came from the other two distributions (Normal and negative Gamma)
}
\usage{
N2G.Likelihood.Ratio(data, par)
}
\arguments{
  \item{data}{The dataset (usually a vector)}
  \item{par}{The parameter vector for the N2G model}
}
\value{Returns the vector of likelihood ratio's
}
\author{J. L. Marchini}

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{N2G.Class.Probability}},   \code{\link{N2G.Spatial.Mixture}},
 \code{\link{N2G.Density}} ,         \code{\link{N2G.Likelihood}} ,   \code{\link{N2G.Transform}},
 \code{\link{N2G.Fit}}    ,         \code{\link{N2G}} ,          
 \code{\link{N2G.Inverse}}   ,         \code{\link{N2G.Region}}
}


\keyword{utilities}
