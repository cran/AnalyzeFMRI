\name{eigenvalues}
\alias{eigenvalues}
\title{eigenvalues}
\description{This function computes the eigenvalues of a centered and reduced data matrix}
\usage{eigenvalues(X, draw = FALSE)}
\arguments{\item{X}{a matrix of size tm x vm which contains the functionnal images centered and reduced}
  \item{draw}{Logical. Should we plot the eigenvalues}
}
\value{A list containing
\item{eigenvalues}{vector of the eigenvalues}
}
\examples{
X <- matrix(rnorm(5 * 4), nrow = 5, ncol = 4)
Xc <- centering(X, col.first = TRUE)$Xcentred
Xcr <- reduction(Xc, row.red = FALSE)$Xred
eigencr <- eigenvalues(Xcr, draw = FALSE)$eigenvalues
}
\keyword{utilities}



