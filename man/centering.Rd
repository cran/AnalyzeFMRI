\name{centering}
\alias{centering}
\title{centering}
\description{This function center the data in the two dimensions, the first dimension being indicated by col.first argument}
\usage{centering(X,col.first=TRUE)}
\arguments{\item{X}{a matrix of size tm x vm which contains the functionnal images}
  \item{col.first}{Logical. Center the columns or the rows first}
}
\value{
\item{Xcentred}{the double centered matrix}
}
\seealso{
  \code{\link{reduction}}
}
\examples{
X <- matrix(rnorm(5 * 4), nrow = 5, ncol = 4)
Xcentred <- centering(X, col.first = TRUE)$Xcentred
}
\keyword{utilities}



