\name{reduction}
\alias{reduction}
\title{reduction}
\description{This function reduces the data in the row or col dimension.}
\usage{reduction(X, row.red = TRUE)}
\arguments{\item{X}{a matrix of size tm x vm which contains the functionnal images}
  \item{row.red}{Logical. Reduces the columns or the rows}
}
\value{
\item{Xred}{the reduced matrix}
}
\seealso{
  \code{\link{centering}}
}
\examples{
X <- matrix(rnorm(5 * 4), nrow = 5, ncol = 4)
Xred <- reduction(X, row.red = TRUE)$Xred
}
\keyword{utilities}



