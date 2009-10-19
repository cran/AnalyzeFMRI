\name{twoDto4D}
\alias{twoDto4D}
\title{twoDto4D}
\description{This function transform a 2D matrix of size tm x vm containing images in each row into a 4D array image.}
\usage{twoDto4D(x.2d, dim)}
\arguments{\item{x.2d}{a 2D matrix to be transformed}
  \item{dim}{vector of length 4 containing the dimensions of the array. dim[1:3] are the space dimensions. dim[4] is the time dimension}
}
\value{
\item{volume.4d}{a 4D array image}
}
\seealso{
  \code{\link{threeDto4D}}
  \code{\link{fourDto2D}}
}
\examples{


}
\keyword{utilities}



