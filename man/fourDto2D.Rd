\name{fourDto2D}
\alias{fourDto2D}
\title{fourDto2D}
\description{This function transforms a 4D image array into a 2D image matrix by unrolling space. 
This is usefull to perform a subsequent ICA.}
\usage{fourDto2D(volume.4d, tm)}
\arguments{\item{volume.4d}{a 4D array to be transformed}
  \item{tm}{number of time dimensions}
}
\value{
\item{x.2d}{matrix of size tm x vm which contains the tm images}
}
\seealso{
  \code{\link{threeDto4D}}
  \code{\link{twoDto4D}}
}
\examples{


}
\keyword{utilities}



