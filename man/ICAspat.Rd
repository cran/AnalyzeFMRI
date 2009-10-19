\name{ICAspat}
\alias{ICAspat}
\title{ICAspat}
\description{This function performs a spatial ICA}
\usage{ICAspat(X,n.comp,alg.typ="parallel",centering=TRUE,hp.filter=TRUE)}
\arguments{\item{X}{a matrix of size tm x vm which contains the functionnal images}
  \item{n.comp}{number of maximally independent components to extract}
  \item{alg.typ}{if 'alg.typ == "parallel"' the components are extracted
          simultaneously (the default). if 'alg.typ == "deflation"' the
          components are extracted one at a time.}
  \item{centering}{Logical. Should we center the data first. Centering
    will be performed by firstly removing the column mean.}
  \item{hp.filter}{Logical. Should we perform high-pass filtering on the
  data}
}
\value{A list containing
  \item{time.series}{estimated mixing matrix of size tm x n.comp}
  \item{spatial.components}{estimated source matrix of size n.comp x vm}
}
\seealso{
  \code{\link{ICAtemp}}
}
\examples{
# TODO!!
}
\keyword{utilities}



