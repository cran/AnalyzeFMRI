\name{ICAtemp}
\alias{ICAtemp}
\title{ICAtemp}
\description{This function performs a temporal ICA}
\usage{ICAtemp(X,n.comp,alg.typ="parallel",centering=TRUE,hp.filter=TRUE)}
\arguments{\item{X}{a matrix of size vm x tm which contains the functionnal images}
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
  \item{time.series}{estimated source matrix of size n.comp x tm}
  \item{spatial.components}{estimated mixing matrix of size vm x n.comp}
}
\seealso{
  \code{\link{ICAspat}}
}

\keyword{utilities}



