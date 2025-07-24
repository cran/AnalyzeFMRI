\name{f.basic.hdr.list.create}
\alias{f.basic.hdr.list.create}
\title{creates basic .hdr list in ANALYZE format}
\description{Creates a basic list that can be used to write a .hdr file
}
\usage{f.basic.hdr.list.create(X, file.hdr)
}

\arguments{
  \item{X}{Array that is to be converted to a .img file}
  \item{file.hdr}{Name of the .hdr file that will be created }
}
\value{Returns a list of all the fields needed to create a .hdr file
  (see the functions code for details).}
\seealso{ 
  \code{\link{f.write.list.to.hdr}},
  \code{\link{f.analyze.file.summary}} 
}
\examples{
a <- array(rnorm(20 * 30 * 40 * 3), dim = c(20, 30, 40, 3))
file <- "temp.hdr"
hdr.list <- f.basic.hdr.list.create(a, file)
}
\keyword{utilities}

