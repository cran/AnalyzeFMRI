\name{f.write.array.to.img.8bit}
\alias{f.write.array.to.img.8bit}
\title{write array of 1 byte integers}
\description{Writes an array to a .img file of 1 byte integers}
\usage{f.write.array.to.img.8bit(mat,file,path.out=NULL)}
\arguments{\item{mat}{An array}
  \item{file}{The name of the file to be written, preferably with .img suffix}
    \item{path.out}{The path where to write the file (mandatory argument, even if the default value is NULL for backward compatibility)}
}
\value{Nothing is returned}
\seealso{
  \code{\link{f.write.analyze}},
  \code{\link{f.write.array.to.img.float}},
  \code{\link{f.write.array.to.img.2bytes}}  }
\keyword{utilities}
