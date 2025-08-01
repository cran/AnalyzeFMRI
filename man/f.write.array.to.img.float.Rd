\name{f.write.array.to.img.float}
\alias{f.write.array.to.img.float}
\title{write array of 4 byte floats}
\description{Writes an array to a .img file of 4 byte floats}
\usage{f.write.array.to.img.float(mat,file,path.out=NULL)}
\arguments{\item{mat}{An array}
  \item{file}{The name of the file to be written, preferably with .img
    suffix}
        \item{path.out}{The path where to write the file (mandatory argument, even if the default value is NULL for backward compatibility)}
}
 \seealso{
  \code{\link{f.write.analyze}},
  \code{\link{f.write.array.to.img.2bytes}} ,
  \code{\link{f.write.array.to.img.8bit}}   }
\value{Nothing is returned}
\keyword{utilities}
