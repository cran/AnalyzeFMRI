\name{f.write.list.to.hdr}
\alias{f.write.list.to.hdr}
\title{writes a .hdr file in ANALYZE format}
\description{Writes a list of attributes to a .hdr file}
\usage{f.write.list.to.hdr(L,file,path.out=NULL)}
\arguments{\item{L}{A list of all the fields included in a .hdr file}
  \item{file}{The name of the file to write, preferably with .hdr suffix}
      \item{path.out}{The path where to write the file (mandatory argument, even if the default value is NULL for backward compatibility)}
}
\value{Nothing is returned}
\seealso{
   \code{\link{f.basic.hdr.list.create}}    } 
 \examples{
a <- array(rnorm(20 * 30 * 40 * 3), dim = c(20, 30, 40, 3))
file <- "temp.hdr"
b <- f.basic.hdr.list.create(a, file)
tmpdir <- tempdir()
f.write.list.to.hdr(b, file, path.out = tmpdir)
unlink(tmpdir) # tidy up
}
\keyword{utilities}
