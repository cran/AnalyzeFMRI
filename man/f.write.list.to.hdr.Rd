\name{f.write.list.to.hdr}
\alias{f.write.list.to.hdr}
\title{writes a .hdr file}
\description{Writes a list of attributes to a .hdr file}
\usage{f.write.list.to.hdr(L,file)}
\arguments{\item{L}{A list of the all the fields included in a .hdr file}
  \item{file}{The name of the file to write, preferably with .hdr suffix}}
\value{Nothing is returned}
\seealso{
   \code{\link{f.basic.hdr.list.create}}    } 
 \examples{
a<-array(rnorm(20*30*40*3),dim=c(20,30,40,3))
file<-"temp.hdr"
b<-f.basic.hdr.list.create(a, file)
f.basic.hdr.list.create(b,file)}
\keyword{utilities}
