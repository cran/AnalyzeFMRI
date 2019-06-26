\name{f.write.list.to.hdr.nifti}
\alias{f.write.list.to.hdr.nifti}
\title{writes a .hdr file in NITI format}
\description{Writes a list of attributes to a .hdr file}
\usage{f.write.list.to.hdr.nifti(L,file)}
\arguments{\item{L}{A list of the all the fields included in a .hdr file}
  \item{file}{The name of the file to write, preferably with .hdr suffix}}
\value{Nothing is returned}
\seealso{
   \code{\link{f.basic.hdr.nifti.list.create}}    } 
 \examples{
\dontrun{
a<-array(rnorm(20*30*40*3),dim=c(20,30,40,3))
file<-"temp.hdr"
b<-f.basic.hdr.nifti.list.create(dim(a), file)
f.write.list.to.hdr.nifti(b,file)}
}
\keyword{utilities}
