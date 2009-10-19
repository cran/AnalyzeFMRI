\name{fps2diminfo}
\alias{fps2diminfo}
\title{fps2diminfo}
\description{Encode freq.dim, phase.dim and slice.dim fields into the one byte dim.info field of a NIFTI header file.}
\usage{fps2diminfo(freq.dim,phase.dim,slice.dim)}
\arguments{\item{freq.dim}{freq.dim field of a NIFTI file}
  \item{phase.dim}{phase.dim field of a NIFTI file}
  \item{slice.dim}{slice.dim field of a NIFTI file}
}
\value{A list containing dim.info field.}
\seealso{
  \code{\link{diminfo2fps}}
}
\examples{
dim.info <- f.read.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))$dim.info
mylist <- diminfo2fps(dim.info)
fps2diminfo(mylist$freq.dim,mylist$phase.dim,mylist$slice.dim)
}
\keyword{utilities}



