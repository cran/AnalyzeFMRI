\name{f.read.analyze.slice.at.all.timepoints}
\alias{f.read.analyze.slice.at.all.timepoints}

\title{reads a slice at all time points from a .img file}
\description{Reads in a slice of a .img file at all time points into an array}
\usage{f.read.analyze.slice.at.all.timepoints(file, slice)
}
\arguments{
  \item{file}{\code{file} The location of the .img file}
  \item{slice}{\code{slice} The number of the slice to be read in}
}
\value{An array containing the slice at all time points
}
\seealso{
  \code{\link{f.read.analyze.slice}},
  \code{\link{f.read.analyze.ts}},
  \code{\link{f.read.analyze.volume}}   

}
\examples{
a<-f.read.analyze.slice.at.all.timepoints(system.file("example.img", package="AnalyzeFMRI"),10)
dim(a)
}
\keyword{utilities}
