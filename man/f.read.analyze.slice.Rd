\name{f.read.analyze.slice}
\alias{f.read.analyze.slice}
\title{read one slice from a .img file}
\description{Reads in a specific slice from an ANALYZE .img image format
  file into an array.}
\usage{f.read.analyze.slice(file, slice, tpt)}
\arguments{
  \item{file}{The .img file to be read from}
  \item{slice}{The number of the slice (assumed to be the 3rd dimension)}
  \item{tpt}{The number of the scan that the slice is to be taken from}}
\details{The entire dataset is assumed to be 4D and a slice is extracted
  that is referenced by specifying the last two dimensions of the
  dataset i.e.slice and tpt.}
\value{An array containing the slice}
\seealso{
  \code{\link{f.read.analyze.slice.at.all.timepoints}},
  \code{\link{f.read.analyze.ts}},
  \code{\link{f.read.analyze.volume}}
 
}
\examples{a<-f.read.analyze.slice(paste(.lib.loc,"AnalyzeFMRI","example.img",sep=.Platform$file.sep),10,1)
dim(a)}
\keyword{utilities}
