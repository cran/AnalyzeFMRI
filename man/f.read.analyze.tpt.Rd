\name{f.read.analyze.tpt}
\alias{f.read.analyze.tpt}
\title{Read in a volume at one time point}
\description{Given a 4D ANALYZE .img/.hdr image pair this function can
  read in the 3D volume of measurements at a specific time point.}
\usage{f.read.analyze.tpt(file, tpt)}
\arguments{
  \item{file}{The .img file.} 
  \item{tpt}{The time point to read in.}}
\details{Given a 4D ANALYZE .img/.hdr image pair this function can
  read in the 3D volume of measurements at a specific time point.}
\value{A 3D array containing the volume.}\
seealso{
  \code{\link{f.read.analyze.slice}},
  \code{\link{f.read.analyze.slice.at.all.timepoints}},
  \code{\link{f.write.analyze}}, }
\examples{
f.read.analyze.tpt(system.file("example.img", package="AnalyzeFMRI"),1)
}
\keyword{utilities}
