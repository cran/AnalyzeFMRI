\name{f.read.analyze.ts}
\alias{f.read.analyze.ts}
\title{read in one voxel time series}
\description{Given a 4D ANALYZE .img/.hdr image pair this function can
  read in the time series from a specified position in 3D into a vector.}
\usage{f.read.analyze.ts(file, x, y, z)}
\arguments{
  \item{file}{The .img file}                 
  \item{x}{The x-coordinate}
  \item{y}{The y-coordinate}
  \item{z}{The z-coordinate}}
\details{Given a 4D ANALYZE .img/.hdr image pair this function can
  read in the time series from a specified position in 3D into a vector.}
\value{A vector containing the time series}
\seealso{
  \code{\link{f.read.analyze.slice}},
  \code{\link{f.read.analyze.slice.at.all.timepoints}},
  \code{\link{f.write.analyze}}, }
\examples{
f.read.analyze.ts(system.file("example.img", package="AnalyzeFMRI"),30,30,10)
}
\keyword{utilities}
