\name{f.read.nifti.ts}
\alias{f.read.nifti.ts}
\title{read in one voxel time series}
\description{Given a 4D NIFTI .img/.hdr image pair this function can
  read in the time series from a specified position in 3D into a vector.}
\usage{f.read.nifti.ts(file, x, y, z)}
\arguments{
  \item{file}{The .img file}                 
  \item{x}{The x-coordinate}
  \item{y}{The y-coordinate}
  \item{z}{The z-coordinate}}
\details{Given a 4D NIFTI .img/.hdr image pair this function can
  read in the time series from a specified position in 3D into a vector.}
\value{A vector containing the time series}
\seealso{
  \code{\link{f.read.nifti.slice}},
  \code{\link{f.read.nifti.slice.at.all.timepoints}},
  \code{\link{f.write.nifti}}, }
\examples{
f.read.nifti.ts(system.file("example-nifti.img", package="AnalyzeFMRI"),30,30,10)
}
\keyword{utilities}
