\name{f.read.nifti.tpt}
\alias{f.read.nifti.tpt}
\title{Read in a volume at one time point}
\description{Given a 4D NIFTI .img/.hdr image pair this function can
  read in the 3D volume of measurements at a specific time point.}
\usage{f.read.nifti.tpt(file, tpt)}
\arguments{
  \item{file}{The .img file.} 
  \item{tpt}{The time point to read in.}}
\details{Given a 4D NIFTI .img/.hdr image pair this function can
  read in the 3D volume of measurements at a specific time point.}
\value{A 3D array containing the volume.}
\seealso{
  \code{\link{f.read.nifti.slice}},
  \code{\link{f.read.nifti.slice.at.all.timepoints}},
  \code{\link{f.write.nifti}}, }
\examples{
f.read.nifti.tpt(system.file("example-nifti.img", package="AnalyzeFMRI"),1)
}
\keyword{utilities}
