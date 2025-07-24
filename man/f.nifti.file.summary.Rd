\name{f.nifti.file.summary}
\alias{f.nifti.file.summary}
\title{prints summary of .img file contents}
\description{Prints a summary of the contents of a NIFTI .img file using
  the associated .hdr header file.}
\usage{f.nifti.file.summary(file)}
\arguments{\item{file}{The location of .img file to be read}}
\value{A print out containing information about the .img file. This includes
    File name, Data Dimension, X dimension, Y dimension, Z dimension, Time dimension, Voxel dimensions, Data type}

\seealso{  \code{\link{f.read.nifti.header}},
  \code{\link{f.read.nifti.slice}},
  \code{\link{f.read.nifti.slice.at.all.timepoints}},
  \code{\link{f.read.nifti.ts}},
  \code{\link{f.write.nifti}},
  \code{\link{f.read.nifti.volume}},
  \code{\link{f.spectral.summary.nifti}},
  \code{\link{f.write.array.to.img.2bytes}},
  \code{\link{f.write.array.to.img.float}},
  \code{\link{f.write.list.to.hdr.nifti}},
   \code{\link{f.basic.hdr.nifti.list.create}}   }
 \examples{
f.nifti.file.summary(system.file("example-nifti.img", package = "AnalyzeFMRI"))
}
\keyword{utilities}

