\name{f.analyze.file.summary}
\alias{f.analyze.file.summary}
\title{prints summary of .img file contents}
\description{Prints a summary of the contents of an ANALYZE .img file using
  the associated .hdr header file.}
\usage{f.analyze.file.summary(file)}
\arguments{\item{file}{The location of .img file to be read}}
\value{A print out containing information about the .img file. This includes
    File name, Data Dimension, X dimension, Y dimension, Z dimension, Time dimension, Voxel dimensions, Data type}
\seealso{  \code{\link{f.read.analyze.header}},
  \code{\link{f.read.analyze.slice}},
  \code{\link{f.read.analyze.slice.at.all.timepoints}},
  \code{\link{f.read.analyze.ts}},
  \code{\link{f.write.analyze}},
  \code{\link{f.read.analyze.volume}},
  \code{\link{f.spectral.summary}},
  \code{\link{f.write.array.to.img.2bytes}},
  \code{\link{f.write.array.to.img.float}},
  \code{\link{f.write.list.to.hdr}},
   \code{\link{f.basic.hdr.list.create}}   }
 \examples{
f.analyze.file.summary(system.file("example.img", package="AnalyzeFMRI"))
}
\keyword{utilities}

