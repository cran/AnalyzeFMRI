\name{f.read.analyze.header}
\alias{f.read.analyze.header}
\title{read Analyze header file}
\description{Reads the ANALYZE image format .hdr header file into a
  list.}
\usage{f.read.analyze.header(file)}
\arguments{\item{file}{The .hdr file to be read}}
\value{A list containing the information in the fields of the .hdr
  file. Only the ones considered useful are included. See comments
  included in the function definition if you want to extend the amount
  of information extracted and stored.
\item{swap}{T or F variable indicating whether files are big or little endian}
\item{file.name}{path name of the .img file}
\item{dim}{vector of image dimensions. The first entry specifies the number
  of dimensions}
\item{vox.units}{specifies the spatial units of measure for a voxel}
\item{cal.units}{specifies the name of the calibration unit i.e. pixel,voxel}
\item{datatype}{integer indicator of data storage type for each voxel}
\item{data.type}{character vector indicating data storage type for each voxel}
\item{bit.pix}{the number of bits per voxel}
\item{pixdim}{parallel vector to dim giving real world voxel unit measurements}
\item{glmax}{maximum voxel value}
\item{glmin}{minimum voxel value}}
\seealso{
  \code{\link{f.analyze.file.summary}}}
\examples{f.read.analyze.header(paste(.lib.loc,"AnalyzeFMRI","example.hdr",sep=.Platform$file.sep))}
\keyword{utilities}



