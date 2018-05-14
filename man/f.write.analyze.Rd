\name{f.write.analyze}
\alias{f.write.analyze}
\alias{write2byteappend_JM}
\alias{write8bit_JM}
\alias{write8bitappend_JM}
\alias{write_analyze_header_wrap_JM}
\alias{write_nifti_header_wrap_JM}
\alias{writefloat_JM}
\alias{writefloatappend_JM}
\title{writes an array to a .img/.hdr pair in ANALYZE format}
\description{Creates a .img and .hdr pair of files from a given array}
\usage{f.write.analyze(mat,file,size,pixdim,vox.units,cal.units,originator)}
\arguments{\item{mat}{An array}
  \item{file}{The name of the file to be written, without .img or .hdr
    suffix}
  \item{size}{Specify the format of the .img file. Either "float" (for 4
    byte floats) or "int" (2 byte integers) or "char" (1 byte integers).}
  \item{pixdim}{A vector of length 3 specifying the voxel dimensions in
    mm}
  \item{vox.units}{String specifying the spatial units of measure for a
    voxel}
  \item{cal.units}{String specifying the name of calibration unit}
  \item{originator}{vector of length 5, only the three first values are
    used. Put the last two equal to zero}
}

\value{Nothing is returned}
\seealso{
  \code{\link{f.write.array.to.img.8bit}},
  \code{\link{f.write.array.to.img.2bytes}},
  \code{\link{f.write.array.to.img.float}} }    
\examples{
a<-array(rnorm(20*30*40*3),dim=c(20,30,40,3))
file<-"temp"
f.write.analyze(a,file,size="float")
f.analyze.file.summary("temp.img")
}
\keyword{utilities}
