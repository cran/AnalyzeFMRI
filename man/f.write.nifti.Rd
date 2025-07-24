\name{f.write.nifti}
\alias{f.write.nifti}
\title{writes an array to a .img/.hdr pair in NIFTI format or to a .nii file}
\description{Creates a .img/.hdr pair of files or a .nii file from a given array}
\usage{f.write.nifti(mat,file,size,L,nii,path.out=NULL)}
\arguments{\item{mat}{An array}
  \item{file}{The name of the file to be written, without .img or .hdr
    suffix}
  \item{size}{Specify the format of the .img file. Either "float" (for 4
    byte floats) or "int" (2 byte integers) or "char" (1 byte
    integers).}
  \item{L}{if NULL, the list is created by the function, else it should
    be provided. This list contains the header part of a NIFTI image.}
    \item{nii}{should we write only one .nii file or a .hdr/.img pair of
    files}
          \item{path.out}{The path where to write the file (mandatory argument, even if the default value is NULL for backward compatibility)}
}

\value{Nothing is returned}
\seealso{
  \code{\link{f.write.array.to.img.8bit}},
  \code{\link{f.write.array.to.img.2bytes}},
  \code{\link{f.write.array.to.img.float}}    
  \code{\link{f.write.nii.array.to.img.8bit}},
  \code{\link{f.write.nii.array.to.img.2bytes}},
  \code{\link{f.write.nii.array.to.img.float}}}
\examples{
a<-array(rnorm(20*30*40*3),dim=c(20,30,40,3))
file<-"temp"
tmpdir <- tempdir()
f.write.nifti(a,file,size="float",nii=TRUE, path.out = tmpdir)
unlink(tmpdir) # tidy up
}
\keyword{utilities}
