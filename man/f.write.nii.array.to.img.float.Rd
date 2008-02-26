\name{f.write.nii.array.to.img.float}
\alias{f.write.nii.array.to.img.float}
\title{write array of 4 byte floats and add at the begining of the file the NIFTI header part}
\description{Writes an array to a .img file of 4 byte floats and add at the begining of the file the NIFTI header part}
\usage{f.write.nii.array.to.img.float(mat,L,file)}
\arguments{\item{mat}{An array}
  \item{L}{A list containing the header information}
  \item{file}{The name of the file to be written, preferably with .img
    suffix}}
 \seealso{
  \code{\link{f.write.nifti}},
  \code{\link{f.write.nii.array.to.img.2bytes}} ,
  \code{\link{f.write.nii.array.to.img.8bit}}   }
\value{Nothing is returned}
\keyword{utilities}
