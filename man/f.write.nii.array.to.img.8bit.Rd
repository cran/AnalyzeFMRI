\name{f.write.nii.array.to.img.8bit}
\alias{f.write.nii.array.to.img.8bit}
\title{write array of 1 byte integers and add at the begining of the file the NIFTI header part}
\description{Writes an array to a .img file of 1 byte integers and add at the begining of the file the NIFTI header part}
\usage{f.write.nii.array.to.img.8bit(mat,L,file,path.out)}
\arguments{\item{mat}{An array}
  \item{L}{A list containing the header information}
  \item{file}{The name of the file to be written, preferably with .img suffix}
        \item{path.out}{The path where to write the file}
}
\value{Nothing is returned}
\seealso{
  \code{\link{f.write.nifti}},
  \code{\link{f.write.nii.array.to.img.float}},
  \code{\link{f.write.nii.array.to.img.2bytes}}  }
\keyword{utilities}
