\name{f.basic.hdr.nifti.list.create}
\alias{f.basic.hdr.nifti.list.create}
\title{creates basic .hdr list in NIFTI format}
\description{Creates a basic list that can be used to write a .hdr file
or the header part of a .nii file}
\usage{f.basic.hdr.nifti.list.create(dim.mat, file)
}

\arguments{
  \item{dim.mat}{\code{dim.mat} vector of the dimensions of the image
  array associated with the header file to be written}
  \item{file}{\code{file} Name of the .hdr file that will be contained
  in the file field of the header }
}
\value{Returns a list of all the fields needed to create a .hdr file
  (see the function code for details).}
\seealso{ 
  \code{\link{f.write.list.to.hdr.nifti}},
  \code{\link{f.nifti.file.summary}} 
}
\examples{
\dontrun{
dim.mat <- c(20,30,40,3)
file<-"temp.hdr"
f.basic.hdr.nifti.list.create(dim.mat, file)
}
}
\keyword{utilities}

