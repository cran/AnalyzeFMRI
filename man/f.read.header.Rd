\name{f.read.header}
\alias{f.read.header}
\title{read ANALYZE or NIFTI header file}
\description{Reads the ANALYZE or NIFTI image format .hdr (or .nii) header file into a list. The format type is determined by first reading the magic field.}
\usage{f.read.header(file)}
\arguments{\item{file}{The .hdr file to be read}}
\value{A list containing the information in the fields of the .hdr (.nii) file. See f.read.analyze.header of f.read.nifti.header to have the list of values.}
\seealso{
  \code{\link{f.read.analyze.header}}
  \code{\link{f.read.nifti.header}}
}
\examples{
f.read.header(system.file("example.hdr", package = "AnalyzeFMRI"))
f.read.header(system.file("example-nifti.hdr", package = "AnalyzeFMRI"))
}
\keyword{utilities}



