\name{magicfield}
\alias{magicfield}
\title{Get magicfield from the header of an image file}
\description{Determine the type of a file : NIFTI .nii format, NIFTI .hdr/.img pair format, ANALYZE format.}
\usage{magicfield(file, verbose = TRUE)}
\arguments{
\item{file}{character, filename of an image (or header) file}
\item{verbose}{\code{verbose} logical to display information in the console}
}
\value{A list containing the magic and dim fields.}
\examples{
magicfield(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
}
\keyword{utilities}



