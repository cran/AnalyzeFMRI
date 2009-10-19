\name{magicfield}
\alias{magicfield}
\title{Get magicfield from the header of an image file}
\description{Determine the type of a file : NIFIT .nii format, NIFTI .hdr/.img pair format, Analyze format.}
\usage{magicfield(file)}
\arguments{\item{file}{character, filename of an image (or header) file}}
\value{A list containing the magic and dim fields.}
\examples{
magicfield(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
}
\keyword{utilities}



