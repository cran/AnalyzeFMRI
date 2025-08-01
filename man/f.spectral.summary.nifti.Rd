\name{f.spectral.summary.nifti}
\alias{f.spectral.summary.nifti}
\title{plots graphical summary of spectral properties of an fMRI dataset}
\description{For a NIFTI .img file the periodogram of the time series are divided by a flat spectral estimate using the median periodogram ordinate. The resulting values are then combined within each Fourier frequency and quantiles are plotted against frequency. This provides a fast look at a fMRI dataset to identify any artifacts that reside at single frequencies.
}
\usage{f.spectral.summary.nifti(file, mask.file, ret.flag = FALSE, verbose = TRUE)}

\arguments{
  \item{file}{\code{file} The location of .img file  }
  \item{mask.file}{\code{mask.file} Optional location of a .img file
    containing a mask. If not given then one is created.}
  \item{ret.flag}{\code{ret.flag} flag specifying whether to return the
    array of quantiles at each frequency}
  \item{verbose}{\code{verbose} logical to display information on the progress of computations}  
}
\value{If \code{ret.flag = TRUE} the an array of quantiles at each
  frequency is returned
}
\seealso{
  \code{\link{f.nifti.file.summary}}}     
\keyword{utilities}
