\name{f.ica.fmri.gui}
\alias{f.ica.fmri.gui}
\title{tcltk GUI to apply ICA to fMRI datasets}
\description{The GUI provides a quick and easy to use interface for
  applying spatial ICA to fMRI datasets. Computations are done in C for
  speed and low memory usage. 
}
\usage{
f.ica.fmri.gui()
}
\details{The user is required to enter the location of the fMRI dataset
  (stored in the ANALYZE format) and (optionally) a mask for the
  dataset. If no mask is supplied then an option to create mask is
  available. There is option to normalize the columns of the data matrix
  .

Once completed, the user has the option of saving the results to an R
object or viewing the estimated components. The slices of each component
map are plotted sequentially in a grid followed by the components
associated time-course and that time-courses periodogram/power spectrum.}
\value{
  \item{User named R object (optional)}{Once completed, the user has the
    option of saving the results to an R object named by the user.}
}
\author{J L Marchini <marchini@stats.ox.ac.uk> and C Heaton <chrisheaton99@yahoo.com>}

\seealso{\code{\link{f.ica.fmri}},\code{\link{f.plot.ica.fmri}}}

\keyword{utilities}