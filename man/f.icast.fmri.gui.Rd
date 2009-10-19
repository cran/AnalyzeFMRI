\name{f.icast.fmri.gui}
\alias{f.icast.fmri.gui}
\title{tcltk GUI to apply Spatial or Temporal ICA to fMRI NIFTI datasets}
\description{The GUI provides a quick and easy to use interface for
  applying spatial or temporal ICA to fMRI NIFTI datasets. Computations
  WILL BE (NOT YET IMPLEMENTED) done in C for speed and low memory usage. 
}
\usage{
f.icast.fmri.gui()
}
\details{The user is required to enter the location of the fMRI dataset
  (stored in the NIFTI format) and (optionally) a mask for the
  dataset. If no mask is supplied then an option to create mask is
  available. TODO!!

Once completed, the user has the option of saving the results to an R
object or viewing the estimated components. The slices of each component
map are plotted sequentially in a grid followed by the components
associated time-course and that time-courses periodogram/power spectrum.TODO!!}
\value{
  \item{User named R object (optional)}{Once completed, the user has the
    option of saving the results to an R object named by the user.TODO!!}
}
\author{P Lafaye de Micheaux <plafaye@club.fr>}

\seealso{\code{\link{f.icast.fmri}},\code{\link{f.ica.fmri.gui}}}

\keyword{utilities}
