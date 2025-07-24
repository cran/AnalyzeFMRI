\name{f.plot.ica.fmri}
\alias{f.plot.ica.fmri}
\title{Plots a specified component from the output of f.ica.fmri}
\description{Plots a specified component from the output of f.ica.fmri
}
\usage{
f.plot.ica.fmri(obj.ica, comp, cols)
}
\arguments{
  \item{obj.ica}{R object returned by the function f.ica.fmri}
  \item{comp}{number of the component to plot}
  \item{cols}{optional vector of colours to use for plotting}
}
\details{The slices of the specified component map are plotted
  sequentially in a grid followed by the components associated
  time-course and that time-courses periodogram/power spectrum
}
\value{No return value, just plot graphs}
\author{J L Marchini <marchini@stats.ox.ac.uk> and C Heaton <chrisheaton99@yahoo.com>}

\seealso{\code{\link{f.ica.fmri}},\code{\link{f.ica.fmri.gui}}}

\keyword{utilities}

