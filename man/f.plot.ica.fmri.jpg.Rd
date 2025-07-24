\name{f.plot.ica.fmri.jpg}
\alias{f.plot.ica.fmri.jpg}
\title{Plot the components of the ouput of f.ica.fmri to a series of
  jpeg files}
\description{This function allows the compact graphical storage of the
  output of a spatial ICA decomposition of an fMRI dataset. each
  component is plotted to a jpeg.
}
\usage{
f.plot.ica.fmri.jpg(ica.obj, file="", cols=heat.colors(100), width=700, height=700)
}
\arguments{
  \item{ica.obj}{Object that is the output of f.ica.fmri}
  \item{file}{The component i will be plotted in file 'file'.comp.i.jpeg}
  \item{cols}{Optional colour vector for plotting the components}
  \item{width}{Width of jpeg images}
  \item{height}{Height of jpeg images}
}
\value{No return value, just write in jpeg files}
\author{J L Marchini}


\seealso{\code{\link{f.ica.fmri}},
  \code{\link{jpeg}}
}

\keyword{utilities}



 





















