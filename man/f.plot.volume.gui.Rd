\name{f.plot.volume.gui}
\alias{f.plot.volume.gui}
\title{tcltk GUI to display FMRI or MRI images}
\description{tcltk GUI to display FMRI or MRI images. This GUI is very
  usefull, for example, for investigating the results of an ICA
  performed with f.icast.fmri.gui(). But it can also be used to display
  an MRI or an FMRI image}
\usage{f.plot.volume.gui(array.fonc=NULL,hdr.fonc=NULL)}
\arguments{
  \item{array.fonc}{An optionnal array containing the MRI values}
  \item{hdr.fonc}{If array.fonc is not NULL, one must provide a list
    'hdr.fonc' with a 'pixdim' field containing a vector of length 4
    with the pixel dimensions}
  }
\value{Nothing}
\details{
One has the possibility to enter either a filename (with its path)
or directly an R object in the file field.
This function will only work if package tkrplot is installed. It seems
this package is not available for arm64 Macs.
}
\author{P Lafaye de Micheaux <plafaye@club.fr>}

\seealso{
  \code{\link{f.icast.fmri.gui}}
}
\examples{
# TODO!!
}
\keyword{utilities}



