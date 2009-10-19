\name{mat34.to.TZSR}
\alias{mat34.to.TZSR}
\title{Affine 4x4 (or 3x4) matrix to Translation, Scale, Shear and Rotation}
\description{Extract in that order Translation, Scale, Shear and Rotation from a 4x4
  (or 3x4) affine matrix from a NIFTI header list (srow.x, srow.y, srow.z).}
\usage{mat34.to.TZSR(M)}
\arguments{\item{M}{the affine matrix}
}
\value{A list containing Translation, Scale, Shear and Rotation.}
\seealso{
  \code{\link{R2Q}}
  \code{\link{Q2R}}
    \code{\link{mat34.to.TRSZ}}
}
\examples{
}
\keyword{utilities}



