\name{mat34.to.TZSR}
\alias{mat34.to.TZSR}
\title{Affine 4x4 (or 3x4) matrix to Translation, Scale, Shear and Rotation}
\description{Extract in that order Translation, Scale, Shear and Rotation from a 4x4
  (or 3x4) affine matrix from a NIFTI header list (srow.x, srow.y, srow.z).}
\usage{mat34.to.TZSR(M)}
\arguments{\item{M}{the affine matrix}
}
\value{A list containing Translation, Scale, Shear and Rotation. Rotation decomposition is also provided (rotation = RotZ*RotY*RotX*Ref 
where Ref is a Reflexion if the rotation is improper or is Identity if the rotation is proper).}
\details{
Decomposes M using the convention: M = translation * scale * skew * rotation. Be careful that rotation can be improper.
}
\seealso{
  \code{\link{R2Q}}
  \code{\link{Q2R}}
    \code{\link{mat34.to.TRSZ}}
}
\examples{
L <- f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
M <- rbind(L$srow.x,L$srow.y,L$srow.z)
mat34.to.TZSR(M)
}
\keyword{utilities}



