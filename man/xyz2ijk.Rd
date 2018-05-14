\name{xyz2ijk}
\alias{xyz2ijk}
\title{xyz2ijk}
\description{This function maps from some real world (x,y,z) positions
  in space into data coordinates (e.g. column i, row j, slice k). These
  original positions could relate to Talairach-Tournoux (T&T) space, MNI space, or patient-based scanner coordinates.}
\usage{xyz2ijk(xyz=c(1,1,1),method=2,L)}
\arguments{\item{xyz}{matrix. Each column of xyz should contain a voxel
    real world index coordinates (x,y,z) to be mapped to its (i,j,k) voxel index coordinates in the dataset}
  \item{method}{1 (qform.code=sform.code=0),2 (qform.code>0, rigid
    transformation) or 3 (sform.code>0, affine transformation).}
  \item{L}{header list of a NIFTI file}
}
\details{

  See help page of function ijk2xyz().

}
\value{A list containing the matrix xyz of the positions of the points
  specified in ijk.}
\seealso{
  \code{\link{ijk2xyz}}
  \code{\link{Q2R}}
  \code{\link{R2Q}}  
}
\examples{
L <- f.read.header(system.file("example-nifti.hdr",
package="AnalyzeFMRI"))
xyz <- matrix(c(1,1,1,2,3,7),byrow=FALSE,nrow=3)
xyz2ijk(xyz=xyz,method=2,L)
}
\keyword{utilities}



