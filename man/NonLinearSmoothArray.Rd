\name{NonLinearSmoothArray}
\alias{NonLinearSmoothArray}
\title{Non-linear spatial smmothing of 3D and 4D arrays.}
\description{Smooths the values in an array spatially using a weighting
  kernel that doesn't smooth across boundaries.}
\usage{
NonLinearSmoothArray(x, voxdim=c(1, 1, 1), radius=2, sm=3, mask=NULL)
}
\arguments{
  \item{x}{The array to be smoothed.}
  \item{voxdim}{The voxel dimensions of the array.}
  \item{radius}{The radius of the spatial smoothing}
  \item{sm}{The standard deviation of the Gaussian smoothing kernel.}
  \item{mask}{Optional mask for smoothing.}
}
\details{For a 3D array the smoothed values are obtained through a
  weighted sum of the surrounding voxel values within the specfied
  radius. The weights are calculated using a
  Gaussian kernel function applied to the differences between the voxel
  and its surrounding voxels. In this way the smoothing is anisotropic.

  For a 4D array the first 3 dimensions represent space and the fourth
  represents time. Therefore, each spatial location contains a time series of
  values. These time series are smoothed spatially in an anisotropic
  fashion. The sum of squared differences between each pair of time
  series are used to define the smoothing weights.
}
\value{The smoothed array is returned.}
\author{J. L. Marchini}

\seealso{\code{\link{GaussSmoothArray}}}

\examples{

#3D array
d<-rep(10,3)
a<-array(3,dim=d)
a[,5:10,5:10]<-7
a<-a+array(rnorm(n=1000,sd=1),dim=d)

h<-NonLinearSmoothArray(a,voxdim=c(1,1,1),radius=2,sm=3)

par(mfrow=c(2,2))
image(a[1,,],zlim=c(-1,12));title("Before smoothing")
image(h[1,,],zlim=c(-1,12));title("After smoothing")
persp(a[1,,],zlim=c(-1,12))
persp(h[1,,],zlim=c(-1,12))

#4D array
d<-c(10,10,10,20)
a<-array(1,dim=d)
a[,,6:10,]<-2
a<-a+array(rnorm(20000,sd=.1),dim=d)

h<-NonLinearSmoothArray(a,voxdim=c(1,1,1),radius=2,sm=3)

par(mfrow=c(2,2),mar=c(0,0,0,0))
for(i in 1:10){
    for(j in 10:1){
        plot(a[1,i,j,],type="l",ylim=c(0,3),axes=FALSE);box()
        lines(h[1,i,j,],col=2)
    }}

}
\keyword{utilities}
