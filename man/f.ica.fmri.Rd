\name{f.ica.fmri}
\alias{f.ica.fmri}
\title{Applies Spatial ICA (Independent Component Analysis) to fMRI datasets}
\description{Decomposes an fMRI dataset into a specified number of
  Spatially Independent Components maps and associated time-courses using the FastICA algorithm}
\usage{
f.ica.fmri(file.name, n.comp, norm.col=T, fun="logcosh", maxit=1000,
alg.type="parallel", alpha=1, tol=1e-04, mask.file.name=NULL, slices=NULL)
}
\arguments{
  \item{file.name}{path to fMRI dataset (ANALYZE format .img file) }
  \item{n.comp}{number of components to extract}
  \item{norm.col}{a logical value indicating whether each voxel time
    series should be standardised to have zero mean and unit variance
    before the ICA algorithm is applied (default=T recommended in practice)}
  \item{fun}{the functional form of the G function used in the
    approximation to negentropy (see details)}
  \item{maxit}{maximum number of iterations to perform}
  \item{alg.type}{if \code{alg.typ=="deflation"} the components are
    extracted one at a time (the default).
    if \code{alg.typ=="parallel"} the components are extracted
    simultaneously.}
  \item{alpha}{constant in range [1,2] used in approximation to
    negentropy when \code{fun=="logcosh"}}
  \item{tol}{a positive scalar giving the tolerance at which the
    un-mixing matrix is considered to have converged.}
  \item{mask.file.name}{Optional path to file containing a 0/1 mask for
    the dataset}
  \item{slices}{Optional vector of slices to be included}
}
\details{The fMRI dataset is rearranged into a 2-dimensional data matrix
  X, where the column vectors are voxel time-series. A mask is used to
  specify which voxels are included. If this is not supplied by the user
  then a mask is constructed automatically using a 10\% intensity threshold.

  The data matrix is considered to be a linear combination of
  non-Gaussian (independent) components i.e. X = AS where rows of S
  contain the independent components and A is a linear mixing
  matrix. In short ICA attempts to 'un-mix' the data by estimating an
  un-mixing matrix U where UX = S.

  Under this generative model the measured 'signals' in X will tend to
  be 'more Gaussian' than the source components (in S) due to the Central
  Limit Theorem. Thus, in order to extract the independent
  components/sources we search for an un-mixing matrix U that
  maximizes the non-gaussianity of the sources.

  In FastICA, non-gaussianity is measured using approximations to
  negentropy (J) which are more robust than kurtosis based measures and fast
  to compute.

  The approximation takes the form

  \eqn{J(y)=[E{G(y)}-E{G(v)}]^2} where \eqn{v} is a N(0,1) r.v

  The following choices of G are included as options
  \eqn{G(u)=\frac{1}{\alpha} \log \cosh (\alpha u)} and \eqn{G(u)=-\exp(\frac{-u^2}{2})}

  The FastICA algorithm is used to 'un-mix' the data and recover
  estimates of the mixing matrix A and the source matrix S. Rows of the
  source matrix S represent spatially independent components of the
  dataset (these are arranged spatially in the output). Columns of A
  contain the associated time-courses of the independent components.

  Pre-processing involves removing the mean of each row of the data
  matrix and (optionally) standardizing the columns of the data matrix
  to have zero mean and unit variance.

  All computations are done using C code. This avoids reading the entire
  dataset into R and thus saves memory space.
  
}
\value{A list containing the following components
  
  \item{A}{estimated mixing matrix}
  \item{S}{estimated source matrix that has been rearranged spatially
    i.e. S is a 4-D array and S[,,,i] contains the 3-D map of the ith component}
}
\references{A. Hyvarinen and E. Oja (2000) Independent Component
  Analysis: Algorithms and Applications, Neural Networks,
  13(4-5):411-430
  
  Beckmann C. (2000) Independent Component Analysis for fMRI. First Year
  D.Phil Report, Dept. of Engineering Science, University of Oxford.}
\author{J L Marchini <marchini@stats.ox.ac.uk> and C Heaton <chrisheaton99@yahoo.com>}

\seealso{\code{\link{f.ica.fmri.gui}},\code{\link{f.plot.ica.fmri}} }

\keyword{utilities}
