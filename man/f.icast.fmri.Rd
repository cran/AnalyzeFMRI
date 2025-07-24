\name{f.icast.fmri}
\alias{f.icast.fmri}
\title{Applies Spatial or Temporal ICA (Independent Component Analysis)
  to fMRI NIFTI datasets}
\description{Decomposes an fMRI dataset into a specified number of
  Spatially or Temporally Independent Components maps and associated time-courses using the FastICA algorithm}
\usage{
f.icast.fmri(foncfile, maskfile, is.spatial, n.comp.compute = TRUE,
             n.comp = 0, hp.filter = TRUE, verbose = TRUE, path.out = "")
}
\arguments{
  \item{foncfile}{path and filename to fMRI dataset (NIFTI format .img
    or .nii file) }
  \item{maskfile}{path and filename to fMRI maskfile (0 and 1 values to
    determine if you are inside or outside the brain) dataset (NIFTI format .img
    or .nii file)}
  \item{is.spatial}{Logical. Should we perform a spatial or temporal
    ICA.}
  \item{n.comp.compute}{Logical. Should we estimate the number of
    components to exatract. If FALSE, n.comp value (>0) should be provided}
  \item{n.comp}{number of components to extract}
\item{hp.filter}{Logical. Should we perform high-pass filtering on the data}
  \item{verbose}{\code{verbose} logical to display information on the progress of computations}  
      \item{path.out}{The path where to write the file}
}
\details{TODO!!!
  The fMRI dataset is rearranged into a 2-dimensional data matrix
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
\value{Nothing for the moment ... TODO!!
The spatial and temporal components are written on disk
}
\references{A. Hyvarinen and E. Oja (2000) Independent Component
  Analysis: Algorithms and Applications, Neural Networks,
  13(4-5):411-430
  
  Beckmann C. (2000) Independent Component Analysis for fMRI. First Year
  D.Phil Report, Dept. of Engineering Science, University of Oxford.}
\author{P Lafaye de Micheaux <plafaye@club.fr>}

\seealso{\code{\link{f.icast.fmri.gui}} }

\keyword{utilities}

