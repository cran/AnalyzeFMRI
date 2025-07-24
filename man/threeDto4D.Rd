\name{threeDto4D}
\alias{threeDto4D}
\title{threeDto4D}
\description{To read tm functionnal images files in ANALYZE or NIFTI format,
  and concatenate them to obtain one 4D image file in Analyze (hdr/img
  pair) or Nifti format (hdr/img pair or single nii) which is written on
  disk. Note that this function outputs the files in the format sent
  in. If desired, one can use the function \code{analyze2nifti} to
  create NIFTI files from ANALYZE files.}
\usage{threeDto4D(outputfile, path.in = NULL, prefix = NULL, regexp = NULL, times = NULL,
       list.of.in.files = NULL, path.out = NULL, is.nii.pair = FALSE, hdr.number = 1)}
\arguments{\item{outputfile}{character. Name of the outputfile without extension}
  \item{path.in}{character with the path to the directory containing the image files}
  \item{prefix}{character. Common prefix to each file}
  \item{regexp}{character. Regular expression to get all the files, e.g., "????.img"}
  \item{times}{vector. Numbers (indices) of the image files to retrieve}
  \item{list.of.in.files}{names of img files to concatenate (with full path)}
  \item{path.out}{where to write the output hdr/img pair files. Will be taken as path.in if not provided.}
  \item{is.nii.pair}{logical. Should we write a single nii NIFTI file or a hdr/img NIFTI pair file}
  \item{hdr.number}{Number of the original 3D Analyze or NIFTI image file from
    which to take the header that should serve as the final header of the newly 4D created image file}
}
\value{None. Writes an Analyze or a NIFTI file on the disk.}
\seealso{
  \code{\link{twoDto4D}}
  \code{\link{fourDto2D}}
}
\examples{
a <- f.read.analyze.volume(system.file("example.img", package = "AnalyzeFMRI"))
dim(a)
tmpdir <- tempdir()
b <- threeDto4D(outputfile = "fourdfile", path.in =
          paste0(system.file(package = "AnalyzeFMRI"), "/"), 
      prefix = NULL, regexp = NULL, times = 2, list.of.in.files = rep("example.img", 2), 
      path.out = tmpdir, is.nii.pair = FALSE, hdr.number = 1)
dim(b)
unlink(tmpdir) # tidy up
}
\keyword{utilities}



