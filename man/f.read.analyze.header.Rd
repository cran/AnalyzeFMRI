\name{f.read.analyze.header}
\alias{f.read.analyze.header}
\title{read Analyze header file}
\description{Reads the ANALYZE image format .hdr header file into a
  list.}
\usage{f.read.analyze.header(file)}
\arguments{\item{file}{The .hdr file to be read}}
\value{A list containing the information in the fields of the .hdr
  file. Only the ones considered useful are included. See comments
  included in the function definition if you want to extend the amount
  of information extracted and stored.
\item{file.name}{path name of the .img file}
\item{swap}{TRUE or FALSE variable indicating whether files are big or little endian}
\item{sizeof.hdr}{Must indicate the byte size of the header file}
\item{data.type}{character vector indicating data storage type for each voxel}
\item{db.name}{}
\item{extents}{Should be 16384, the image file is created as contiguous with a minimum extent size}
\item{session.error}{}
\item{regular}{Must be `r' to indicate that all images and volumes are the same size}
\item{hkey.un0}{}
\item{dim}{vector of the image dimensions: dim[1] Number of dimensions in
  database, usually 4; dim[2] Image X dimension; number of pixels in an image row; dim[3] Image Y dimension; number of pixel rows in
  slice; dim[4] Volume Z dimension, number of slices in a volume; dim[5] Time points, number of volumes in database}
\item{vox.units}{specifies the spatial units of measure for a voxel}
\item{cal.units}{specifies the name of the calibration unit i.e. pixel,voxel}
\item{unused1}{}
\item{datatype}{integer indicator of data storage type for this image}
\item{bitpix}{number of bits per pixel: 1, 8, 16, 32, or 64}
\item{dim.un0}{unused}
\item{pixdim}{Parallel vector to dim, giving real world measurements in
  mm. and ms. pixdim[1]: voxel width in mm. pixdim[2]: voxel height in
  mm. pixdim[3]: slice thickness in mm.}
\item{vox.offset}{byte offset in the .img file at which voxels
  start. This value can be negative to specify that the absolute value is applied for every image in the file}
\item{scale}{specify the range of calibration values. SPM extends the Analyze format by using a scaling factor for the image from the header}
\item{funused2}{}
\item{funused3}{}
\item{cal.max}{Max display intensity}
\item{cal.min}{Min display intensity}
\item{compressed}{}
\item{verified}{}
\item{glmax}{The maximum pixel values for the entire database}
\item{glmin}{The minimum pixel values for the entire database}
\item{descrip}{any text you like}
\item{aux.file}{auxiliary filename}
\item{orient}{slice orientation for this dataset: 0 transverse
  unflipped; 1 coronal unflipped; 2 sagittal unflipped; 3 transverse
  flipped; 4 coronal flipped; 5 sagittal flipped}
\item{originator}{image central voxel coordinates. SPM uses this Analyze header field in an unorthodox way}
\item{generated}{}
\item{scannum}{}
\item{patient.id}{}
\item{exp.date}{}
\item{exp.time}{}
\item{hist.un0}{}
\item{views}{}
\item{vols.added}{}
\item{start.field}{}
\item{field.skip}{}
\item{omax}{}
\item{omin}{}
\item{smax}{}
\item{smin}{}}

\seealso{
  \code{\link{f.analyze.file.summary}}}
\examples{
f.read.analyze.header(system.file("example.hdr", package="AnalyzeFMRI"))
}
\keyword{utilities}



