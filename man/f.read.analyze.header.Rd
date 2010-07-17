\name{f.read.analyze.header}
\alias{f.read.analyze.header}
\title{read Analyze header file}
\description{Reads the ANALYZE image format .hdr header file into a
  list.}
\usage{f.read.analyze.header(file)}
\arguments{\item{file}{The .hdr file to be read}}
\value{A list containing the information in the fields of the .hdr
  file. 
\item{file.name}{name of the .img file}
\item{swap}{TRUE or FALSE variable indicating whether files are big or little endian}
\item{....... HEADER KEY ..........}{}
\item{sizeof.hdr}{This field implies that Analyze format was originally intended to be extensible, 
but in practice this did not happen, and instead the file size (and hence the value of this field) is 348. 
Software commonly tests the value in this field to detect whether the byte ordering is Big-Endian or Little-Endian.}
\item{data.type}{character vector indicating data storage type for each voxel}
\item{db.name}{database name}
\item{extents}{Should be 16384, the image file is created as contiguous with a minimum extent size}
\item{session.error}{}
\item{regular}{Must be `r' to indicate that all images and volumes are the same size}
\item{hkey.un0}{}
\item{....... IMAGE DIMENSION ..........}{}
\item{dim}{vector of the image dimensions: 
dim[1] Number of dimensions in database, usually 4; 
dim[2] Image X dimension (slice width), number of pixels in an image row; 
dim[3] Image Y dimension (slice height), number of pixel rows in slice; 
dim[4] Volume Z dimension (volume depth), number of slices in a volume; 
dim[5] Time points, number of volumes in database
dim[6] ??
dim[7] ??
dim[8] ??
}
\item{vox.units}{3 characters to specify the spatial units of measure for a voxel (mm., um., cm.)}
\item{cal.units}{7 characters to specify the name of the calibration unit i.e. pixel,voxel}
\item{unused1}{??}
\item{datatype}{integer indicator of data storage type for this image: 
0 (None or Unknown), 1 (Binary), 2 (Unsigned-char), 4 (Signed-short), 8 (Signed-int), 16 (float), 
32 (Complex), 64 (Double), 128 (RGB), 255 (All)}
\item{bitpix}{number of bits per pixel: 1 (packed binary, slices begin on byte boundaries), 
8 (unsigned char, gray scale), 16 (signed short), 32 (signed integers or float), or 24 (RGB, 8 bits per channel)s}
\item{dim.un0}{unused}
\item{pixdim}{Parallel vector to dim, giving real world measurements in mm. and ms. 
pixdim[1]: ??
pixdim[2]: voxel width in mm. 
pixdim[3]: voxel height in  mm. 
pixdim[4]: slice thickness (interslice distance) in mm.
pixdim[5]: timeslice in ms.
pixdim[6]: ??
pixdim[7]: ??
pixdim[8]: ??
}
\item{vox.offset}{byte offset in the .img file at which voxels
  start. This value can be negative to specify that the absolute value is applied for every image voxel in the file}
\item{funused1}{specify the range of calibration values. SPM extends the Analyze format by using a scaling factor for 
the image from the header}
\item{funused2}{SPM2 image intensity zero intercept}
\item{funused3}{??}
\item{cal.max}{Max display intensity, calibration value, values of 0.0 for both fields imply that no 
       calibration max and min values are used}
\item{cal.min}{Min display intensity, calibration value}
\item{compressed}{??}
\item{verified}{??}
\item{glmax}{The maximum pixel values for the entire database}
\item{glmin}{The minimum pixel values for the entire database}
\item{....... DATA HISTORY ..........}{}
\item{descrip}{any text you like}
\item{aux.file}{auxiliary filename}
\item{orient}{planar slice orientation for this dataset: 0 transverse
  unflipped; 1 coronal unflipped; 2 sagittal unflipped; 3 transverse
  flipped; 4 coronal flipped; 5 sagittal flipped}
\item{originator}{image central voxel coordinates. SPM uses this Analyze header field in an unorthodox way. 
originator[1]: SPM99 X near Anterior Commissure,
originator[2]: SPM99 Y near Anterior Commissure,
originator[3]: SPM99 Z near Anterior Commissure,
originator[4]:??, 
originator[5]:??}
\item{generated}{??}
\item{scannum}{??}
\item{patient.id}{??}
\item{exp.date}{??}
\item{exp.time}{??}
\item{hist.un0}{??}
\item{views}{??}
\item{vols.added}{??}
\item{start.field}{??}
\item{field.skip}{??}
\item{omax}{??}
\item{omin}{??}
\item{smax}{??}
\item{smin}{??}}

\seealso{
  \code{\link{f.analyze.file.summary}}}
\examples{
f.read.analyze.header(system.file("example.hdr", package="AnalyzeFMRI"))
}
\keyword{utilities}



