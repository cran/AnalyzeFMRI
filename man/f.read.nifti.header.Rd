\name{f.read.nifti.header}
\alias{f.read.nifti.header}
\title{read Nifti header file}
\description{Reads the NIFTI image format .hdr header file into a
  list.}
\usage{f.read.nifti.header(file)}
\arguments{\item{file}{The .hdr file to be read}}
\value{A list containing the information in the fields of the .hdr
  file. Only the ones considered useful are included. See comments
  included in the function definition if you want to extend the amount
  of information extracted and stored.
\item{file.name}{path name of the .img file}
\item{swap}{TRUE or FALSE variable indicating whether files are big or little endian}
\item{sizeof.hdr}{MUST be 348}
\item{data.type}{UNUSED}
\item{db.name}{UNUSED}
\item{extents}{UNUSED}
\item{session.error}{UNUSED}
\item{regular}{UNUSED}
\item{dim.info}{MRI slice ordering}
\item{dim}{vector of image dimensions. The first entry specifies the number of dimensions}
\item{intent.p1}{1st intent parameter}
\item{intent.p2}{2nd intent parameter}
\item{intent.p3}{3rd intent parameter}
\item{intent.code}{NIFTI INTENT code}
\item{datatype}{integer indicator of data storage type for each voxel}
\item{bitpix}{the number of bits per voxel}
\item{slice.start}{First slice index}
\item{pixdim}{Grid spacings}
\item{vox.offset}{Offset into .nii file}
\item{scl.slope}{Data scaling: slope}
\item{scl.inter}{Data scaling: offset}
\item{slice.end}{Last slice index}
\item{slice.code}{Slice timing order}
\item{xyzt.units}{Units of pixdim[1..4]}
\item{cal.max}{Max display intensity}
\item{cal.min}{Min display intensity}
\item{slice.duration}{Time for 1 slice}
\item{toffset}{Time axis shift}
\item{glmax}{UNUSED}
\item{glmin}{UNUSED}
\item{descrip}{any text you like}
\item{aux.file}{auxiliary filename}
\item{qform.code}{NIFTI XFORM code}
\item{sform.code}{NIFTI XFORM code}
\item{quatern.b}{Quaternion b param}
\item{quatern.c}{Quaternion c param}
\item{quatern.d}{Quaternion d param}
\item{qoffset.x}{Quaternion x shift}
\item{qoffset.y}{Quaternion y shift}
\item{qoffset.z}{Quaternion z shift}
\item{srow.x}{1st row affine transform}
\item{srow.y}{2nd row affine transform}
\item{srow.z}{3rd row affine transform}
\item{intent.name}{'name' or meaning of data}
\item{magic}{MUST be "ni1" or "n+1"}}


\examples{
f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
}
\keyword{utilities}



