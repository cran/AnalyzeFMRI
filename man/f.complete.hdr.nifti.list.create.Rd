\name{f.complete.hdr.nifti.list.create}
\alias{f.complete.hdr.nifti.list.create}
\title{creates complete .hdr list in NIFTI format}
\description{Creates a complete list that can be used to write a .hdr file
or the header part of a .nii file}
\usage{f.complete.hdr.nifti.list.create(file,dim.info=character(1),dim,
intent.p1=single(1),intent.p2=single(1),intent.p3=single(1),intent.code=integer(1),
datatype=integer(1),bitpix=integer(1),slice.start=integer(1),pixdim=single(8),
scl.slope=single(1),scl.inter=single(1),slice.end=integer(1),slice.code=character(1),
xyzt.units=character(1),cal.max=single(1),cal.min=single(1),slice.duration=single(1),
toffset=single(1),descrip=paste(rep(" ", 80), sep = "", collapse = ""),
aux.file=paste(rep(" ", 24), sep = "", collapse =""),qform.code=integer(1),
sform.code=integer(1),quatern.b=single(1),quatern.c=single(1),quatern.d=single(1),
qoffset.x=single(1),qoffset.y=single(1),qoffset.z=single(1),srow.x=single(4),
srow.y=single(4),srow.z=single(4),
intent.name=paste(rep(" ", 16), sep = "", collapse = ""))
}

\arguments{
\item{file}{The .hdr filename. If file extension is ".nii", this will
  create a header file for a ".nii" NIFTI file, else for a .hdr/.img
  NIFTI pair}
\item{dim.info}{MRI slice ordering: This field encode which spatial
  dimension (1=\eqn{x}, 2=\eqn{y}, or 3=\eqn{z}) corresponds to which acquisition dimension
  for MRI data. In fact, it contains three informations: \code{freq.dim},
  \code{phase.dim} and \code{slice.dim}, all squished into the single byte field dim.info (2 bits each, since the values for each field are
  limited to the range 0..3). The R function \code{fps2diminfo} can be used to
  encode these values into the \code{dim.info} character byte.}
\item{dim}{vector (of length 8) of image dimensions. \code{dim[1]} specifies the
  number of dimensions. In NIFTI-1 files, \code{dim[2]}, \code{dim[3]}, \code{dim[4]} are for
  space, \code{dim[5]} is for time. The 5th dimension (\code{dim[6]}) of the dataset, if
  present (i.e., \code{dim[1]}=5 and \code{dim[6]} > 1), contains multiple values (for
  example a vector) to be stored at each spatio-temporal location. Uses
  of \code{dim[7]} and \code{dim[8]} are not specified in NIFTI-1 format.}
\item{intent.p1}{1st intent parameter: first auxiliary parameter for a
  possible statistical distribution specified in \code{intent.code}}
\item{intent.p2}{2nd intent parameter: second auxiliary parameter for a
  possible statistical distribution specified in \code{intent.code}}
\item{intent.p3}{3rd intent parameter: third auxiliary parameter for a
  possible statistical distribution specified in \code{intent.code}}
\item{intent.code}{NIFTI INTENT code: if 0, this is a raw dataset; if in
range 2...24, this indicates that the numbers in the dataset should be interpreted
   as being drawn from a given distribution. Most such distributions have
   auxiliary parameters (given with \code{intent.p}?); if in range 1001...1011,
 this is an other meaning. See file \file{intent-code.txt} in the niftidoc
 directory of the source package.  If the dataset DOES NOT have a 5th
 dimension (\code{dim[1]}=4), then the auxiliary parameters are the same for
 each voxel, and are given in header fields \code{intent.p1}, \code{intent.p2}, and
 \code{intent.p3}. If the dataset DOES have a 5th dimension (\code{dim[1]}=5), then the auxiliary parameters are different for each voxel.}
\item{datatype}{integer indicator of data storage type for each
  voxel. This could be 2 (unsigned char), 4 (signed short), 8 (signed
  int), 16 (32 bit float), 32 (64 bit complex = two 32 bit floats), 64 (64
  bit float = double), 128 (3 8 bit bytes), 256 (signed char), 512
  (unsigned short), 768 (unsigned int), 1024 (signed long long), 1280 (unsigned long long),
  1536 (128 bit float = long double), 1792 (128 bit complex = 2 64 bit
  floats), 2048 (256 bit complex = 2 128 bit floats).}
\item{bitpix}{the number of bits per voxel. This field MUST correspond with
   the datatype field.  The total number of bytes in the image data is
     \code{dim[2]}*  ... * \code{dim[dim[1]+1]} * \code{bitpix} / 8}
\item{slice.start}{Indicates the start of the slice acquisition pattern, when \code{slice.code} is nonzero.  These values
                    are present to allow for the possible addition of
                    "padded" slices at either end of the volume, which
                    don't fit into the slice timing pattern.  If there
                    are no padding slices, then \code{slice.start}=0 and
                    \code{slice.end=dim[slice.dim+1]-1} are the correct values.
                    For these values to be meaningful, slice.start must
                    be non-negative and slice.end must be greater than
                    slice.start.}
\item{pixdim}{vector (of length 8). Grid spacings. When reading a
  NIFTI-1 header, \code{pixdim[1]} stores \code{qfac} (which is either -1 or 1). If
  \code{pixdim[1]}=0 (which should not occur), we take \code{qfac}=1. \code{pixdim[2]},
  \code{pixdim[3]} and \code{pixdim[4]} give the voxel width along dimension \eqn{x}, \eqn{y} and
  \eqn{z} respectively. \code{pixdim[5]} gives the time step (=Time Repetition=TR). The units of
  \code{pixdim} can be specified with the \code{xyzt.units} field.}
\item{scl.slope}{Data scaling: If the \code{scl.slope} field is nonzero, then
  each voxel value in the dataset should be scaled as \eqn{y = \code{scl.slope}*x +
  \code{scl.inter}}, where \eqn{x} = voxel value stored and \eqn{y} = "true" voxel value}
\item{scl.inter}{Data scaling: offset. Idem above.}
\item{slice.end}{Indicates the end of the slice acquisition pattern, when \code{slice.code} is nonzero.  These values
                    are present to allow for the possible addition of
                    "padded" slices at either end of the volume, which
                    don't fit into the slice timing pattern.  If there
                    are no padding slices, then \code{slice.start}=0 and
                    \code{slice.end=dim[slice.dim+1]-1} are the correct values.
                    For these values to be meaningful, \code{slice.start} must
                    be non-negative and \code{slice.end} must be greater than
                    \code{slice.start}.}
\item{slice.code}{Slice timing order. If this is nonzero, AND if \code{slice.dim} is nonzero, AND
                 if \code{slice.duration} is positive, indicates the timing
                 pattern of the slice acquisition. The following codes
                 are defined: 0 (NIFTI SLICE UNKNOWN), 1 (NIFTI SLICE
		 SEQ INC), 2 (NIFTI SLICE SEQ DEC), 3 (NIFTI SLICE ALT
		 INC), 4 (NIFTI SLICE ALT DEC)}
\item{xyzt.units}{Units of \code{pixdim[2:5]}. Bits 1..3 of \code{xyzt.units} specify
  the (same) space unit of \code{pixdim[2:4]}. Bits 4..6 of \code{xyzt.units} specify the
  time unit of \code{pixdim[5]}. See \file{xyzt-units.txt} in the niftidoc directory of the
  source package. The R function \code{st2xyzt} can be used to encode these values into the \code{xyzt.units} byte.}
\item{cal.max}{Maximum display intensity (white) corresponds to dataset
  value \code{cal.max}. Dataset values above \code{cal.max} should display as white. \code{cal.min} and \code{cal.max} only make sense when applied to scalar-valued
      datasets (i.e., \code{dim[1]} < 5 or \code{dim[6]} = 1).}
\item{cal.min}{Minimum display intensity (black) corresponds to dataset
  value \code{cal.min}. Dataset values below \code{cal.min} should display as black.}
\item{slice.duration}{Time for 1 slice. If this is positive, AND if \code{slice.dim} is nonzero, indicates the amount of time used to acquire 1 slice.}
\item{toffset}{Time axis shift: The \code{toffset} field can be used to indicate a nonzero start point for
   the time axis.  That is, time point m is at \eqn{t=\code{toffset}+m*\code{pixdim[5]}} for
   m=1, ..., \code{dim[5]}-1.}
\item{descrip}{char[80]. This field may contain any text you like}
\item{aux.file}{char[24]. This field is used to store an auxiliary filename.}
\item{qform.code}{NIFTI code (in 0, ... ,4). 0: Arbitrary
  coordinates; 1: Scanner-based anatomical coordinates; 2:
  Coordinates aligned to another file's, or to anatomical "truth" (coregistration); 3:
  Coordinates aligned to Talairach-Tournoux Atlas; 4: MNI 152 normalized coordinates}
\item{sform.code}{NIFTI code (in 0, ... ,4) with the same meaning as
  \code{qform} codes. The basic idea behind having two coordinate systems is to allow the image to store information about (1) the scanner coordinate system used in the acquisition of the volume (in the \code{qform}) and (2) the relationship to a standard coordinate system - e.g. MNI coordinates (in the \code{sform}). 
 The \code{qform} allows orientation information to be kept for alignment
 purposes without losing volumetric information, since the \code{qform} only
 stores a rigid-body transformation (rotation and translation) which
 preserves volume. On the other hand, the \code{sform} stores a general affine
 transformation (shear, scale, rotation and translation) which can map the image coordinates into a standard coordinate system, like Talairach or MNI, without the need to resample the image. 
 By having both coordinate systems, it is possible to keep the original data (without resampling), along with information on how it was acquired (\code{qform}) and how it relates to other images via a standard space (\code{sform}). This ability is advantageous for many analysis pipelines, and has previously required storing additional files along with the image files. By using NIfTI-1 this extra information can be kept in the image files themselves. 
 Note: the \code{qform} and \code{sform} also store information on whether the coordinate system is left-handed or right-handed and so when both are set they must be consistent, otherwise the handedness of the coordinate system (often used to distinguish left-right order) is unknown and the results of applying operations to such an image are unspecified.
}
\item{quatern.b}{Quaternion b param. These b,c,d quaternion parameters
  encode a rotation matrix used when \code{qform.code} > 0 to
  obtain a rigid transformation that maps voxel indices \eqn{(i,j,k)} to
  spatial coordinates \eqn{(x,y,z)}, typically anatomical coordinates assigned
  by the scanner. This transformation (\emph{Method 2} in the
  \file{nifti1.h} documentation) is generated using also the voxel dimensions
  (\code{pixdim[1:4]}) and a 3D shift, i.e. a translation, (\code{qoffset.}*)}
\item{quatern.c}{Quaternion c param}
\item{quatern.d}{Quaternion d param}
\item{qoffset.x}{Quaternion \eqn{x} shift. If the (0020,0032) DICOM attribute is extracted into \eqn{(px,py,pz)}, then
     \code{qoffset.x} = \eqn{-px} ,  \code{qoffset.y} = \eqn{-py}  and  \code{qoffset.z} = \eqn{pz}
   is a reasonable setting when \code{qform.code}=NIFTI XFORM SCANNER ANAT.}
\item{qoffset.y}{Quaternion \eqn{y} shift}
\item{qoffset.z}{Quaternion \eqn{z} shift}
\item{srow.x}{vector of length 4. 1st row affine transform. These \code{srow.}*
parameters contain an affine (non-rigid) transformation (\emph{Method 3} in the \file{nifti1.h}
  documentation) that maps voxel indices \eqn{(i,j,k)} to spatial coordinates \eqn{(x,y,z)}.}
\item{srow.y}{vector of length 4. 2nd row affine transform}
\item{srow.z}{vector of length 4. 3rd row affine transform}
\item{intent.name}{char[16]. name or meaning of data. If no data name is implied or needed, \code{intent.name[1]} should be set to 0.}
}
\value{Returns a list of all the fields needed to create a .hdr file
  (see the function code for details).}
\seealso{ 
  \code{\link{f.basic.hdr.nifti.list.create}},
  \code{\link{f.write.list.to.hdr.nifti}},
  \code{\link{f.nifti.file.summary}} 
}
\examples{
\dontrun{
dim.mat <- c(20,30,40,3)
dim <- c(length(dim.mat), dim.mat, rep(0, 7 - length(dim.mat)))
filename <- "temp.hdr"
f.complete.hdr.nifti.list.create(file=filename,dim=dim)
}
}
\keyword{utilities}

