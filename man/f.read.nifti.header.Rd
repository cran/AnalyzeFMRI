\name{f.read.nifti.header}
\alias{f.read.nifti.header}
\title{read Nifti header file}
\description{Reads the NIFTI image format .hdr (or .nii) header file into a
  list.}
\usage{f.read.nifti.header(file)}
\arguments{\item{file}{The .hdr (or .nii) file to be read}}
\value{A list containing the information in the fields of the .hdr (.nii) file. 
\item{file.name}{path name of the .img file}
\item{swap}{1 or 0 variable indicating whether files are big (=native) or
  little (=swapped) endian}
\item{sizeof.hdr}{MUST be 348}
\item{data.type}{char[10]. UNUSED}
\item{db.name}{char[18]. UNUSED}
\item{extents}{UNUSED}
\item{session.error}{UNUSED}
\item{regular}{UNUSED, but filled with 'r' as SPM does}
\item{dim.info}{MRI slice ordering: This field encode which spatial
  dimension (1=x, 2=y, or 3=z) corresponds to which acquisition dimension
  for MRI data. In fact, it contains three informations: freq.dim,
  phase.dim and slice.dim, all squished into the single byte field dim.info (2 bits each, since the values for each field are
  limited to the range 0..3). The R function diminfo2fps can be used to extract these values from the dim.info byte.}
\item{dim}{vector (of length 8) of image dimensions. dim[1] specifies the
  number of dimensions. In NIFTI-1 files, dim[2], dim[3], dim[4] are for
  space, dim[5] is for time. The 5th dimension (dim[6]) of the dataset, if
  present (i.e., dim[1]=5 and dim[6] > 1), contains multiple values (for
  example a vector) to be stored at each spatiotemporal location. Uses of dim[7] and dim[8] are not specified in NIFTI-1 format.}
\item{intent.p1}{1st intent parameter: first auxiliary parameter for a
  possible statistical distribution specified in intent.code}
\item{intent.p2}{2nd intent parameter: second auxiliary parameter for a
  possible statistical distribution specified in intent.code}
\item{intent.p3}{3rd intent parameter: third auxiliary parameter for a
  possible statistical distribution specified in intent.code}
\item{intent.code}{NIFTI INTENT code: if 0, this is a raw dataset; if in
range 2...24, this indicates that the numbers in the dataset should be interpreted
   as being drawn from a given distribution. Most such distributions have
   auxiliary parameters (given with intent.p?); if in range 1001...1011,
 this is an other meaning. See file intent-code.txt in the niftidoc
 directory of the source package.  If the dataset DOES NOT have a 5th
 dimension (dim[1]=4), then the auxiliary parameters are the same for
 each voxel, and are given in header fields intent.p1, intent.p2, and
 intent.p3. If the dataset DOES have a 5th dimension (dim[1]=5), then the auxiliary parameters are different for each voxel.}
\item{datatype}{integer indicator of data storage type for each
  voxel. This could be 0 (unknown), 2 (unsigned char = 1 byte), 4 (signed short = 2 bytes), 8 (signed
  int = 4 bytes), 16 (32 bit float = 4 bytes), 32 (64 bit complex = two
  32 bit floats = 8 bytes), 64 (64
  bits float = double = 8 bytes), 128 (RGB triple = three 8 bits bytes =
  3 bytes), 256 (signed char = 1 byte), 512
  (unsigned short = 2 bytes), 768 (unsigned int = 4 bytes), 1024 (signed
  long long = 8 bytes), 1280 (unsigned long long = 8 bytes),
  1536 (128 bit float = long double = 16 bytes), 1792 (128 bit complex = 2 64 bit
  floats = 16 bytes), 2048 (256 bit complex = 2 128 bit floats = 32  bytes).}
\item{bitpix}{the number of bits per voxel. This field MUST correspond with
   the datatype field.  The total number of bytes in the image data is
     dim[2]*  ... * dim[dim[1]+1] * bitpix / 8}
\item{slice.start}{Indicates the start of the slice acquisition pattern, when slice.code is nonzero.  These values
                    are present to allow for the possible addition of
                    "padded" slices at either end of the volume, which
                    don't fit into the slice timing pattern.  If there
                    are no padding slices, then slice.start=0 and
                    slice.end=dim[slice.dim+1]-1 are the correct values.
                    For these values to be meaningful, slice.start must
                    be non-negative and slice.end must be greater than
                    slice.start.}
\item{pixdim}{vector (of length 8). Grid spacings. When reading a
  NIFTI-1 header, pixdim[1] stores qfac (which is either -1 or 1). If
  pixdim[1]=0 (which should not occur), we take qfac=1. pixdim[2],
  pixdim[3] and pixdim[4] give the voxel width along dimension x, y and
  z respectively. pixdim[5] gives the time step (=Time Repetition=TR). The units of
  pixdim can be specified with the xyzt.units field.}
\item{vox.offset}{Offset into .nii file. Should be 352 for a .nii file,
  0 for a nifti .hdr/.img pair.}
\item{scl.slope}{Data scaling: If the scl.slope field is nonzero, then
  each voxel value in the dataset should be scaled as y = scl.slope*x +
  scl.inter, where x = voxel value stored and y = "true" voxel value}
\item{scl.inter}{Data scaling: offset. Idem above.}
\item{slice.end}{Indicates the end of the slice acquisition pattern, when slice.code is nonzero.  These values
                    are present to allow for the possible addition of
                    "padded" slices at either end of the volume, which
                    don't fit into the slice timing pattern.  If there
                    are no padding slices, then slice.start=0 and
                    slice.end=dim[slice.dim+1]-1 are the correct values.
                    For these values to be meaningful, slice.start must
                    be non-negative and slice.end must be greater than
                    slice.start.}
\item{slice.code}{Slice timing order. If this is nonzero, AND if slice.dim is nonzero, AND
                 if slice.duration is positive, indicates the timing
                 pattern of the slice acquisition. The following codes
                 are defined: 0 (unknown), 1 (sequential increasing), 2 (sequential decreasing), 3 (alternating increasing),
                 4 (alternating decreasing), 5 (alternating increasing #2), 6 (alternating decreasing #2)}
\item{xyzt.units}{Units of pixdim[2:5]. Bits 1..3 of xyzt.units specify
  the (same) space unit of pixdim[2:4]. Bits 4..6 of xyzt.units specify the
  time unit of pixdim[5]. See xyzt-units.txt in the niftidoc directory of the
  source package. The R function xyzt2st can be used to extract these values from the xyzt.units byte.}
\item{cal.max}{Maximum display intensity (white) corresponds to dataset
  value cal.max. Dataset values above cal.max should display as white. cal.min and cal.max only make sense when applied to scalar-valued
      datasets (i.e., dim[1] < 5 or dim[6] = 1).}
\item{cal.min}{Minimum display intensity (black) corresponds to dataset
  value cal.min. Dataset values below cal.min should display as black.}
\item{slice.duration}{Time for 1 slice. If this is positive, AND if slice.dim is nonzero, indicates the amount of time used to acquire 1 slice.}
\item{toffset}{Time axis shift: The toffset field can be used to indicate a nonzero start point for
   the time axis.  That is, time point m is at t=toffset+m*pixdim[5] for
   m=1, ..., dim[5]-1.}
\item{glmax}{UNUSED}
\item{glmin}{UNUSED}
\item{descrip}{char[80]. This field may contain any text you like}
\item{aux.file}{char[24]. This field is used to store an auxiliary filename.}
\item{qform.code}{NIFTI code (in 0, ... ,4). 0: Arbitrary
  coordinates; 1: Scanner-based anatomical coordinates; 2:
  Coordinates aligned to another file's, or to anatomical "truth" (coregistration); 3:
  Coordinates aligned to Talairach-Tournoux Atlas; 4: MNI 152 normalized coordinates}
\item{sform.code}{NIFTI code (in 0, ... ,4) with the same meaning as
  qform codes. The basic idea behind having two coordinate systems is to allow the image to store information about (1) the scanner coordinate system used in the acquisition of the volume (in the qform) and (2) the relationship to a standard coordinate system - e.g. MNI coordinates (in the sform). 
 The qform allows orientation information to be kept for alignment
 purposes without losing volumetric information, since the qform only
 stores a rigid-body transformation (rotation and translation) which
 preserves volume. On the other hand, the sform stores a general affine
 transformation (shear, scale, rotation and translation) which can map the image coordinates into a standard coordinate system, like Talairach or MNI, without the need to resample the image. 
 By having both coordinate systems, it is possible to keep the original data (without resampling), along with information on how it was acquired (qform) and how it relates to other images via a standard space (sform). This ability is advantageous for many analysis pipelines, and has previously required storing additional files along with the image files. By using NIfTI-1 this extra information can be kept in the image files themselves. 
 Note: the qform and sform also store information on whether the coordinate system is left-handed or right-handed and so when both are set they must be consistent, otherwise the handedness of the coordinate system (often used to distinguish left-right order) is unknown and the results of applying operations to such an image are unspecified.
}
\item{quatern.b}{Quaternion b param. These b,c,d quaternion parameters
  encode a rotation matrix used when qform.code > 0 to
  obtain a rigid transformation that maps voxel indices (i,j,k) to
  spatial coordinates (x,y,z), typically anatomical coordinates assigned
  by the scanner. This transformation ("Method 2" in the nifti1.h
  documentation) is generated using also the voxel dimensions
  (pixdim[1:4]) and a 3D shift, i.e. a translation, (qoffset.*)}
\item{quatern.c}{Quaternion c param}
\item{quatern.d}{Quaternion d param}
\item{qoffset.x}{Quaternion x shift. If the (0020,0032) DICOM attribute is extracted into (px,py,pz), then
     qoffset.x = -px   qoffset.y = -py   qoffset.z = pz
   is a reasonable setting when qform.code=NIFTI XFORM SCANNER ANAT.}
\item{qoffset.y}{Quaternion y shift}
\item{qoffset.z}{Quaternion z shift}
\item{srow.x}{vector of length 4. 1st row affine transform. These srow.*
parameters contain an affine (non-rigid) transformation ("Method 3" in the nifti1.h
  documentation) that maps voxel indices (i,j,k) to spatial coordinates (x,y,z).}
\item{srow.y}{vector of length 4. 2nd row affine transform}
\item{srow.z}{vector of length 4. 3rd row affine transform}
\item{intent.name}{char[16]. 'name' or meaning of data. If no data name is implied or needed, intent.name[1] should be set to 0.}
\item{magic}{MUST be "nix" or "n+x", where x in 0...9}
\item{extension}{By default,all 4 bytes of this array should be set to zero. In a .nii file, these
   4 bytes will always be present, since the earliest start point for
   the image data is byte #352. In a separate .hdr file, these bytes may
   or may not be present. If not present (i.e., if the length of the .hdr
   file is 348 bytes), then a NIfTI-1 compliant program should use the
   default value of extension={0,0,0,0}. The first byte (extension[0])
   is the only value of this array that is specified at present. The other
   3 bytes are reserved for future use.

   If extension[0] is nonzero, it indicates that extended header information
   is present in the bytes following the extension array. In a .nii file,
   this extended header data is before the image data (and vox_offset
   must be set correctly to allow for this). In a .hdr file, this extended
   data follows extension and proceeds (potentially) to the end of the file.}
}


\examples{
f.read.nifti.header(system.file("example-nifti.hdr", package="AnalyzeFMRI"))
}
\keyword{utilities}



