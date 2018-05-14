\name{ijk2xyz}
\alias{ijk2xyz}
\title{ijk2xyz}
\description{This function maps from data coordinates (e.g. column i, row j, slice k), into some real world (x,y,z) positions in space. These positions could relate to Talairach-Tournoux (T&T) space, MNI space, or patient-based scanner coordinates.}
\usage{ijk2xyz(ijk=c(1,1,1),method=2,L)}
\arguments{\item{ijk}{matrix. Each column of ijk should contain a voxel index coordinates (i,j,k) to be mapped to its (x,y,z) real coordinates in some other space}
  \item{method}{1 (qform.code=sform.code=0),2 (qform.code>0, rigid
    transformation) or 3 (sform.code>0, affine transformation).}
  \item{L}{header list of a NIFTI file}
}
\details{

The NIfTI format allows storage on disk to be in either a left- or
right-handed coordinate system. However, the format includes an implicit
spatial transformation into a RIGHT-HANDED coordinate system. This
transform maps from data coordinates (e.g. column \eqn{i}, row \eqn{j},
slice \eqn{k}), into some real world \eqn{(x,y,z)} positions in
space. These positions could relate to Talairach-Tournoux (T&T) space,
MNI space, or patient-based scanner coordinates. For T&T, and MNI
coordinates, \eqn{x} increases from left to right, \eqn{y} increases
from posterior to anterior, and \eqn{z} increases in the inferior to
superior direction. Directions in the scanner coordinate system are
similar. MRI data is usually exported as DICOM format, which encodes the
positions and orientations of the slices. When data are converted from
DICOM to NIfTI-1 format, the relevant information can be determined from
the Pixel Spacing, Image Orientation (Patient) and Image Position
(Patient) fields of the DICOM files. NIfTI-1 also allows the space of one image to be mapped to that of
another (via a rigid or affine transform). This is to enable on-the-fly
resampling of registered images. This would allow intra-subject images,
collected with lots of different orientations or resolutions, to be
treated as if they are all in register.


Neurological and radiological conventions only relate to visualization
of axial images. They are unrelated to how the data are stored on disk,
or even how the real-world coordinates are represented. 
It is more appropriate to consider whether the real-world coordinate
system is left- or right-handed (see below). Talairach and Tournoux use
a right-handed system, whereas the storage convention of ANALYZE files
is usually considered as left-handed. These coordinate systems are
mirror images of each other (if you are a psychologist, try explaining
why mirror images appear to be left-right flipped, rather than flipped
up-down, or back-front). Transforming between left- and right-handed
coordinate systems involves flipping, and can not be done by rotations
alone.

\eqn{x}=thumb, \eqn{y}=index finger (forefinger), \eqn{z}=left (resp. right) hand's middle finger for left-handed persons (resp. right-handed persons).


Volume orientation is given by a transformation that maps voxel indices
\eqn{(i,j,k)} to spatial coordinates \eqn{(x,y,z)},
typically anatomical coordinates assigned by the scanner. This
transformation (\emph{Method 2} in the \file{nifti1.h} documentation)
is generated using the voxel dimensions, a quaternion encoding a rotation matrix, and a 3D shift, all stored in the NIfTI-1 header; details can be found in the \file{nifti1.h} comments.
The NIfTI-1 header also provides for a general affine transformation, separate from that described by \emph{Method 2}. This transformation (\emph{Method 3}) also maps voxel indices \eqn{(i,j,k)} to \eqn{(x,y,z)}, which in this case are typically coordinates in a standard space such as the Talairach space. The elements of this transformation matrix are stored in the NIfTI-1 header. For example, the \emph{Method 2} transformation can be constructed from the attributes from a set of DICOM files; the \emph{Method 3} transform can be computed offline and inserted into the header later.
The exact "meaning" of the coordinates given by the \emph{Method 2} and
\emph{Method 3} transformations is recorded in header fields
\code{qform.code} and \code{sform.code}, respectively. Code values can
indicate if the \eqn{(x,y,z)} axes are
\itemize{
\item Anatomical coordinates from the scanner (e.g., the DICOM header)
\item Aligned to some anatomical "truth" or standard
\item Aligned and warped to Talairach-Tournoux coordinates
\item Aligned and warped to MNI-152 coordinates
}
 
 
It is possible that neither transformation is specified (i.e.,
\code{qform.code}=\code{sform.code}=0), in which case we are left with the voxel size
in \code{pixdim[]}, and no orientation is given or assumed. This use
(\emph{Method 1}) is discouraged.


The basic idea behind having two coordinate systems is to allow the image to store information about (1) the scanner coordinate system used in the acquisition of the volume (in the \code{qform}) and (2) the relationship to a standard coordinate system - e.g. MNI coordinates (in the \code{sform}). 
 The \code{qform} allows orientation information to be kept for alignment purposes without losing volumetric information, since the \code{qform} only stores a rigid-body transformation which preserves volume. On the other hand, the \code{sform} stores a general affine transformation which can map the image coordinates into a standard coordinate system, like Talairach or MNI, without the need to resample the image. 
 By having both coordinate systems, it is possible to keep the original data (without resampling), along with information on how it was acquired (\code{qform}) and how it relates to other images via a standard space (\code{sform}). This ability is advantageous for many analysis pipelines, and has previously required storing additional files along with the image files. By using NIfTI-1 this extra information can be kept in the image files themselves. \cr
 Note: the \code{qform} and \code{sform} also store information on whether the coordinate system is left-handed or right-handed (see Q15) and so when both are set they must be consistent, otherwise the handedness of the coordinate system (often used to distinguish left-right order) is unknown and the results of applying operations to such an image are unspecified.


There are 3 different methods by which continuous coordinates can be
attached to voxels.  The discussion below emphasizes 3D volumes, and
the continuous coordinates are referred to as \eqn{(x,y,z)}.  The voxel
index coordinates (i.e., the array indexes) are referred to as \eqn{(i,j,k)},
with valid ranges:\cr
\itemize{
\item  \eqn{i = 0,\ldots,}\code{dim[1]-1}\cr
\item  \eqn{j = 0,\ldots,}\code{dim[2]-1}  (if \code{dim[0]} \eqn{>= 2})\cr
\item  \eqn{k = 0,\ldots,}\code{dim[3]-1}  (if \code{dim[0]} \eqn{>= 3})\cr
  }
The \eqn{(x,y,z)} coordinates refer to the CENTER of a voxel.  In methods
2 and 3, the \code{(x,y,z)} axes refer to a subject-based coordinate system,
with

\eqn{+x} = Right  \eqn{+y} = Anterior  \eqn{+z} = Superior.

This is a right-handed coordinate system.  However, the exact direction
these axes point with respect to the subject depends on \code{qform.code}
(\emph{Method 2}) and \code{sform.code} (\emph{Method 3}).

N.B.: The \eqn{i} index varies most rapidly, \eqn{j} index next, \eqn{k} index slowest.
Thus, voxel \eqn{(i,j,k)} is stored starting at location

   \eqn{(i + j*\code{dim[1]} + k*\code{dim[1]}*\code{dim[2]}) *
     (\code{bitpix}/8)}
   
 into the dataset array.

 N.B.: The ANALYZE 7.5 coordinate system is
 
 \eqn{+x} = Left  \eqn{+y} = Anterior  \eqn{+z} = Superior
 
 which is a left-handed coordinate system.  This backwardness is
 too difficult to tolerate, so this NIFTI-1 standard specifies the
 coordinate order which is most common in functional neuroimaging.

N.B.: The 3 methods below all give the locations of the voxel centers
 in the \eqn{(x,y,z)} coordinate system.  In many cases, programs will wish
 to display image data on some other grid.  In such a case, the program
 will need to convert its desired \eqn{(x,y,z)} values into \eqn{(i,j,k)} values
 in order to extract (or interpolate) the image data.  This operation
 would be done with the inverse transformation to those described below.

N.B.: \emph{Method 2} uses a factor \code{qfac} which is either -1 or 1; \code{qfac} is
 stored in the otherwise unused \code{pixdim[0]}.  If \code{pixdim[0]}=0.0 (which
 should not occur), we take \code{qfac=1}.  Of course, \code{pixdim[0]} is only used
 when reading a NIFTI-1 header, not when reading an ANALYZE 7.5 header.

N.B.: The units of \eqn{(x,y,z)} can be specified using the \code{xyzt.units} field.

\itemize{
\item \emph{METHOD 1} (the "old" way, used only when \code{qform.code} = 0):
 
   The coordinate mapping from \eqn{(i,j,k)} to \eqn{(x,y,z)} is the ANALYZE
   7.5 way.  This is a simple scaling relationship:

     \code{x = pixdim[1] * i}\cr
     \code{y = pixdim[2] * j}\cr
     \code{z = pixdim[3] * k}

   No particular spatial orientation is attached to these \eqn{(x,y,z)}
   coordinates.  (NIFTI-1 does not have the ANALYZE 7.5 orient field,
   which is not general and is often not set properly.)  This method
   is not recommended, and is present mainly for compatibility with
   ANALYZE 7.5 files.

\item   \emph{METHOD 2} (used when \code{qform.code} > 0, which should be the "normal" case):
   
   The \eqn{(x,y,z)} coordinates are given by the \code{pixdim[]} scales, a rotation
   matrix, and a shift.  This method is intended to represent
   "scanner-anatomical" coordinates, which are often embedded in the
   image header (e.g., DICOM fields (0020,0032), (0020,0037), (0028,0030),
   and (0018,0050)), and represent the nominal orientation and location of
   the data.  This method can also be used to represent "aligned"
   coordinates, which would typically result from some post-acquisition
   alignment of the volume to a standard orientation (e.g., the same
   subject on another day, or a rigid rotation to true anatomical
   orientation from the tilted position of the subject in the scanner).
   The formula for \eqn{(x,y,z)} in terms of header parameters and \eqn{(i,j,k)} is:

   \tabular{cccrcc}{
     [ x ] \tab   \tab [ R11 R12 R13 ] [\tab         \code{pixdim[1]} * i ] \tab   \tab [ \code{qoffset.x} ]\cr
     [ y ] \tab = \tab [ R21 R22 R23 ] [\tab         \code{pixdim[2]} * j ] \tab + \tab [ \code{qoffset.y} ]\cr
     [ z ] \tab   \tab [ R31 R32 R33 ] [\tab  \code{qfac} *
     \code{pixdim[3]} * k ]\tab  \tab   [ \code{qoffset.z} ]
     }

   The \code{qoffset.}* shifts are in the NIFTI-1 header.  Note that the center
   of the \eqn{(i,j,k)=(0,0,0)} voxel (first value in the dataset array) is
   just \eqn{(x,y,z)=(\code{qoffset.x},\code{qoffset.y},\code{qoffset.z})}.

   The rotation matrix \eqn{R} is calculated from the \code{quatern.}* parameters.
   This calculation is described below.

   The scaling factor \code{qfac} is either 1 or -1.  The rotation matrix \eqn{R}
   defined by the quaternion parameters is "proper" (has determinant 1).
   This may not fit the needs of the data; for example, if the image
   grid is\cr
     \eqn{i} increases from Left-to-Right\cr
     \eqn{j} increases from Anterior-to-Posterior\cr
     \eqn{k} increases from Inferior-to-Superior\cr
   Then \eqn{(i,j,k)} is a left-handed triple.  In this example, if \code{qfac}=1,
   the \eqn{R} matrix would have to be

     [  1   0   0 ]\cr
     [  0  -1   0 ]  which is "improper" (determinant = -1).\cr
     [  0   0   1 ]

   If we set \code{qfac}=-1, then the \eqn{R} matrix would be

     [  1   0   0 ]\cr
     [  0  -1   0 ]  which is proper.\cr
     [  0   0  -1 ]

   This \eqn{R} matrix is represented by quaternion \eqn{[a,b,c,d] = [0,1,0,0]}
   (which encodes a 180 degree rotation about the \eqn{x}-axis).

\item   \emph{METHOD 3} (used when \code{sform.code} > 0):
   
   The \eqn{(x,y,z)} coordinates are given by a general affine transformation
   of the \eqn{(i,j,k)} indexes:

   \tabular{l}{
     x = \code{srow.x[0]} * i + \code{srow.x[1]} * j + \code{srow.x[2]} * k + \code{srow.x[3]}\cr
     y = \code{srow.y[0]} * i + \code{srow.y[1]} * j + \code{srow.y[2]} * k + \code{srow.y[3]}\cr
     z = \code{srow.z[0]} * i + \code{srow.z[1]} * j + \code{srow.z[2]} * k + \code{srow.z[3]}
   }
   
   The \code{srow.}* vectors are in the NIFTI.1 header.  Note that no use is
   made of \code{pixdim[]} in this method.
 
 
\item   WHY 3 METHODS?
   
   \emph{Method 1} is provided only for backwards compatibility.  The intention
   is that \emph{Method 2} (\code{qform.code} > 0) represents the nominal voxel locations
   as reported by the scanner, or as rotated to some fiducial orientation and
   location.  \emph{Method 3}, if present (\code{sform.code} > 0), is to be used to give
   the location of the voxels in some standard space.  The \code{sform.code}
   indicates which standard space is present.  Both methods 2 and 3 can be
   present, and be useful in different contexts (\emph{method 2} for displaying the
   data on its original grid; \emph{method 3} for displaying it on a standard grid).

   In this scheme, a dataset would originally be set up so that the
   \emph{Method 2} coordinates represent what the scanner reported.  Later,
   a registration to some standard space can be computed and inserted
   in the header.  Image display software can use either transform,
   depending on its purposes and needs.

   In \emph{Method 2}, the origin of coordinates would generally be whatever
   the scanner origin is; for example, in MRI, (0,0,0) is the center
   of the gradient coil.

   In \emph{Method 3}, the origin of coordinates would depend on the value
   of \code{sform.code}; for example, for the Talairach coordinate system,
   (0,0,0) corresponds to the Anterior Commissure.

\item   QUATERNION REPRESENTATION OF ROTATION MATRIX (\emph{METHOD 2})\cr
   
   The orientation of the \eqn{(x,y,z)} axes relative to the \eqn{(i,j,k)} axes
   in 3D space is specified using a unit quaternion \eqn{[a,b,c,d]}, where
   \eqn{a*a+b*b+c*c+d*d=1}.  The \eqn{(b,c,d)} values are all that is needed, since
   we require that \eqn{a = \sqrt(1.0-(b*b+c*c+d*d))} be nonnegative.  The \eqn{(b,c,d)}
   values are stored in the (\code{quatern.b},\code{quatern.c},\code{quatern.d}) fields.

   The quaternion representation is chosen for its compactness in
   representing rotations. The (proper) 3x3 rotation matrix that
   corresponds to \eqn{[a,b,c,d]} is

   \tabular{ccrlllr}{
       \tab   \tab [ \tab a*a+b*b-c*c-d*d   \tab 2*b*c-2*a*d       \tab 2*b*d+2*a*c     \tab ]\cr
     R \tab = \tab [ \tab 2*b*c+2*a*d       \tab a*a+c*c-b*b-d*d   \tab 2*c*d-2*a*b     \tab ]\cr
       \tab   \tab [ \tab 2*b*d-2*a*c       \tab 2*c*d+2*a*b       \tab a*a+d*d-c*c-b*b \tab ]
     }
   \tabular{ccrlllr}{
     \tab    \tab [ R11               \tab R12               \tab R13             \tab]\cr
     \tab  = \tab [ R21               \tab R22               \tab R23             \tab]\cr
     \tab    \tab [ R31               \tab R32               \tab R33             \tab]
       }
       
   If \eqn{(p,q,r)} is a unit 3-vector, then rotation of angle \eqn{h} about that
   direction is represented by the quaternion

     \eqn{[a,b,c,d] = [cos(h/2), p*sin(h/2), q*sin(h/2), r*sin(h/2)]}.

   Requiring \eqn{a >= 0} is equivalent to requiring \eqn{-Pi <= h <= Pi}.  (Note that
   \eqn{[-a,-b,-c,-d]} represents the same rotation as \eqn{[a,b,c,d]}; there are 2
   quaternions that can be used to represent a given rotation matrix \eqn{R}.)
   To rotate a 3-vector \eqn{(x,y,z)} using quaternions, we compute the
   quaternion product

     \eqn{[0,x',y',z'] = [a,b,c,d] * [0,x,y,z] * [a,-b,-c,-d]}

   which is equivalent to the matrix-vector multiply

   \tabular{ccl}{
     [ x' ] \tab      \tab  [ x ]\cr
     [ y' ] \tab  = R \tab  [ y ]   (equivalence depends on a*a+b*b+c*c+d*d=1)\cr
     [ z' ] \tab      \tab  [ z ]
   }
   
   Multiplication of 2 quaternions is defined by the following:

   \eqn{[a,b,c,d] = a*1 + b*I + c*J + d*K}
   
     where\cr
       \eqn{I*I = J*J = K*K = -1} (\eqn{I,J,K} are square roots of -1)\cr
       \eqn{I*J =  K}  ,  \eqn{J*K =  I} ,   \eqn{K*I =  J}\cr
       \eqn{J*I = -K}  ,  \eqn{K*J = -I}  ,  \eqn{I*K = -J}  (not
       commutative!).
       
     For example\cr
       \deqn{[a,b,0,0] * [0,0,0,1] = [0,0,-b,a]}
     since this expands to\cr
       \deqn{(a+b*I)*(K) = (a*K+b*I*K) = (a*K-b*J).}

   The above formula shows how to go from quaternion \eqn{(b,c,d)} to
   rotation matrix and direction cosines.  Conversely, given \eqn{R},
   we can compute the fields for the NIFTI-1 header by

     \eqn{a = 0.5  * \sqrt(1+R11+R22+R33)}    (not stored)\cr
     \eqn{b = 0.25 * (R32-R23) / a}       => \code{quatern.b}\cr
     \eqn{c = 0.25 * (R13-R31) / a}       => \code{quatern.c}\cr
     \eqn{d = 0.25 * (R21-R12) / a}       => \code{quatern.d}

   If \code{a=0} (a 180 degree rotation), alternative formulas are needed.
   See the \file{nifti1.io.c} function mat44.to.quatern() for an implementation
   of the various cases in converting \eqn{R} to \eqn{[a,b,c,d]}.

   Note that \eqn{R}-transpose (= \eqn{R}-inverse) would lead to the quaternion
   \eqn{[a,-b,-c,-d]}.

   The choice to specify the \code{qoffset.x} (etc.) values in the final
   coordinate system is partly to make it easy to convert DICOM images to
   this format.  The DICOM attribute "Image Position (Patient)" (0020,0032)
   stores the \eqn{(Xd,Yd,Zd)} coordinates of the center of the first voxel.
   Here, \eqn{(Xd,Yd,Zd)} refer to DICOM coordinates, and \eqn{Xd=-x}, \eqn{Yd=-y}, \eqn{Zd=z},
   where \eqn{(x,y,z)} refers to the NIFTI coordinate system discussed above.
   (i.e., DICOM \eqn{+Xd} is Left, \eqn{+Yd} is Posterior, \eqn{+Zd} is Superior,\cr
        whereas \eqn{+x} is Right, \eqn{+y} is Anterior  , \eqn{+z} is Superior. )\cr
   Thus, if the (0020,0032) DICOM attribute is extracted into \eqn{(px,py,pz)}, then\cr
     \code{qoffset.x} = \eqn{-px}   \code{qoffset.y} = \eqn{-py}   \code{qoffset.z} = \eqn{pz}\cr
   is a reasonable setting when \code{qform.code}=NIFTI.XFORM.SCANNER.ANAT.

   That is, DICOM's coordinate system is 180 degrees rotated about the \eqn{z}-axis
   from the neuroscience/NIFTI coordinate system.  To transform between DICOM
   and NIFTI, you just have to negate the \eqn{x}- and \eqn{y}-coordinates.

   The DICOM attribute (0020,0037) "Image Orientation (Patient)" gives the
   orientation of the \eqn{x}- and \eqn{y}-axes of the image data in terms of 2 3-vectors.
   The first vector is a unit vector along the \eqn{x}-axis, and the second is
   along the \eqn{y}-axis.  If the (0020,0037) attribute is extracted into the
   value (\eqn{xa,xb,xc,ya,yb,yc)}, then the first two columns of the \eqn{R} matrix
   would be\cr
              [ -xa  -ya ]\cr
              [ -xb  -yb ]\cr
              [  xc   yc ]\cr
   The negations are because DICOM's \eqn{x}- and \eqn{y}-axes are reversed relative
   to NIFTI's.  The third column of the \eqn{R} matrix gives the direction of
   displacement (relative to the subject) along the slice-wise direction.
   This orientation is not encoded in the DICOM standard in a simple way;
   DICOM is mostly concerned with 2D images.  The third column of \eqn{R} will be
   either the cross-product of the first 2 columns or its negative.  It is
   possible to infer the sign of the 3rd column by examining the coordinates
   in DICOM attribute (0020,0032) "Image Position (Patient)" for successive
   slices.  However, this method occasionally fails for reasons that I
   (RW Cox) do not understand.
   }
}
\value{A list containing the matrix xyz of the positions of the points
  specified in ijk.}
\seealso{
  \code{\link{xyz2ijk}}
  \code{\link{Q2R}}
  \code{\link{R2Q}}  
}
\examples{
L <- f.read.header(system.file("example-nifti.hdr",
package="AnalyzeFMRI"))
ijk <- matrix(c(1,1,1,2,3,7),byrow=FALSE,nrow=3)
ijk2xyz(ijk=ijk,method=2,L)
}
\keyword{utilities}



