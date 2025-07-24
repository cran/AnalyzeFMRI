\name{analyze2nifti}
\alias{analyze2nifti}
\title{Create a NIFTI file from an Analyze file}
\description{Create a NIFTI file from an Analyze file.}
\usage{analyze2nifti(file.in,path.in=".",path.out=NULL,file.out=NULL,is.nii=TRUE,
qform.code=2,sform.code=2,data.type=rawToChar(raw(10)),db.name=rawToChar(raw(18)),
dim.info=rawToChar(raw(1)),dim=NULL,TR=0,slice.code=rawToChar(raw(1)),
xyzt.units=rawToChar(raw(1)),descrip=NULL,aux.file=rawToChar(raw(24)),
intent.name=rawToChar(raw(16)))}
\arguments{\item{file.in}{character, filename of the Analyze file to be read}
  \item{path.in}{character, Directory path from where to take the
    .hdr,.img,.mat files}
\item{path.out}{character, Directory path where to write the
  .hdr/.img or .nii file}
\item{file.out}{character, filename of the NIFTI file to write (without
  extension). If \code{NULL}, same as \code{file.in}}
\item{is.nii}{logical, if \code{TRUE} a NIFTI .nii file will be created, if
  \code{FALSE} a .hdr/.img NIFTI file will be created}
\item{qform.code}{value in 0,...,4}
\item{sform.code}{value in 0,...,4}
\item{data.type}{char[10]. UNUSED in NIFTI-1 but could be filled with what you want}
\item{db.name}{char[18]. UNUSED in NIFTI-1 but could be filled with what you want}
\item{dim.info}{MRI slice ordering: This field encode which spatial
  dimension (1=\eqn{x}, 2=\eqn{y} or 3=\eqn{z}) corresponds to which acquisition dimension
  for MRI data. In fact, it contains three informations: \code{freq.dim},
  \code{phase.dim} and \code{slice.dim}, all squished into the single
  byte field dim.info (2 bits each, since the values for each field are 
  limited to the range 0..3). The R function \code{fps2diminfo} can be used to encode these values from the \code{dim.info} byte.}
\item{dim}{vector (of length 8) of image dimensions. \code{dim[1]} specifies the
  number of dimensions. In NIFTI-1 files, \code{dim[2]}, \code{dim[3]}, \code{dim[4]} are for
  space, \code{dim[5]} is for time. The 5th dimension (\code{dim[6]}) of the dataset, if
  present (i.e., \code{dim[1]}=5 and \code{dim[6]} > 1), contains multiple values (for
  example a vector) to be stored at each spatiotemporal location. Uses of \code{dim[7]} and \code{dim[8]} are not specified in NIFTI-1 format.}
\item{TR}{Time Repetition to be stored in \code{pixdim[5]}}
\item{slice.code}{Slice timing order. If this is nonzero, AND if \code{slice.dim} is nonzero, AND
                 if \code{slice.duration} is positive, indicates the timing
                 pattern of the slice acquisition. The following codes
                 are defined: 0 (NIFTI SLICE UNKNOWN), 1 (NIFTI SLICE
		 SEQ INC), 2 (NIFTI SLICE SEQ DEC), 3 (NIFTI SLICE ALT
		 INC), 4 (NIFTI SLICE ALT DEC)}
\item{xyzt.units}{Units of \code{pixdim[2:5]}. Bits 1..3 of \code{xyzt.units} specify
  the (same) space unit of \code{pixdim[2:4]}. Bits 4..6 of \code{xyzt.units} specify the
  time unit of \code{pixdim[5]}. See \file{xyzt-units.txt} in the niftidoc directory of the
  source package. The R function \code{st2xyzt} can be used to encode these values from the \code{xyzt.units} byte.}
\item{descrip}{char[80]. This field may contain any text you like}
\item{aux.file}{char[24]. This field is used to store an auxiliary filename.}
\item{intent.name}{char[16]. name or meaning of data. If no data name is implied or needed, \code{intent.name[1]} should be set to 0.}
}
\value{Nothing is returned. The NIFTI file is created in the specified
  path.out directory (default is current directory). 
}

\examples{
tmpdir <- tempdir()
analyze2nifti(path.in = system.file(package = "AnalyzeFMRI"),
              path.out = tmpdir, file.in = "example",
              file.out = "nifti-tmp", is.nii = TRUE)
unlink(tmpdir) # tidy up
}
\keyword{utilities}



