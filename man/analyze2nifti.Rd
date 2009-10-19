\name{analyze2nifti}
\alias{analyze2nifti}
\title{Create a NIFTI file from an Analyze file}
\description{Create a NIFTI file from an Analyze file.}
\usage{analyze2nifti(file.in,path.in=".",path.out=".",file.out=NULL,is.nii=TRUE,qform.code=2,sform.code=2,data.type=rawToChar(raw(10)),db.name=rawToChar(raw(18)),dim.info=rawToChar(raw(1)),dim=NULL,TR=0,slice.code=rawToChar(raw(1)),xyzt.units=rawToChar(raw(1)),descrip=NULL,aux.file=rawToChar(raw(24)),intent.name=rawToChar(raw(16)))}
\arguments{\item{file.in}{character, filename of the Analyze file to be read}
  \item{path.in}{character, Directory path from where to take the
    .hdr,.img,.mat files}
\item{path.out}{character, Directory path where to write the
  .hdr/.img or .nii file}
\item{file.out}{character, filename of the NIFTI file to write (without
  extension). If NULL, same as file.in}
\item{is.nii}{logical, if TRUE a NIFTI .nii file will be created, if
  FALSE a .hdr/.img NIFTI file will be created}
\item{qform.code}{qform.code value (in 0,...4)}
\item{sform.code}{sform.code value (in 0,...4)}
\item{data.type}{char[10]. UNUSED in NIFTI-1 but could be filled with what you want}
\item{db.name}{char[18]. UNUSED in NIFTI-1 but could be filled with what you want}
\item{dim.info}{MRI slice ordering: This field encode which spatial
  dimension (1=x, 2=y, or 3=z) corresponds to which acquisition dimension
  for MRI data. In fact, it contains three informations: freq.dim,
  phase.dim and slice.dim, all squished into the single byte field dim.info (2 bits each, since the values for each field are
  limited to the range 0..3). The R function fps2diminfo can be used to encode these values from the dim.info byte.}
\item{dim}{vector (of length 8) of image dimensions. dim[1] specifies the
  number of dimensions. In NIFTI-1 files, dim[2], dim[3], dim[4] are for
  space, dim[5] is for time. The 5th dimension (dim[6]) of the dataset, if
  present (i.e., dim[1]=5 and dim[6] > 1), contains multiple values (for
  example a vector) to be stored at each spatiotemporal location. Uses of dim[7] and dim[8] are not specified in NIFTI-1 format.}
\item{TR}{Time Repetition to be stored in pixdim[5]}
\item{slice.code}{Slice timing order. If this is nonzero, AND if slice.dim is nonzero, AND
                 if slice.duration is positive, indicates the timing
                 pattern of the slice acquisition. The following codes
                 are defined: 0 (NIFTI SLICE UNKNOWN), 1 (NIFTI SLICE
		 SEQ INC), 2 (NIFTI SLICE SEQ DEC), 3 (NIFTI SLICE ALT
		 INC), 4 (NIFTI SLICE ALT DEC)}
\item{xyzt.units}{Units of pixdim[2:5]. Bits 1..3 of xyzt.units specify
  the (same) space unit of pixdim[2:4]. Bits 4..6 of xyzt.units specify the
  time unit of pixdim[5]. See xyzt-units.txt in the niftidoc directory of the
  source package. The R function st2xyzt can be used to encode these values from the xyzt.units byte.}
\item{descrip}{char[80]. This field may contain any text you like}
\item{aux.file}{char[24]. This field is used to store an auxiliary filename.}
\item{intent.name}{char[16]. 'name' or meaning of data. If no data name is implied or needed, intent.name[1] should be set to 0.}
}
\value{Nothing is returned. The NIFTI file is created in the specified
  path.out directory (default is current directory). 
}

\examples{
analyze2nifti(path.in=system.file(package="AnalyzeFMRI"),file.in="example",file.out="nifti-tmp",is.nii=TRUE)
}
\keyword{utilities}



