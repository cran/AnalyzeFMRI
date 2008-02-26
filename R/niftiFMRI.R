f.read.nifti.header <- function(file){
  #This function reads in the header information from a NIFTI format file
  
  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")

    if(file.exists(file.hdr) == FALSE) stop(paste(file.hdr, "not found"))
  }

  
  

# Detect whether the data is big or little endian. The first part of a .hdr file is the size of the file which is always a C int (i.e. 4 bytes) and always has value 348.
# Therefore trying to read it in assuming little-endian will tell you if that is the correct mode.

    swap <- 0

    if(.C("swaptest_wrap_JM",
          ans = integer(1),
          file.hdr,
          PACKAGE="AnalyzeFMRI")$ans != 348) # $ans is sizeof_hdr
        swap <- 1


    
# A C function is used to read in all the components of the .hdr file
    a<-.C("read_nifti_header_wrap_JM",
          file.hdr,  # name of hdr file (with .hdr extension)         1
          as.integer(swap), # as defined above                        2
          integer(1), # sizeof_hdr                                    3
          paste(rep(" ", 10), sep = "", collapse = ""), # data_type   4
          paste(rep(" ", 18), sep = "", collapse = ""), # db_name     5
          integer(1), # extents                                       6
          integer(1), # session_error                                 7
          paste(rep(" ", 1), sep = "", collapse = ""), # regular      8
          paste(rep(" ", 1), sep = "", collapse = ""), # dim_info     9
          integer(8), # dim                                           10
          single(1), # intent_p1                                      11
          single(1), # intent_p2                                      12
          single(1), # intent_p3                                      13
          integer(1), # intent_code                                   14
          integer(1), # datatype                                      15
          integer(1), # bitpix                                        16
          integer(1), # slice_start                                   17
          single(8), # pixdim                                         18
          single(1), # vox_offset                                     19
          single(1), #         scl_slope                              20
          single(1), #        scl_inter                               21
          integer(1), #      slice_end                                22
          paste(rep(" ", 1), sep = "", collapse = ""), # slice_code   23       
          paste(rep(" ", 1), sep = "", collapse = ""), # xyzt_units   24
          single(1), # cal_max                                        25
          single(1), # cal_min                                        26
          single(1), # slice_duration                                 27
          single(1), #         toffset                                28
          integer(1), # glmax                                         29
          integer(1), # glmin                                         30
          paste(rep(" ", 80), sep = "", collapse = ""), # descrip     31
          paste(rep(" ", 24), sep = "", collapse = ""), # aux_file    32
          integer(1), #      qform_code                               33
          integer(1), #      sform_code                               34
          single(1), # quatern_b                                      35
          single(1), # quatern_c                                      36
          single(1), # quatern_d                                      37
          single(1), # qoffset_x                                      38
          single(1), # qoffset_y                                      39
          single(1), # qoffset_z                                      40
          single(4), # srow_x                                         41
          single(4), # srow_y                                         42
          single(4), # srow_z                                         43
          paste(rep(" ", 16), sep = "", collapse = ""), # intent_name 44
          paste(rep(" ", 4), sep = "", collapse = ""), # magic        45  
          PACKAGE="AnalyzeFMRI")

# A list (called L) is created containing all the components of the .hdr (or header part of .nii) file

    L <- list()
    L$file.name <- file.img
    L$swap <- a[[2]]
    L$sizeof.hdr <- a[[3]]
    L$data.type <- a[[4]]
    L$db.name <- a[[5]]
    L$extents <- a[[6]]
    L$session.error <- a[[7]]
    L$regular <- a[[8]]
    L$dim.info <- a[[9]]
    L$dim <- a[[10]]
    L$intent.p1 <- a[[11]]
    L$intent.p2 <- a[[12]]
    L$intent.p3 <- a[[13]]
    L$intent.code <- a[[14]]
    L$datatype <- a[[15]]
    L$bitpix <- a[[16]]
    L$slice.start <- a[[17]]
    L$pixdim <- a[[18]]
    L$vox.offset <- a[[19]]  
    L$scl.slope <- a[[20]]
    L$scl.inter <- a[[21]]
    L$slice.end <- a[[22]]
    L$slice.code <- a[[23]]
    L$xyzt.units <- a[[24]]
    L$cal.max <- a[[25]]
    L$cal.min <- a[[26]]
    L$slice.duration <- a[[27]]
    L$toffset <- a[[28]]
    L$glmax <- a[[29]]
    L$glmin <- a[[30]]
    L$descrip <- a[[31]] 
    L$aux.file <- a[[32]]
    L$qform.code <- a[[33]]
    L$sform.code <- a[[34]]
    L$quatern.b <- a[[35]]
    L$quatern.c <- a[[36]]
    L$quatern.d <- a[[37]]
    L$qoffset.x <- a[[38]]
    L$qoffset.y <- a[[39]]
    L$qoffset.z <- a[[40]]
    L$srow.x <- a[[41]]
    L$srow.y <- a[[42]]
    L$srow.z <- a[[43]]
    L$intent.name <- a[[44]]
    L$magic <- a[[45]]

    return(L)}



f.nifti.file.summary <- function(file){
#This function prints out a concise summary of the contents of a NIFTI .img/.hdr image pair (or .nii file)


  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension
  
  if (is.nii == "nii") {
    file.img <- paste(file.name, ".nii", sep = "")
  } 
  else {
    file.img <- paste(file.name, ".img", sep = "")
  }


  hdr <- f.read.nifti.header(file)


  cat("\n")
  cat("       File name:", file.img, "\n")
  cat("  Data Dimension:", paste(hdr$dim[1], "-D", sep = ""), "\n")
  cat("     X dimension:", hdr$dim[2], "\n")
  cat("     Y dimension:", hdr$dim[3], "\n")
  cat("     Z dimension:", hdr$dim[4], "\n")
  cat("  Time dimension:", hdr$dim[5], "time points", "\n")
  cat("Voxel dimensions:", paste(hdr$pixdim[2], hdr$vox.units, "x",
                                 hdr$pixdim[3], hdr$vox.units, "x",
                                 hdr$pixdim[4], hdr$vox.units), "\n")
  cat("       Data type:", hdr$data.type, paste("(", hdr$bitpix, " bits per voxel)", sep = ""), "\n")
}


f.basic.hdr.nifti.list.create <- function(dim.mat, file){

#creates a basic list that can be used to write a .hdr file in NIFTI format (or the header part of a .nii file)

  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension
  
  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
  }
  
  dim <- c(length(dim.mat), dim.mat, rep(0, 7 - length(dim.mat)))
  
  l <- list(file = file.hdr,
            sizeof.hdr = 348,
            data.type = paste(rep(" ", 10), sep = "", collapse = ""),
            db.name = paste(rep(" ", 18), sep = "", collapse = ""),
            extents = integer(1),
            session.error = integer(1),
            regular = character(1),
            dim.info = character(1),
            dim = as.integer(dim),
            intent.p1 = single(1),
            intent.p2 = single(1),
            intent.p3 = single(1),
            intent.code = integer(1),
            datatype = integer(1),
            bitpix = integer(1),
            slice.start = integer(1),
            pixdim = single(8),
            vox.offset = single(1),
            scl.slope = single(1),
            scl.inter = single(1),
            slice.end = integer(1),
            slice.code = character(1),
            xyzt.units = character(1),
            cal.max = single(1),
            cal.min = single(1),
            slice.duration = single(1),
            toffset = single(1),
            glmax = integer(1),
            glmin = integer(1),
            descrip = paste(rep(" ", 80), sep = "", collapse = ""),
            aux.file = paste(rep(" ", 24), sep = "", collapse = ""),
            qform.code = integer(1),
            sform.code = integer(1),
            quatern.b = single(1),
            quatern.c = single(1),
            quatern.d = single(1),
            qoffset.x = single(1),
            qoffset.y = single(1),
            qoffset.z = single(1),
            srow.x = single(4),
            srow.y = single(4),
            srow.z = single(4),
            intent.name = paste(rep(" ", 16), sep = "", collapse = ""),
            magic = paste(rep(" ", 4), sep = "", collapse = ""))
  return(l)
}


f.write.list.to.hdr.nifti <- function(L, file){

# Writes a list to a .hdr file in NIFTI format (and always in little-endian)
  a <- .C("write_nifti_header_wrap_JM",
          file,
          as.integer(L$sizeof.hdr),
          as.character(L$data.type),
          as.character(L$db.name),
          as.integer(L$extents),
          as.integer(L$session.error),
          as.character(L$regular),
          as.character(L$dim.info),
          as.integer(L$dim),
          as.single(L$intent.p1),
          as.single(L$intent.p2),
          as.single(L$intent.p3),
          as.integer(L$intent.code),
          as.integer(L$datatype),
          as.integer(L$bitpix),
          as.integer(L$slice.start),
          as.single(L$pixdim),
          as.single(L$vox.offset),
          as.single(L$scl.slope),
          as.single(L$scl.inter),
          as.integer(L$slice.end),
          as.character(L$slice.code),
          as.character(L$xyzt.units),
          as.single(L$cal.max),
          as.single(L$cal.min),
          as.single(L$slice.duration),
          as.single(L$toffset),
          as.integer(L$glmax),
          as.integer(L$glmin),
          as.character(L$descrip),
          as.character(L$aux.file),
          as.integer(L$qform.code),
          as.integer(L$sform.code),
          as.single(L$quatern.b),
          as.single(L$quatern.c),
          as.single(L$quatern.d),
          as.single(L$qoffset.x),
          as.single(L$qoffset.y),
          as.single(L$qoffset.z),
          as.single(L$srow.x),
          as.single(L$srow.y),
          as.single(L$srow.z),
          as.character(L$intent.name),
          as.character(L$magic),
          PACKAGE="AnalyzeFMRI")
}






f.read.nifti.slice <- function(file, slice, tpt){
  #Reads in a .img file into an array


  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
    toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
    toadd <- 0
    if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
  }


  hdr <- f.read.nifti.header(file.hdr)

  dim <- hdr$dim[2:3]

  num.data.pts <- dim[1] * dim[2]
  if(tpt < 1 || tpt > hdr$dim[5]) stop("tpt is not in range")
  if(slice < 1 || slice > hdr$dim[4]) stop("slice is not in range")
  
  offset <- (tpt - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4] + (slice - 1) * hdr$dim[2] * hdr$dim[3]

  if(hdr$datatype == 2){
    
    vol <- .C("readchar_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 1 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
  #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 4){
    
    vol <- .C("read2byte_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 2 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
#this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 8){
    vol <- .C("read4byte_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 4 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 16){
    vol <- .C("readfloat_v1_JM",
              mat = single(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 4 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim=dim)
#this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 64){
    vol <- .C("readdouble_v1_JM",
              mat = numeric(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 8 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  
  if(hdr$datatype == 0 || hdr$datatype == 1 || hdr$datatype == 32 || hdr$datatype == 128 || hdr$datatype == 255) print(paste("The format", hdr$data.type, "is not supported yet. Please contact me if you want me to extend the functions to do this (marchini@stats.ox.ac.uk)"), quote=FALSE)
  
  return(vol)}



f.read.nifti.tpt <- function(file, tpt){
#Reads in one timepoint of a .img file into an array

  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
    toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
    toadd <- 0
    if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
  }



  hdr <- f.read.nifti.header(file.hdr)

  dim <- hdr$dim[2:4]
  
  num.data.pts <- dim[1] * dim[2] * dim[3]
  if(tpt < 1 || tpt > hdr$dim[5]) stop("tpt is not in range")
  
  offset <- (tpt - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4]
  
  if(hdr$datatype == 2){
    
    vol <- .C("readchar_v1_JM",
                  mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 1 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
                                        #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 4){
    vol <- .C("read2byte_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 2 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 8){
    vol <- .C("read4byte_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 4 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 16){
    vol <- .C("readfloat_v1_JM",
              mat = single(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 4 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 64){
    vol <- .C("readdouble_v1_JM",
              mat = numeric(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(offset * 8 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  
  if(hdr$datatype == 0 || hdr$datatype == 1 || hdr$datatype == 32 || hdr$datatype == 128 || hdr$datatype == 255) print(paste("The format", hdr$data.type, "is not supported yet. Please contact me if you want me to extend the functions to do this (marchini@stats.ox.ac.uk)"), quote = FALSE)
  
  return(vol)}



f.read.nifti.slice.at.all.timepoints <- function(file, slice){
  #Reads in a slice of a .img file at all time points into an array


  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
    toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
    toadd <- 0
    if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
  }

  
  hdr <- f.read.nifti.header(file.hdr)

  dim <- hdr$dim[2:3]
  
  num.data.pts <- dim[1] * dim[2]
  if(slice < 1 || slice > hdr$dim[4]) stop("slice is not in range")
  
  vl <- array(0, dim = hdr$dim[c(2, 3, 5)])
  
  if(hdr$datatype == 2){
    for(i in 1:hdr$dim[5]){
      offset <- (i - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4] + (slice - 1) * hdr$dim[2] * hdr$dim[3]
      vol <- .C("readchar_v1_JM",
                mat = integer(num.data.pts),
                file.img,
                as.integer(hdr$swap),
                as.integer(num.data.pts),
                as.integer(offset * 1 + toadd),
                as.integer(1), PACKAGE="AnalyzeFMRI")
      vol <- array(vol$mat, dim = dim)
      vl[, , i] <- vol
    }
  }
  if(hdr$datatype == 4){
    for(i in 1:hdr$dim[5]){
      offset <- (i - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4] + (slice - 1) * hdr$dim[2] * hdr$dim[3]
      vol <- .C("read2byte_v1_JM",
                mat = integer(num.data.pts),
                file.img,
                as.integer(hdr$swap),
                as.integer(num.data.pts),
                as.integer(offset * 2 + toadd),
                as.integer(1), PACKAGE="AnalyzeFMRI")
      vol <- array(vol$mat, dim = dim)
      vl[, , i] <- vol
    }
  }
  
  if(hdr$datatype == 8){
    for(i in 1:hdr$dim[5]){
      offset <- (i - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4] + (slice - 1) * hdr$dim[2] * hdr$dim[3]
      vol <- .C("read4byte_v1_JM",
                mat = integer(num.data.pts),
                file.img,
                as.integer(hdr$swap),
                as.integer(num.data.pts),
                as.integer(offset * 4 + toadd),
                as.integer(1), PACKAGE="AnalyzeFMRI")
      vol <- array(vol$mat, dim = dim)
      vl[, , i] <- vol
    }
  }
  
  if(hdr$datatype == 16){
    for(i in 1:hdr$dim[5]){
      offset <- (i - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4] + (slice - 1) * hdr$dim[2] * hdr$dim[3]
      vol <- .C("readfloat_v1_JM",
                mat = single(num.data.pts),
                file.img,
                as.integer(hdr$swap),
                as.integer(num.data.pts),
                as.integer(offset * 4 + toadd),
                as.integer(1), PACKAGE="AnalyzeFMRI")
      vol <- array(vol$mat, dim = dim)
      vl[, , i] <- vol
    }
  }
  
  if(hdr$datatype == 64){
    for(i in 1:hdr$dim[5]){
      offset <- (i - 1) * hdr$dim[2] * hdr$dim[3] * hdr$dim[4] + (slice - 1) * hdr$dim[2] * hdr$dim[3]
      vol <- .C("readdouble_v1_JM",
                mat = numeric(num.data.pts),
                file.img,
                as.integer(hdr$swap),
                as.integer(num.data.pts),
                as.integer(offset * 8 + toadd),
                as.integer(1), PACKAGE="AnalyzeFMRI")
      vol <- array(vol$mat, dim = dim)
      vl[, , i] <- vol
    }}
  
  if(hdr$datatype == 0 || hdr$datatype == 1 || hdr$datatype == 32 || hdr$datatype == 128 || hdr$datatype == 255) print(paste("The format", hdr$data.type, "is not supported yet. Please contact me if you want me to extend the functions to do this (marchini@stats.ox.ac.uk)"), quote = FALSE)
  
  return(vl)}



f.read.nifti.ts <- function(file, x, y, z){
  #Reads in a .img file into an array

  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
    toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
    toadd <- 0
    if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
  }


  
  hdr <- f.read.nifti.header(file.hdr)

  if(x < 1 || x > hdr$dim[2]) stop("x is not in range")
  if(y < 1 || y > hdr$dim[3]) stop("y is not in range")
  if(z < 1 || z > hdr$dim[4]) stop("z is not in range")
  
  offset.start <- (z - 1) * hdr$dim[2] * hdr$dim[3] + (y - 1) * hdr$dim[2] + (x - 1)
  offset.add <- hdr$dim[2] * hdr$dim[3] * hdr$dim[4]
  
  vol <- 1:hdr$dim[5]
  
  if(hdr$datatype == 2){
    for(i in 1:hdr$dim[5]){
      
      v <- .C("readchar_v1_JM",
              mat = integer(1),
              file.img,
              as.integer(hdr$swap),
              as.integer(1),
              as.integer(offset.start * 1 + 1 * (i - 1) * offset.add + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
      vol[i] <- v$mat}
  }
  
  if(hdr$datatype  == 4){
    for(i in 1:hdr$dim[5]){
      
      v <- .C("read2byte_v1_JM",
              mat = integer(1),
              file.img,
              as.integer(hdr$swap),
              as.integer(1),
              as.integer(offset.start * 2 + 2 * (i - 1) * offset.add + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
      vol[i] <- v$mat}
  }
  
  if(hdr$datatype  == 8){
    for(i in 1:hdr$dim[5]){
      
      v <- .C("read4byte_v1_JM",
              mat = integer(1),
              file.img,
              as.integer(hdr$swap),
              as.integer(1),
              as.integer(offset.start * 4 + 4 * (i - 1) * offset.add + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
      vol[i] <- v$mat}
  }
  
  if(hdr$datatype == 16){
    for(i in 1:hdr$dim[5]){
      
      v <- .C("readfloat_v1_JM",
              mat = single(1),
              file.img,
              as.integer(hdr$swap),
              as.integer(1),
              as.integer(offset.start * 4 + 4 * (i - 1) * offset.add + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
      vol[i] <- v$mat}
  }
  
  if(hdr$datatype == 64){
    for(i in 1:hdr$dim[5]){
      
      v <- .C("readdouble_v1_JM",
              mat = numeric(1),
              file.img,
              as.integer(hdr$swap),
              as.integer(1),
              as.integer(offset.start * 8 + 8 * (i - 1) * offset.add + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
      vol[i] <- v$mat}
  }
  
  if(hdr$datatype == 0 || hdr$datatype == 1 || hdr$datatype == 32 || hdr$datatype == 128 || hdr$datatype == 255) print(paste("The format", hdr$data.type, "is not supported yet. Please contact me if you want me to extend the functions to do this (marchini@stats.ox.ac.uk)"), quote = FALSE)
  
  return(vol)}


f.read.nifti.volume <- function(file){
  #Reads in a .img file into an array

  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
    toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
    toadd <- 0
    if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
  }



  
  hdr <- f.read.nifti.header(file.hdr)
  num.dim <- hdr$dim[1]
  dim <- hdr$dim[1:num.dim + 1]
  
  num.data.pts <- 1
  for(i in 1:num.dim){num.data.pts <- num.data.pts * dim[i]}
  
  
  if(hdr$datatype == 2){
    vol <- .C("readchar_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(0 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 4){
    vol <- .C("read2byte_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(0 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
#this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 8){
    vol <- .C("read4byte_v1_JM",
              mat = integer(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(0 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
#this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 16){
    vol <- .C("readfloat_v1_JM",
              mat = single(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(0 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }
  if(hdr$datatype == 64){
    vol <- .C("readdouble_v1_JM",
              mat = numeric(num.data.pts),
              file.img,
              as.integer(hdr$swap),
              as.integer(num.data.pts),
              as.integer(0 + toadd),
              as.integer(1), PACKAGE="AnalyzeFMRI")
    vol <- array(vol$mat, dim = dim)
    #this works because array fills itself with the left most subscript moving fastest
  }

  if(hdr$datatype == 0 || hdr$datatype == 1 || hdr$datatype == 32 || hdr$datatype == 128 || hdr$datatype == 255) print(paste("The format", hdr$data.type, "is not supported yet. Please contact me if you want me to extend the functions to do this (marchini@stats.ox.ac.uk)"), quote = FALSE)
  
  return(vol)}




f.spectral.summary.nifti <- function(file, mask.file, ret.flag = FALSE)
{
  #for a NIFTI .img file the periodogram of the time series are divided by a flat spectral estimate using the median periodogram ordinate. The resulting values are then combined within each Fourier frequency and quantiles are plotted against freequency. This provides a fast look at a fMRI dataset to identify any artefacts that reside at single frequencies.

########################
#get info about dataset
########################
  is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension

  file.name <- substring(file, 1, nchar(file) - 4) # file name without extension

  if (is.nii == "nii") {
    file.hdr <- paste(file.name, ".nii", sep = "")
    file.img <- paste(file.name, ".nii", sep = "")
    toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
  } 
  else {
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
    toadd <- 0
    if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
  }


  hdr <- f.read.nifti.header(file.hdr)
  nsl <- hdr$dim[4]
  nim <- hdr$dim[5]
  pxdim <- hdr$pixdim[2:4]

#####################
#read in/create mask
#####################

  f.mask.create <- function(dat, pct = .1, slices = c(0)) {
  #function that creates a mask for an fMRI dataset by thresholding the mean of the pixel time series at a percentage point of the maximum intensity of the dataset
    is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension
    
    file.name <- substring(file, 1, nchar(file) - 4) # file name without extension
    
    if (is.nii == "nii") {
      file.hdr <- paste(file.name, ".nii", sep = "")
      file.img <- paste(file.name, ".nii", sep = "")
      toadd <- 352 # because the image data begins at offset 0+348+4=352 in a .nii file
    } 
    else {
      file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")
      toadd <- 0
      if(file.exists(file.img) == FALSE) stop(paste(file.img, "not found"))
    }
    
    hdr <- f.read.nifti.header(file.hdr)
    nsl <- hdr$dim[4]
    xc <- hdr$dim[2]
    yc <- hdr$dim[3]
    if(slices[1] == 0){slices <- seq(1, nsl)}
    mask <- array(0, dim = c(xc, yc, length(slices)))
    
    max.int <- 0
    for(k in 1:length(slices)){
      slice <- f.read.nifti.slice.at.all.timepoints(dat$file, slices[k])
      if(max(slice)>max.int){max.int <- max(slice)}
    }
    
    for(k in 1:length(slices)){
      slice <- f.read.nifti.slice.at.all.timepoints(dat$file, slices[k])
      
      for(i in 1:(xc * yc)){
        a <- (i - 1) %/% xc + 1
        b <- i - (a - 1) * xc
        mask[b, a, k] <- mean(slice[b, a, ])
        
        if(mask[b, a, k] >= (pct * max.int)){mask[b, a, k] <- 1}
        else{mask[b, a, k] <- 0}
      }
    }
    
    return(mask)
    
  }

  if(mask.file!=FALSE){mask <- f.read.nifti.volume(mask.file)}
  else{
    dat <- list(file = file, mask.file = mask.file)
    mask <- f.mask.create(dat = dat)}
  dim(mask) <- hdr$dim[2:4]

###############
#set constants
###############

  n <- floor(nim / 2) + 1


##########################
#initialise storage arrays
##########################

  res <- array(NA, dim = c(dim(mask), n))

#####################
#main evaluation loop
#####################
  cat("Processing slices...")
  for(l in 1:nsl){
    cat(" [", l, "]", sep = "")
    
    slice <- f.read.nifti.slice.at.all.timepoints(file, l)
    
    
    for(i in 1:(dim(slice)[1] * dim(slice)[2])){
      a <- (i - 1) %/% dim(slice)[1] + 1
      b <- i - (a - 1) * dim(slice)[1]
      if(mask[b, a, l] == 1){
        t <- Mod(fft(slice[b, a, ]) / sqrt(2 * pi * nim))[1:n]
        s <- median(t)
        res[b, a, l, ] <- t / s
      }
    }
  }
  cat("\n")
  
  b <- apply(res, 4, FUN = quantile, probs = seq(.5, 1, .05), na.rm = TRUE)
  plot(c(0, n - 1), c(0, 30), type = "n", xlab = "", ylab = "", axes = FALSE)
  axis(1, at = seq(0, n, 5))
  axis(2, at = seq(0, 30, 5))
  for(i in 0:(n - 1)){
    points(rep(i, 11), b[, i + 1])}
  if(ret.flag == TRUE)  return(b)
}




f.read.header <- function(file){
  #This function reads in the header information from a NIFTI (.nii or .hdr) or ANALYZE (.hdr) file depending on the magic field

	is.nii <- substring(file, nchar(file) - 2,nchar(file))

	file.name <- substring(file, 1, nchar(file) - 4)

	if (is.nii == "nii") {file.hdr <- paste(file.name, ".nii", sep = "")} else {file.hdr <- paste(file.name, ".hdr", sep = "")}

	if(file.exists(file.hdr) == FALSE) stop(paste(file.hdr, "not found"))

#Detect whether the data is big or little endian. The first part of a .hdr file is the size of the file which is always a C int (i.e. 4 bytes) and always has value 348. Therefore trying to read it in assuming little-endian will tell you if that is the correct mode

    swap <- 0

    if(.C("swaptest_wrap_JM",
          ans = integer(1),
          file.hdr,
          PACKAGE="AnalyzeFMRI")$ans != 348) # $ans is sizeof_hdr
        swap <- 1
    
    a<-.C("read_nifti_magic_wrap",
          file.hdr,  
          as.integer(swap), 
	  magic =  paste(rep(" ", 4), sep = "", collapse = ""),
	PACKAGE="AnalyzeFMRI")

if (a$magic == "ni1" | a$magic == "n+1") {res <- f.read.nifti.header(file.hdr)}
else if (a$magic == "") {res <- f.read.analyze.header(file.hdr)}
else {stop("Problem in your magic field!")}

return(res)

}


f.write.nifti <- function(mat, file, size = "float", L = NULL, nii = FALSE){
# Creates a NIFTI .img/.hdr pair of files or a .nii file from a given array


  if(max(mat) == "NA") stop("NA values in array not allowed. Files not written.")

  extension <- substring(file, nchar(file) - 2,nchar(file))
  is.nii <- extension
  
  if (extension == "nii" | extension == "img" | extension == "hdr") {

    if (is.nii == "nii") {
      if (!nii) stop("If you want to create a .nii file, you also shoud put argument nii to TRUE")
      file.hdr <- file
      file.img <- file
    }
    else {
      file.name <- substring(file, 1, nchar(file) - 4)
      file.hdr <- paste(file.name, ".hdr", sep = "")
      file.img <- paste(file.name, ".img", sep = "")
    }
  }

  else {

    if (nii) {
      file.hdr <- paste(file, ".nii", sep = "")
      file.img <- paste(file, ".nii", sep = "")
      
    }
    else {
      file.hdr <- paste(file, ".hdr", sep = "")
      file.img <- paste(file, ".img", sep = "")
    }
  } 
  
  if (is.null(L)) {

    L <- f.basic.hdr.nifti.list.create(dim(mat), file.hdr)
  }

  if (L$datatype == 16) size <- "float"
  if (L$datatype == 4) size <- "int"
  if (L$datatype == 2) size <- "char"

  
  if(size == "float"){
    L$datatype <- 16
    L$bitpix <- 32
    L$data.type <- "float"
    if (nii) {
      f.write.nii.array.to.img.float(mat, L, file.img)
    }
    else {
      f.write.array.to.img.float(mat, file.img)
      f.write.list.to.hdr.nifti(L, file.hdr)
    }
  }
  
  if(size == "int"){
    if(max(mat)>32767 || min(mat) < ( -32768)) stop("Values are outside integer range. Files not written.")
    L$datatype <- 4
    L$bitpix <- 16
    L$data.type <- "signed sho" # signed short
    if (nii) {
      f.write.nii.array.to.img.2bytes(mat, L, file.img)
    }
    else {
      f.write.array.to.img.2bytes(mat, file.img)
      f.write.list.to.hdr.nifti(L, file.hdr)
    }
  }
  
  if(size == "char"){
    if(max(mat)>255 || min(mat) < 0) stop("Values are outside integer range. Files not written.")
    L$datatype <- 2
    L$bitpix <- 8
    L$data.type <- "unsignchar" # unsigned char
    if (nii) {
      f.write.nii.array.to.img.8bit(mat, L, file.img)
    }
    else {
      f.write.array.to.img.8bit(mat, file.img)
      f.write.list.to.hdr.nifti(L, file.hdr)
    }
  }
  
}


f.write.nii.array.to.img.2bytes <- function(mat, L, file){
  #writes an array into a .img file of 2 byte integers
  # and add at the begining of the file the NIFTI header part

  f.write.list.to.hdr.nifti(L, file)
  
  dm <- dim(mat)
  dm.ln <- length(dm)
  num.data.pts <- prod(dm)
  
  null <- .C("write2byteappend_JM",
     as.integer(mat),
     file,
     as.integer(num.data.pts), PACKAGE="AnalyzeFMRI")
  
}

f.write.nii.array.to.img.8bit <- function(mat, L, file){
#writes an array into a .img file of 8 bit (1 byte) integers
  # and add at the begining of the file the NIFTI header part

  f.write.list.to.hdr.nifti(L, file)

  dm <- dim(mat)
  dm.ln <- length(dm)
  num.data.pts <- prod(dm)
  
  null <- .C("write8bitappend_JM",
     as.integer(mat),
     file,
     as.integer(num.data.pts), PACKAGE="AnalyzeFMRI")
  
}



f.write.nii.array.to.img.float <- function(mat, L, file){
  #writes an array into a .img file of 4 byte flotas
  # and add at the begining of the file the NIFTI header part

  f.write.list.to.hdr.nifti(L, file)

  dm <- dim(mat)
  dm.ln <- length(dm)
  num.data.pts <- prod(dm)
  
  null <- .C("writefloatappend_JM",
     as.single(mat),
     file,
     as.integer(num.data.pts), PACKAGE="AnalyzeFMRI")
  
}

