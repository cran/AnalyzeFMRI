

f.read.nifti.header <- function(file){
  #This function reads in the information from an ANALYZE format .hdr file
    file.name <- substring(file, 1, nchar(file) - 4)
    file.hdr <- paste(file.name, ".hdr", sep = "")
    file.img <- paste(file.name, ".img", sep = "")

    if(file.exists(file.img) == FALSE) return(paste(file.img, "not found"))
    if(file.exists(file.hdr) == FALSE) return(paste(file.hdr, "not found"))

#Detect whether the data is big or little endian. The first part of a .hdr file is the size of the file which is always a C int (i.e. 4 bytes) and always has value 348. Therefore trying to read it in assuming little-endian will tell you if that is the correct mode

    swap <- 0

    if(.C("swaptest_wrap_JM",
          ans = integer(1),
          file.hdr,
          PACKAGE="AnalyzeFMRI")$ans != 348) # $ans is sizeof_hdr
        swap <- 1


    
# A C function is used to read in all the components of the .hdr file
    a<-.C("read_nifti_header_wrap_JM",
          file.hdr,  # name of hdr file (with .hdr extension)       1
          as.integer(swap), # as defined above                      2
          integer(1), # sizeof_hdr                                  3
          paste(rep(" ", 10), sep = "", collapse = ""), # data_type 4
          paste(rep(" ", 18), sep = "", collapse = ""), # db_name   5
          integer(1), # extents                                     6
          integer(1), # session_error                               7
          paste(rep(" ", 1), sep = "", collapse = ""), # regular    8
          paste(rep(" ", 1), sep = "", collapse = ""), # dim_info   9
          integer(8), # dim                                         10
          single(1), # intent_p1                                    11
          single(1), # intent_p1                                    12
          single(1), # intent_p1                                    13
          integer(1), # intent_code                                 14
          integer(1), # datatype                                    15
          integer(1), # bitpix                                      16
          integer(1), # slice_start                                 17
          single(8), # pixdim                                       18
          single(1), # vox_offset                                   19
          single(1), #         scl_slope                            20
          single(1), #        scl_inter                             21
          integer(1), #      slice_end                              22
          paste(rep(" ", 1), sep = "", collapse = ""), # slice_code 23       
          paste(rep(" ", 1), sep = "", collapse = ""), # xyzt_units 24
          single(1), # cal_max                                      25
          single(1), # cal_min                                      26
          single(1), # compressed                                   27
          single(1), #         toffset                              28
          integer(1), # glmax                                       29
          integer(1), # glmin                                       30
          paste(rep(" ", 80), sep = "", collapse = ""), # descrip   31
          paste(rep(" ", 24), sep = "", collapse = ""), # aux_file  32
          integer(1), #      qform_code                             33
          integer(1), #      sform_code                             34
          single(1), # quatern_b                                    35
          single(1), # quatern_c                                    36
          single(1), # quatern_d                                    37
          single(1), # qoffset_x                                    38
          single(1), # qoffset_y                                    39
          single(1), # qoffset_z                                    40
          single(1), # srow_x                                       41
          single(1), # srow_y                                       42
          single(1), # srow_z                                       43
          paste(rep(" ", 16), sep = "", collapse = ""), # intent_name 44
          paste(rep(" ", 4), sep = "", collapse = ""), # magic        45  
          PACKAGE="AnalyzeFMRI")

# A list (called L) is created containing all the components of the .hdr file

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


