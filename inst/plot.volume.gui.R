


  maColorBar <- function (x, horizontal = TRUE, col = heat.colors(50), scale = 1:length(x),
                          k = 10, ...)
    {
      if (is.numeric(x)) {
        x <- x
        colmap <- col
      }
      else {
        colmap <- x
        low <- range(scale)[1]
        high <- range(scale)[2]
        x <- seq(low, high, length = length(x))
      }
      if (length(x) > k)
        x.small <- seq(x[1], x[length(x)], length = k)
      else x.small <- x
      if (horizontal) {
        image(x, 1, matrix(x, length(x), 1), axes = FALSE, xlab = "",
              ylab = "", col = colmap, ...)
        axis(1, at = rev(x.small), labels = signif(rev(x.small),
                                     2), srt = 270)
      }
      if (!horizontal) {
        image(1, x, matrix(x, 1, length(x)), axes = FALSE, xlab = "",
              ylab = "", col = colmap, ...)
        oldpar <- par(las = 1)
        axis(4, at = rev(x.small), labels = signif(rev(x.small),2))
        par(las = 0)
        par(oldpar)
      }
      box()
    }

  gui.file.fonc <- function(){
    tcltk::tclvalue(file.name.fonc) <- tcltk::tcl("tk_getOpenFile")   }
    
  gui.file.anat <- function(){
    tcltk::tclvalue(file.name.anat) <- tcltk::tcl("tk_getOpenFile")  }    
    
  gui.file.time.series <- function(){
    tcltk::tclvalue(file.name.time.series) <- tcltk::tcl("tk_getOpenFile")  }    


  plot.volume <- function(vol.fonc="",vol.anat="",time.series="", envir = NULL) {

  ## !!! J'ai pris method=3 (lecture de la matrice affine) pour faire conincider l'anat et la fonctionnelle
  ### si sform.code=0 cela ne marchera pas ...
    method <- 3
    
    hscaletmp <- as.numeric(tcltk::tclvalue(hscaletmp))
    vscaletmp <- as.numeric(tcltk::tclvalue(vscaletmp))
  
# Permet d'afficher un volume cérébral anatomique et/ou fonctionnel
# Paramètres d'entrée:
#	vol.fonc: nom du fichier (.img ou .nii) ou bien une array 4D contenant les images volumiques fonctionnelles
#	vol.anat: nom du fichier (.img ou .nii) ou bien une array 4D contenant les images volumiques anatomiques
#       time.series : fichier .dat contenant les composantes temporelles extraites par l'ICA  


  # J'ai rajouté cela à la fonction plot.volume pour qu'elle soit appelable par le tkwidget
    vol.fonc <- tcltk::tclvalue(file.name.fonc)
    vol.anat <- tcltk::tclvalue(file.name.anat)
    time.series <- tcltk::tclvalue(file.name.time.series)

    is.vol.fonc <- FALSE
    is.vol.anat <- FALSE
    is.time.series <- FALSE
  
    if (nzchar(vol.fonc)[1]) is.vol.fonc <- TRUE
    if (nzchar(vol.anat)[1]) is.vol.anat <- TRUE
    if (nzchar(time.series)[1]) is.time.series <- TRUE
    
    my.title.fonc <- "FMRI visualization:"
    my.title.anat <- "MRI visualization:"

    scanText.fonc <- tcltk::tclVar("Scanner coordinates: ")
    scanText.anat <- tcltk::tclVar("Scanner coordinates: ")

    
    if (is.vol.fonc) {
      
      if (exists(vol.fonc)) vol.fonc <- get(vol.fonc)

      col.fonc <- heat.colors(256)    
    
      if (is.character(vol.fonc)) { # reading of the image
        my.title.fonc <- paste(my.title.fonc,vol.fonc)
        hdr.fonc <- f.read.header(vol.fonc)

        # I added that so that no error is output when visualizing ANALYZE images. But really not sure this is correct!
        file <- vol.fonc
        is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension
        file.name <- substring(file, 1, nchar(file) - 4) # file name without extension
        if (is.nii == "nii") {
          file.hdr <- paste(file.name, ".nii", sep = "")
        } 
        else {
         file.hdr <- paste(file.name, ".hdr", sep = "")
         if(file.exists(file.hdr) == FALSE) stop(paste(file.hdr, "not found"))
        }
        a <- substr(.C("read_nifti_magic_wrap", file.hdr, 0L, magic =  paste(rep(" ", 4), sep = "", collapse = ""), PACKAGE = "AnalyzeFMRI")$magic, start = 1, stop = 2)
        if (a == "") { # Old ANALYZE format
          method <- 1
          hdr.fonc$sform.code <- 0
          hdr.fonc$qform.code <- 0
          }


        
        vol.fonc <- f.read.volume(vol.fonc)



        # if flip.fonc == - 1 :Radiological else if flip.fonc == 1: Neurological
        flip.fonc <- orientation(hdr.fonc)
            
      } else {
        if (!exists("flip.fonc")) print("You must create variable flip.fonc (-1 for Radiological and 1 for Neurological)")
        if (length(dim(vol.fonc)) == 3) {
          vol.fonc <- array(vol.fonc,dim=c(dim(vol.fonc),1))
        } else {vol.fonc <- vol.fonc}
      }
    
      dimensions.fonc <- dim(vol.fonc)
      dim.fonc.sagit <- dimensions.fonc[1]
      dim.fonc.coron <- dimensions.fonc[2]
      dim.fonc.axia <- dimensions.fonc[3]
      dim.fonc.time <- dimensions.fonc[4]
    
# Variable initialisation
      nn.fonc.sagit <- ceiling(dim.fonc.sagit/2)
      nn.fonc.coron <- ceiling(dim.fonc.coron/2)
      nn.fonc.axia <- ceiling(dim.fonc.axia/2)
      nn.fonc.time <- 1
    
      mini.fonc <- min(vol.fonc[,,,nn.fonc.time],na.rm=TRUE)
      maxi.fonc <- max(vol.fonc[,,,nn.fonc.time],na.rm=TRUE)
      if ((mini.fonc-maxi.fonc) == 0) maxi.fonc <- maxi.fonc+0.00001
      breaks.fonc <- c(seq(from=mini.fonc,to=0,len=length(col.fonc)/2),seq(from=0,to=maxi.fonc,len=length(col.fonc)/2+1)) 
    
      SliderSagit.fonc <- tcltk::tclVar(dim.fonc.sagit)
      SliderCoron.fonc <- tcltk::tclVar(dim.fonc.coron)
      SliderAxia.fonc <- tcltk::tclVar(dim.fonc.axia)
      SliderTime.fonc <- tcltk::tclVar(dim.fonc.time)

    }

    if (is.vol.anat) {

      if (exists(vol.anat)) vol.anat <- get(vol.anat)
      
      col.anat <- gray(seq(from=0.2,to=1,len=256))

      if (is.character(vol.anat)) { # reading of the image
        my.title.anat <- paste(my.title.anat,vol.anat)
        hdr.anat <- f.read.header(vol.anat)


        # I added that so that no error is output when visualizing ANALYZE images. But really not sure this is correct!
        file <- vol.anat
        is.nii <- substring(file, nchar(file) - 2,nchar(file)) # file extension
        file.name <- substring(file, 1, nchar(file) - 4) # file name without extension
        if (is.nii == "nii") {
          file.hdr <- paste(file.name, ".nii", sep = "")
        } 
        else {
         file.hdr <- paste(file.name, ".hdr", sep = "")
         if(file.exists(file.hdr) == FALSE) stop(paste(file.hdr, "not found"))
        }
        a <- substr(.C("read_nifti_magic_wrap", file.hdr, 0L, magic =  paste(rep(" ", 4), sep = "", collapse = ""), PACKAGE = "AnalyzeFMRI")$magic, start = 1, stop = 2)
        if (a == "") { # Old ANALYZE format
          method <- 1
          hdr.fonc$sform.code <- 0
          hdr.fonc$qform.code <- 0
          }

        

        vol.anat <- f.read.volume(vol.anat)

        # if flip.anat == - 1 :Radiological else if flip.anat == 1: Neurological
        flip.anat <- orientation(hdr.anat)

      }
      if (!exists("flip.anat")) print("You must create variable flip.anat (-1 for Radiological and 1 for Neurological)")

      if (length(dim(vol.anat)) == 4) vol.anat <- vol.anat[,,,1]
      if (length(dim(vol.anat)) == 2) dim(vol.anat) <- c(dim(vol.anat),1)
    
      dimensions.anat <- dim(vol.anat)
      dim.anat.sagit <- dimensions.anat[1]
      dim.anat.coron <- dimensions.anat[2]
      dim.anat.axia <- dimensions.anat[3]
    
      mini.anat <- min(vol.anat[,,],na.rm=TRUE)
      maxi.anat <- max(vol.anat[,,],na.rm=TRUE)
      if ((mini.anat-maxi.anat) == 0) maxi.anat <- maxi.anat+0.00001
      breaks.anat <- seq(from=mini.anat,to=maxi.anat,len=length(col.anat)+1)
  
  
# Variable initialisation
      nn.anat.sagit <- ceiling(dim.anat.sagit/2)
      nn.anat.coron <- ceiling(dim.anat.coron/2)
      nn.anat.axia <- ceiling(dim.anat.axia/2)
      
      SliderSagit.anat <- tcltk::tclVar(dim.anat.sagit)
      SliderCoron.anat <- tcltk::tclVar(dim.anat.coron)
      SliderAxia.anat <- tcltk::tclVar(dim.anat.axia)

    }

    
    
    if (is.vol.fonc) {
    
# Useful functions
      f.fonc.sagit <- function(...) {
        n.fonc.sagit <- as.numeric(tcltk::tclvalue("nn.fonc.sagit"))
        n.fonc.time <- as.numeric(tcltk::tclvalue("nn.fonc.time"))
        if (n.fonc.sagit != nn.fonc.sagit) {
        #  nn.fonc.sagit <<- n.fonc.sagit
        assign("nn.fonc.sagit", n.fonc.sagit, envir = envir)
          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)

          ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
          xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
          tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

        }
        if (n.fonc.time != nn.fonc.time) {
#          nn.fonc.time <<- n.fonc.time
assign("nn.fonc.time", n.fonc.time, envir = envir)

          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)
        }
      }
      
      f.fonc.coron <- function(...) {
        n.fonc.coron <- as.numeric(tcltk::tclvalue("nn.fonc.coron"))
        n.fonc.time <- as.numeric(tcltk::tclvalue("nn.fonc.time"))
        if (n.fonc.coron != nn.fonc.coron) {
#          nn.fonc.coron <<- n.fonc.coron
assign("nn.fonc.coron", n.fonc.coron, envir = envir)

          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)

          ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
          xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
          tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

        }
        if (n.fonc.time != nn.fonc.time) {
#          nn.fonc.time <<- n.fonc.time
assign("nn.fonc.time", n.fonc.time, envir = envir)

          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)
        }
      }
      
      f.fonc.axia <- function(...) {
        n.fonc.axia <- as.numeric(tcltk::tclvalue("nn.fonc.axia"))
        n.fonc.time <- as.numeric(tcltk::tclvalue("nn.fonc.time"))
        if (n.fonc.axia != nn.fonc.axia) {
#        nn.fonc.axia <<- n.fonc.axia
assign("nn.fonc.axia", n.fonc.axia, envir = envir)

        tkrplot::tkrreplot(img.fonc.sagit)
        tkrplot::tkrreplot(img.fonc.coron)
        tkrplot::tkrreplot(img.fonc.axia)
        tkrplot::tkrreplot(img.fonc.palette)

        ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
        xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
        tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

      }
        if (n.fonc.time != nn.fonc.time) {
#          nn.fonc.time <<- n.fonc.time
assign("nn.fonc.time", n.fonc.time, envir = envir)

          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)
        }
      }
        
      f.fonc.time <- function(...) {
        n.fonc.time <- as.numeric(tcltk::tclvalue("nn.fonc.time"))
        if (n.fonc.time != nn.fonc.time) {
#          nn.fonc.time <<- n.fonc.time
assign("nn.fonc.time", n.fonc.time, envir = envir)
#          mini.fonc <<- min(vol.fonc[,,,nn.fonc.time],na.rm=TRUE)
assign("mini.fonc", min(vol.fonc[,,,nn.fonc.time],na.rm=TRUE), envir = envir)
#          maxi.fonc <<- max(vol.fonc[,,,nn.fonc.time],na.rm=TRUE)
assign("maxi.fonc", max(vol.fonc[,,,nn.fonc.time],na.rm=TRUE), envir = envir)
          if ((mini.fonc-maxi.fonc) == 0) assign("maxi.fonc", maxi.fonc+0.00001, envir = envir) # maxi.fonc <<- maxi.fonc+0.00001
#          breaks.fonc <<- c(seq(from=mini.fonc,to=0,len=length(col.fonc)/2),seq(from=0,to=maxi.fonc,len=length(col.fonc)/2+1)) 
assign("breaks.fonc", c(seq(from=mini.fonc,to=0,len=length(col.fonc)/2),seq(from=0,to=maxi.fonc,len=length(col.fonc)/2+1)), envir = envir)
          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)
          if (is.time.series) tkrplot::tkrreplot(img.time.series)
        }
      }
    
      OnOK.fonc <- function() {tcltk::tkdestroy(tt.fonc);if (is.time.series) tcltk::tkdestroy(tt.fonc.time.series)}
    
  #set up base GUI window
      if(.Platform$OS.type == "windows") flush.console()
    
      tt.fonc <- tcltk::tktoplevel(bg="#555555")

      tcltk::tkwm.title(tt.fonc, my.title.fonc)
    
            
  # frame 1 to contain images
      frame1.fonc <- tcltk::tkframe(tt.fonc, relief = "groove", borderwidth = 0, bg = "#555555")
      label.fonc.sagit <- tcltk::tklabel(frame1.fonc,text="Sagittal ", bg = "#aaaaaa")
      label.fonc.coron <- tcltk::tklabel(frame1.fonc,text="Coronal ", bg = "#aaaaaa")
      slider.fonc.sagit <- tcltk::tkscale(frame1.fonc, command=f.fonc.sagit, from=as.numeric(tcltk::tclvalue(SliderSagit.fonc)), to=1, variable="nn.fonc.sagit",showvalue=TRUE, resolution=1, orient="verti",label="X") 
      img.fonc.sagit <- tkrplot::tkrplot(parent=frame1.fonc, fun=function() {
        oldpar <- par(mar=c(0,0,0,0), bg = "#555555")
    # coupe sagittale
        image((1:dim.fonc.coron)/abs(hdr.fonc$pixdim[3]),(1:dim.fonc.axia)/abs(hdr.fonc$pixdim[4]),as.matrix(vol.fonc[if (flip.fonc == -1) dim.fonc.sagit-nn.fonc.sagit+1 else nn.fonc.sagit,,,nn.fonc.time]),col=col.fonc,breaks=breaks.fonc,axes=FALSE,xlab="",ylab="",asp=dim.fonc.coron/dim.fonc.axia)
        abline(h=nn.fonc.axia/abs(hdr.fonc$pixdim[4]),v=nn.fonc.coron/abs(hdr.fonc$pixdim[3]),col="black")
        par(oldpar)
    }
                              , hscale=hscaletmp, vscale=vscaletmp)
      tcltk::tkbind(img.fonc.sagit, "<Button-1>", function(x, y) {
#        asp <- 2
        wid.fonc <- as.integer(tcltk::tkwinfo("width", img.fonc.sagit))
        hei.fonc <- as.integer(tcltk::tkwinfo("height", img.fonc.sagit))

        y0 <- (hei.fonc - hei.fonc * abs(hdr.fonc$pixdim[3]) / abs(hdr.fonc$pixdim[4])) / 2

        
        #        if (as.numeric(y)>(hei.fonc/2 - dim.fonc.axia*wid.fonc/dim.fonc.coron) & as.numeric(y)<(hei.fonc/2 + dim.fonc.axia*wid.fonc/dim.fonc.coron)) {
        if (as.numeric(y) >= y0 & as.numeric(y) <= (hei.fonc - y0)) {
#          nn.fonc.coron <<- ceiling(dim.fonc.coron * as.numeric(x) /wid.fonc)
assign("nn.fonc.coron", ceiling(dim.fonc.coron * as.numeric(x) /wid.fonc), envir = envir)
#          nn.fonc.axia <<- ceiling(dim.fonc.axia/2 + dim.fonc.coron*(hei.fonc/2-as.numeric(y))/(wid.fonc*asp))



#        nn.fonc.axia <<- ceiling( dim.fonc.axia - dim.fonc.axia * (as.numeric(y) - y0) / (hei.fonc - 2 * y0) )
assign("nn.fonc.axia", ceiling( dim.fonc.axia - dim.fonc.axia * (as.numeric(y) - y0) / (hei.fonc - 2 * y0) ), envir = envir)
#        if (as.numeric(y) < y0) nn.fonc.axia <<- dim.fonc.axia
#        if (as.numeric(y) > (hei.fonc - y0)) nn.fonc.axia <<- 1

          

          
          tcltk::tkset(slider.fonc.coron,nn.fonc.coron)
          tcltk::tkset(slider.fonc.axia,nn.fonc.axia)
          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)

          ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
          xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
          tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))
          
          if (is.vol.anat) {
            tmp <- round(xyz2ijk(xyz.fonc,method=method,hdr.anat)$ijk)
#            nn.anat.sagit <<- tmp[1]
assign("nn.anat.sagit", tmp[1], envir = envir)
#            nn.anat.coron <<- tmp[2]
assign("nn.anat.coron", tmp[2], envir = envir)
#            nn.anat.axia <<- tmp[3]
assign("nn.anat.axia", tmp[3], envir = envir)
            if (exists("img.anat.sagit")) {tkrplot::tkrreplot(img.anat.sagit);tcltk::tkset(slider.anat.sagit,nn.anat.sagit)}
            if (exists("img.anat.coron")) {tkrplot::tkrreplot(img.anat.coron);tcltk::tkset(slider.anat.coron,nn.anat.coron)}
            if (exists("img.anat.axia")) {tkrplot::tkrreplot(img.anat.axia);tcltk::tkset(slider.anat.axia,nn.anat.axia)}

            ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
            xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
            tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))

          }
        }
      })
      slider.fonc.coron <- tcltk::tkscale(frame1.fonc, command=f.fonc.coron, from=as.numeric(tcltk::tclvalue(SliderCoron.fonc)), to=1, variable="nn.fonc.coron",showvalue=TRUE, resolution=1, orient="verti",label="Y")
      img.fonc.coron <- tkrplot::tkrplot(parent=frame1.fonc, fun=function() {
        oldpar <- par(mar=c(0,0,0,0), bg = "#555555")
    # coupe coronale
        image((1:dim.fonc.sagit)/abs(hdr.fonc$pixdim[2]),(1:dim.fonc.axia)/abs(hdr.fonc$pixdim[4]),as.matrix(vol.fonc[if (flip.fonc == -1) rev(1:dim.fonc.sagit) else 1:dim.fonc.sagit,nn.fonc.coron,,nn.fonc.time]),col=col.fonc,breaks=breaks.fonc,axes=FALSE,xlab="",ylab="",asp=dim.fonc.sagit/dim.fonc.axia)
        abline(h=nn.fonc.axia/abs(hdr.fonc$pixdim[4]),v=nn.fonc.sagit/abs(hdr.fonc$pixdim[2]),col="black")
        par(oldpar)
      }
                                , hscale=hscaletmp, vscale=vscaletmp)
      tcltk::tkbind(img.fonc.coron, "<Button-1>", function(x, y) {
#        asp <- 2
        wid.fonc <- as.integer(tcltk::tkwinfo("width", img.fonc.coron))
        hei.fonc <- as.integer(tcltk::tkwinfo("height", img.fonc.coron))

        y0 <- (hei.fonc - hei.fonc * abs(hdr.fonc$pixdim[2]) / abs(hdr.fonc$pixdim[4])) / 2

        if (as.numeric(y) >= y0 & as.numeric(y) <= (hei.fonc - y0)) {

          #if (as.numeric(y)>(hei.fonc/2 - dim.fonc.axia*wid.fonc/dim.fonc.sagit) & as.numeric(y)<(hei.fonc/2 + dim.fonc.axia*wid.fonc/dim.fonc.sagit)) {  
#          nn.fonc.sagit <<- ceiling(dim.fonc.sagit * as.numeric(x) /wid.fonc)
assign("nn.fonc.sagit", ceiling(dim.fonc.sagit * as.numeric(x) /wid.fonc), envir = envir)
          #          nn.fonc.axia <<- ceiling(dim.fonc.axia/2 + dim.fonc.sagit*(hei.fonc/2-as.numeric(y))/(wid.fonc*asp))


#        nn.fonc.axia <<- ceiling( dim.fonc.axia - dim.fonc.axia * (as.numeric(y) - y0) / (hei.fonc - 2 * y0) )
assign("nn.fonc.axia", ceiling( dim.fonc.axia - dim.fonc.axia * (as.numeric(y) - y0) / (hei.fonc - 2 * y0) ), envir = envir)
#        if (as.numeric(y) < y0) nn.fonc.axia <<- dim.fonc.axia
#        if (as.numeric(y) > (hei.fonc - y0)) nn.fonc.axia <<- 1


          
          tcltk::tkset(slider.fonc.sagit,nn.fonc.sagit)
          tcltk::tkset(slider.fonc.axia,nn.fonc.axia)
          tkrplot::tkrreplot(img.fonc.sagit)
          tkrplot::tkrreplot(img.fonc.coron)
          tkrplot::tkrreplot(img.fonc.axia)
          tkrplot::tkrreplot(img.fonc.palette)

          ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
          xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
          tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

          if (is.vol.anat) {
            tmp <- round(xyz2ijk(xyz.fonc,method=method,hdr.anat)$ijk)
#            nn.anat.sagit <<- tmp[1]
assign("nn.anat.sagit", tmp[1], envir = envir)
#            nn.anat.coron <<- tmp[2]
assign("nn.anat.coron", tmp[2], envir = envir)
#            nn.anat.axia <<- tmp[3]
assign("nn.anat.axia", tmp[3], envir = envir)
            if (exists("img.anat.sagit")) {tkrplot::tkrreplot(img.anat.sagit);tcltk::tkset(slider.anat.sagit,nn.anat.sagit)}
            if (exists("img.anat.coron")) {tkrplot::tkrreplot(img.anat.coron);tcltk::tkset(slider.anat.coron,nn.anat.coron)}
            if (exists("img.anat.axia")) {tkrplot::tkrreplot(img.anat.axia);tcltk::tkset(slider.anat.axia,nn.anat.axia)}

            ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
            xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
            tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))

          }
          
        }
      })

  
  # frame 2 to contain Axial image and Palette
      frame2.fonc <- tcltk::tkframe(tt.fonc, relief = "groove", borderwidth = 0, bg = "#555555")
      label.fonc.axia <- tcltk::tklabel(frame2.fonc,text="Axial ", bg = "#aaaaaa")  
      label.fonc.palette <- tcltk::tklabel(frame2.fonc,text="Palette values ", bg = "#aaaaaa")
      slider.fonc.axia <- tcltk::tkscale(frame2.fonc, command=f.fonc.axia, from=as.numeric(tcltk::tclvalue(SliderAxia.fonc)), to=1, variable="nn.fonc.axia",showvalue=TRUE, resolution=1, orient="verti",label="Z")
      img.fonc.axia <- tkrplot::tkrplot(parent=frame2.fonc, fun=function() {
        oldpar <- par(mar=c(0,0,0,0), bg = "#555555")
    # coupe axiale     
        image((1:dim.fonc.sagit)/abs(hdr.fonc$pixdim[2]),(1:dim.fonc.coron)/abs(hdr.fonc$pixdim[3]),as.matrix(vol.fonc[if (flip.fonc == -1) rev(1:dim.fonc.sagit) else 1:dim.fonc.sagit,,nn.fonc.axia,nn.fonc.time]),col=col.fonc,breaks=breaks.fonc,axes=FALSE,xlab="",ylab="",asp=dim.fonc.sagit/dim.fonc.coron)
#      image((1:dim.fonc.sagit)/abs(hdr.fonc$pixdim[2]),(1:dim.fonc.coron)/abs(hdr.fonc$pixdim[3]),as.matrix(vol.fonc[1:dim.fonc.sagit,,nn.fonc.axia,nn.fonc.time]),col=col.fonc,breaks=breaks.fonc,axes=FALSE,xlab="",ylab="",asp=dim.fonc.sagit/dim.fonc.coron)
        abline(h=nn.fonc.coron/abs(hdr.fonc$pixdim[3]),v=nn.fonc.sagit/abs(hdr.fonc$pixdim[2]),col="black")    
        par(oldpar)
      }
                               , hscale=hscaletmp, vscale=vscaletmp)
      tcltk::tkbind(img.fonc.axia, "<Button-1>", function(x, y) {
        wid.fonc <- as.integer(tcltk::tkwinfo("width", img.fonc.axia))
        hei.fonc <- as.integer(tcltk::tkwinfo("height", img.fonc.axia))

                y0 <- (hei.fonc - hei.fonc * abs(hdr.fonc$pixdim[2]) / abs(hdr.fonc$pixdim[3])) / 2

        if (as.numeric(y) >= y0 & as.numeric(y) <= (hei.fonc - y0)) {

#        nn.fonc.sagit <<- ceiling(dim.fonc.sagit * as.numeric(x) /wid.fonc)
assign("nn.fonc.sagit", ceiling(dim.fonc.sagit * as.numeric(x) /wid.fonc), envir = envir)
          #        nn.fonc.coron <<- ceiling(dim.fonc.coron - dim.fonc.coron * as.numeric(y) /hei.fonc)

#                  nn.fonc.coron <<- ceiling( dim.fonc.coron - dim.fonc.coron * (as.numeric(y) - y0) / (hei.fonc - 2 * y0) )
assign("nn.fonc.coron", ceiling( dim.fonc.coron - dim.fonc.coron * (as.numeric(y) - y0) / (hei.fonc - 2 * y0) ), envir = envir)

        tcltk::tkset(slider.fonc.sagit,nn.fonc.sagit)
        tcltk::tkset(slider.fonc.coron,nn.fonc.coron)
        tkrplot::tkrreplot(img.fonc.sagit)
        tkrplot::tkrreplot(img.fonc.coron)
        tkrplot::tkrreplot(img.fonc.axia)
        tkrplot::tkrreplot(img.fonc.palette)

        ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
        xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
        tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))
 
        if (is.vol.anat) {
          tmp <- round(xyz2ijk(xyz.fonc,method=method,hdr.anat)$ijk)
#          nn.anat.sagit <<- tmp[1]
assign("nn.anat.sagit", tmp[1], envir = envir)
#          nn.anat.coron <<- tmp[2]
assign("nn.anat.coron", tmp[2], envir = envir)
#          nn.anat.axia <<- tmp[3]
assign("nn.anat.axia", tmp[3], envir = envir)
          if (exists("img.anat.sagit")) {tkrplot::tkrreplot(img.anat.sagit);tcltk::tkset(slider.anat.sagit,nn.anat.sagit)}
          if (exists("img.anat.coron")) {tkrplot::tkrreplot(img.anat.coron);tcltk::tkset(slider.anat.coron,nn.anat.coron)}
          if (exists("img.anat.axia")) {tkrplot::tkrreplot(img.anat.axia);tcltk::tkset(slider.anat.axia,nn.anat.axia)}

          ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
          xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
          tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))

        }

          }

      })
      img.fonc.palette <- tkrplot::tkrplot(parent=frame2.fonc, fun=function() {
        oldpar <- par(mfrow=c(2,1), bg = "#555555")
        nf.fonc <- layout(matrix(c(1,2), nrow=2, ncol=1, byrow = TRUE),respect = FALSE,widths=c(0.8,0.8),height=c(0.3,0.8))
        par(mar=c(5.1,4.1,4.1,2.1), mai=c(0.5,0.82,0.2,0.42), bg = "#555555")
        maColorBar(x=seq(from=mini.fonc,to=maxi.fonc,len=length(col.fonc)+1),col=col.fonc,horizontal=TRUE)
        par(mar=c(1.1,2.1,2.1,0.1), mai=c(0.5,0.42,0.4,0.22))
        plot(vol.fonc[nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia,],type="l",main=paste("Time course of the selected voxel.\nValue at that time: ",round(vol.fonc[nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia,nn.fonc.time],4),sep=""),xlab="",ylab="",cex.main=0.8)
        points(nn.fonc.time,vol.fonc[nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia,nn.fonc.time],col="blue",pch=1,cex=1.2)
        par(oldpar)
      }
                                  , hscale=hscaletmp, vscale=vscaletmp)
    
 
  # frame 3 to contain time slider and Quit button
      frame3.fonc <- tcltk::tkframe(tt.fonc, relief = "groove", borderwidth = 2, bg = "#555555")
      neuro.fonc <- tcltk::tklabel(frame3.fonc,text="Neurological display")
      timenb.fonc <- tcltk::tklabel(frame3.fonc,text="Time", bg = "#aaaaaa")
      slider.fonc.time <- tcltk::tkscale(frame3.fonc, command=f.fonc.time, from=1, to=as.numeric(tcltk::tclvalue(SliderTime.fonc)), variable="nn.fonc.time",showvalue=TRUE, resolution=1,bigincrement=1,sliderlength=10,digits=0,length=150, orient="horiz")
      coord.fonc <- tcltk::tklabel(frame3.fonc,text=tcltk::tclvalue(scanText.fonc),justify="left")
      OK.but.fonc <- tcltk::tkbutton(frame3.fonc,text="Quit",command=OnOK.fonc, bg = "#aaaaaa")
  
  
# We build the tk window with its widgets
      tcltk::tkgrid(tcltk::tklabel(frame1.fonc,text="             ", bg = "#555555"),label.fonc.coron,tcltk::tklabel(frame1.fonc,text="             ", bg = "#555555"),label.fonc.sagit ,padx = 1, pady = 1)
      tcltk::tkgrid(slider.fonc.sagit,img.fonc.coron,slider.fonc.coron,img.fonc.sagit ,padx = 1, pady = 1)
      tcltk::tkgrid(frame1.fonc)
      tcltk::tkgrid(tcltk::tklabel(frame2.fonc,text="             ", bg = "#555555"),label.fonc.axia,tcltk::tklabel(frame2.fonc,text="             ", bg = "#555555"),label.fonc.palette ,padx = 1, pady = 1)
      tcltk::tkgrid(slider.fonc.axia,img.fonc.axia,tcltk::tklabel(frame2.fonc,text="             ", bg = "#555555"), img.fonc.palette ,padx = 1, pady = 1)
      tcltk::tkgrid(frame2.fonc)
      tcltk::tkconfigure(coord.fonc,textvariable=scanText.fonc,width=35,relief="solid")
      tcltk::tkgrid(neuro.fonc,timenb.fonc,slider.fonc.time,OK.but.fonc,coord.fonc, padx = 10, pady = 10)
      tcltk::tkgrid(frame3.fonc, sticky = "ew")
        
      if (is.time.series) {
        if (exists(time.series)) time.series <- get(time.series)
        if (is.character(time.series)) { # reading of the image
          time.series <- as.matrix(read.table(time.series,header=TRUE))
        } else {
          time.series <- as.matrix(time.series)
        }
      
        tt.fonc.time.series <- tcltk::tktoplevel(bg="#555555")
        tcltk::tkwm.title(tt.fonc.time.series, "Associated time course")
      
        img.time.series <- tkrplot::tkrplot(parent=tt.fonc.time.series, fun=function() {
          oldpar <- par(mar=c(2,2,2,2), bg = "#555555")
          if (dim(time.series)[1] > dim(time.series)[2]) {
            plot(time.series[,nn.fonc.time],type="l",main=paste("Component number ",nn.fonc.time,sep=""))
          } else {
            plot(time.series[nn.fonc.time,],type="l",main=paste("Component number ",nn.fonc.time,sep=""))
          }
          par(oldpar)
        }
                                   , hscale=1.5, vscale=0.5)                   
        
        tcltk::tkgrid(img.time.series) 
      }
      
    } # Fin de if (is.vol.fonc)
    

    if (is.vol.anat) {

# Useful functions
      f.anat.sagit <- function(...) {
        n.anat.sagit <- as.numeric(tcltk::tclvalue("nn.anat.sagit"))
        if (n.anat.sagit != nn.anat.sagit) {
#          nn.anat.sagit <<- n.anat.sagit
assign("nn.anat.sagit", n.anat.sagit, envir = envir)
          tkrplot::tkrreplot(img.anat.sagit)
          tkrplot::tkrreplot(img.anat.coron)
          tkrplot::tkrreplot(img.anat.axia)

          ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
          xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
          tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))

        }
      }
    
      f.anat.coron <- function(...) {
        n.anat.coron <- as.numeric(tcltk::tclvalue("nn.anat.coron"))
        if (n.anat.coron != nn.anat.coron) {
#          nn.anat.coron <<- n.anat.coron
assign("nn.anat.coron", n.anat.coron, envir = envir)
          tkrplot::tkrreplot(img.anat.sagit)
          tkrplot::tkrreplot(img.anat.coron)
          tkrplot::tkrreplot(img.anat.axia)

          ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
          xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
          tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))

        }
      }
    
    
      f.anat.axia <- function(...) {
        n.anat.axia <- as.numeric(tcltk::tclvalue("nn.anat.axia"))
        if (n.anat.axia != nn.anat.axia) {
#          nn.anat.axia <<- n.anat.axia
assign("nn.anat.axia", n.anat.axia, envir = envir)
          tkrplot::tkrreplot(img.anat.sagit)
          tkrplot::tkrreplot(img.anat.coron)
          tkrplot::tkrreplot(img.anat.axia)

          ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
          xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
          tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))
     
        }
      }
    
    
      OnOK.anat <- function() {tcltk::tkdestroy(tt.anat)}
 

  #set up base GUI window
      if(.Platform$OS.type == "windows") flush.console()
    
      tt.anat <- tcltk::tktoplevel(bg="#555555")

      tcltk::tkwm.title(tt.anat, my.title.anat)
      

  # frame 1 to contain images
      frame1.anat <- tcltk::tkframe(tt.anat, relief = "groove", borderwidth = 0, bg = "#555555")
      label.anat.sagit <- tcltk::tklabel(frame1.anat,text="Sagittal ", bg = "#aaaaaa")
      label.anat.coron <- tcltk::tklabel(frame1.anat,text="Coronal ", bg = "#aaaaaa")
      slider.anat.sagit <- tcltk::tkscale(frame1.anat, command=f.anat.sagit, from=as.numeric(tcltk::tclvalue(SliderSagit.anat)), to=1, variable="nn.anat.sagit",showvalue=TRUE, resolution=1, orient="verti",label="X") 
      img.anat.sagit <- tkrplot::tkrplot(parent=frame1.anat, fun=function() {
        oldpar <- par(mar=c(0,0,0,0), bg = "#555555")
    # coupe sagittale
        image((1:dim.anat.coron)/abs(hdr.anat$pixdim[3]),(1:dim.anat.axia)/abs(hdr.anat$pixdim[4]),as.matrix(vol.anat[if (flip.anat == -1) dim.anat.sagit-nn.anat.sagit+1 else nn.anat.sagit,,]),col=col.anat,breaks=breaks.anat,axes=FALSE,xlab="",ylab="",asp=dim.anat.coron/dim.anat.axia)
        abline(h=nn.anat.axia/abs(hdr.anat$pixdim[4]),v=nn.anat.coron/abs(hdr.anat$pixdim[3]),col="red")
        par(oldpar)
      }
                                , hscale=hscaletmp, vscale=vscaletmp)
      tcltk::tkbind(img.anat.sagit, "<Button-1>", function(x, y) {
        wid.anat <- as.integer(tcltk::tkwinfo("width", img.anat.sagit))
        hei.anat <- as.integer(tcltk::tkwinfo("height", img.anat.sagit))

        y0 <- (hei.anat - hei.anat * abs(hdr.anat$pixdim[3]) / abs(hdr.anat$pixdim[4])) / 2
        if (as.numeric(y) >= y0 & as.numeric(y) <= (hei.anat - y0)) {

        
#        nn.anat.coron <<- ceiling(dim.anat.coron * as.numeric(x) /wid.anat)
assign("nn.anat.coron", ceiling(dim.anat.coron * as.numeric(x) /wid.anat), envir = envir)
#        nn.anat.axia <<- ceiling( dim.anat.axia - dim.anat.axia * as.numeric(y) /hei.anat )


#        nn.anat.axia <<- ceiling( dim.anat.axia - dim.anat.axia * (as.numeric(y) - y0) / (hei.anat - 2 * y0) )
assign("nn.anat.axia", ceiling( dim.anat.axia - dim.anat.axia * (as.numeric(y) - y0) / (hei.anat - 2 * y0) ), envir = envir)
#        if (as.numeric(y) < y0) nn.anat.axia <<- dim.anat.axia
#        if (as.numeric(y) > (hei.anat - y0)) nn.anat.axia <<- 1




        tcltk::tkset(slider.anat.coron,nn.anat.coron)	
        tcltk::tkset(slider.anat.axia,nn.anat.axia)
        tkrplot::tkrreplot(img.anat.sagit)
        tkrplot::tkrreplot(img.anat.coron)
        tkrplot::tkrreplot(img.anat.axia)

        ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
        xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
        tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))



        if (is.vol.fonc) {
          tmp <- round(xyz2ijk(xyz.anat,method=method,hdr.fonc)$ijk)
#          nn.fonc.sagit <<- tmp[1]
assign("nn.fonc.sagit", tmp[1], envir = envir)
#          nn.fonc.coron <<- tmp[2]
assign("nn.fonc.coron", tmp[2], envir = envir)
#          nn.fonc.axia <<- tmp[3]
assign("nn.fonc.axia", tmp[3], envir = envir)
          if ((1 <= nn.fonc.sagit) & (nn.fonc.sagit <= dim.fonc.sagit) &(1 <= nn.fonc.coron) & (nn.fonc.coron <= dim.fonc.coron) & (1 <= nn.fonc.axia) & (nn.fonc.axia <= dim.fonc.axia)) {
            tkrplot::tkrreplot(img.fonc.sagit)
            tkrplot::tkrreplot(img.fonc.coron)
            tkrplot::tkrreplot(img.fonc.axia)
            tkrplot::tkrreplot(img.fonc.palette)
            tcltk::tkset(slider.fonc.sagit,nn.fonc.sagit)
            tcltk::tkset(slider.fonc.coron,nn.fonc.coron)
            tcltk::tkset(slider.fonc.axia,nn.fonc.axia)

            ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
            xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
            tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

          }
        }
          }

      })
      slider.anat.coron <- tcltk::tkscale(frame1.anat, command=f.anat.coron, from=as.numeric(tcltk::tclvalue(SliderCoron.anat)), to=1, variable="nn.anat.coron",showvalue=TRUE, resolution=1, orient="verti",label="Y")
      img.anat.coron <- tkrplot::tkrplot(parent=frame1.anat, fun=function() {
        oldpar <- par(mar=c(0,0,0,0), bg = "#555555")
    # coupe coronale
        image((1:dim.anat.sagit)/abs(hdr.anat$pixdim[2]),(1:dim.anat.axia)/abs(hdr.anat$pixdim[4]),as.matrix(vol.anat[if (flip.anat == -1) rev(1:dim.anat.sagit) else 1:dim.anat.sagit,nn.anat.coron,]),col=col.anat,breaks=breaks.anat,axes=FALSE,xlab="",ylab="",asp=dim.anat.sagit/dim.anat.axia)
        abline(h=nn.anat.axia/abs(hdr.anat$pixdim[4]),v=nn.anat.sagit/abs(hdr.anat$pixdim[2]),col="red")
        par(oldpar)
      }
                                , hscale=hscaletmp, vscale=vscaletmp)
      tcltk::tkbind(img.anat.coron, "<Button-1>", function(x, y) {
        wid.anat <- as.integer(tcltk::tkwinfo("width", img.anat.coron))
        hei.anat <- as.integer(tcltk::tkwinfo("height", img.anat.coron))


        y0 <- (hei.anat - hei.anat * abs(hdr.anat$pixdim[2]) / abs(hdr.anat$pixdim[4])) / 2
        if (as.numeric(y) >= y0 & as.numeric(y) <= (hei.anat - y0)) {

#        nn.anat.sagit <<- ceiling(dim.anat.sagit * as.numeric(x) /wid.anat)
assign("nn.anat.sagit", ceiling(dim.anat.sagit * as.numeric(x) /wid.anat), envir = envir)
#        nn.anat.axia <<- ceiling( dim.anat.axia - dim.anat.axia * as.numeric(y) /hei.anat )


#        nn.anat.axia <<- ceiling( dim.anat.axia - dim.anat.axia * (as.numeric(y) - y0) / (hei.anat - 2 * y0) )
assign("nn.anat.axia", ceiling( dim.anat.axia - dim.anat.axia * (as.numeric(y) - y0) / (hei.anat - 2 * y0) ), envir = envir)
#        if (as.numeric(y) < y0) nn.anat.axia <<- dim.anat.axia
#        if (as.numeric(y) > (hei.anat - y0)) nn.anat.axia <<- 1



        
        tcltk::tkset(slider.anat.sagit,nn.anat.sagit)
        tcltk::tkset(slider.anat.axia,nn.anat.axia)
        tkrplot::tkrreplot(img.anat.sagit)
        tkrplot::tkrreplot(img.anat.coron)
        tkrplot::tkrreplot(img.anat.axia)

        ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
        xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
        tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))

        if (is.vol.fonc) {
          tmp <- round(xyz2ijk(xyz.anat,method=method,hdr.fonc)$ijk)
#          nn.fonc.sagit <<- tmp[1]
assign("nn.fonc.sagit", tmp[1], envir = envir)
#          nn.fonc.coron <<- tmp[2]
assign("nn.fonc.coron", tmp[2], envir = envir)
#          nn.fonc.axia <<- tmp[3]
assign("nn.fonc.axia", tmp[3], envir = envir)
          if ((1 <= nn.fonc.sagit) & (nn.fonc.sagit <= dim.fonc.sagit) &(1 <= nn.fonc.coron) & (nn.fonc.coron <= dim.fonc.coron) & (1 <= nn.fonc.axia) & (nn.fonc.axia <= dim.fonc.axia)) {
            tkrplot::tkrreplot(img.fonc.sagit)
            tkrplot::tkrreplot(img.fonc.coron)
            tkrplot::tkrreplot(img.fonc.axia)
            tkrplot::tkrreplot(img.fonc.palette)
            tcltk::tkset(slider.fonc.sagit,nn.fonc.sagit)
            tcltk::tkset(slider.fonc.coron,nn.fonc.coron)
            tcltk::tkset(slider.fonc.axia,nn.fonc.axia)

            ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
            xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
            tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

          }
          }
        }     
      })

  
  # frame 2 to contain Axial image and Palette
      frame2.anat <- tcltk::tkframe(tt.anat, relief = "groove", borderwidth = 0, bg = "#555555")
      label.anat.axia <- tcltk::tklabel(frame2.anat,text="Axial ", bg = "#aaaaaa")  
      label.anat.palette <- tcltk::tklabel(frame2.anat,text="Palette values ", bg = "#aaaaaa")
      slider.anat.axia <- tcltk::tkscale(frame2.anat, command=f.anat.axia, from=as.numeric(tcltk::tclvalue(SliderAxia.anat)), to=1, variable="nn.anat.axia",showvalue=TRUE, resolution=1, orient="verti",label="Z")
      img.anat.axia <- tkrplot::tkrplot(parent=frame2.anat, fun=function() {
        oldpar <- par(mar=c(0,0,0,0), bg = "#555555")
    # coupe axiale     
        image((1:dim.anat.sagit)/abs(hdr.anat$pixdim[2]),(1:dim.anat.coron)/abs(hdr.anat$pixdim[3]),as.matrix(vol.anat[if (flip.anat == -1) rev(1:dim.anat.sagit) else 1:dim.anat.sagit,,nn.anat.axia]),col=col.anat,breaks=breaks.anat,axes=FALSE,xlab="",ylab="",asp=dim.anat.sagit/dim.anat.coron)
        abline(h=nn.anat.coron/abs(hdr.anat$pixdim[3]),v=nn.anat.sagit/abs(hdr.anat$pixdim[2]),col="red")    
        par(oldpar)
      }
                               , hscale=hscaletmp, vscale=vscaletmp)
      tcltk::tkbind(img.anat.axia, "<Button-1>", function(x, y) {
        wid.anat <- as.integer(tcltk::tkwinfo("width", img.anat.axia))
        hei.anat <- as.integer(tcltk::tkwinfo("height", img.anat.axia))

        y0 <- (hei.anat - hei.anat * abs(hdr.anat$pixdim[2]) / abs(hdr.anat$pixdim[3])) / 2
        if (as.numeric(y) >= y0 & as.numeric(y) <= (hei.anat - y0)) {

        
#        nn.anat.sagit <<- ceiling(dim.anat.sagit * as.numeric(x) /wid.anat)
assign("nn.anat.sagit", ceiling(dim.anat.sagit * as.numeric(x) /wid.anat), envir = envir)
          #        nn.anat.coron <<- ceiling(dim.anat.coron - dim.anat.coron * as.numeric(y) /hei.anat)

#        nn.anat.coron <<- ceiling( dim.anat.coron - dim.anat.coron * (as.numeric(y) - y0) / (hei.anat - 2 * y0) )
assign("nn.anat.coron", ceiling( dim.anat.coron - dim.anat.coron * (as.numeric(y) - y0) / (hei.anat - 2 * y0) ), envir = envir)

          
        tcltk::tkset(slider.anat.sagit,nn.anat.sagit)
        tcltk::tkset(slider.anat.coron,nn.anat.coron)
        tkrplot::tkrreplot(img.anat.sagit)
        tkrplot::tkrreplot(img.anat.coron)
        tkrplot::tkrreplot(img.anat.axia)

        ijk.anat <- c(nn.anat.sagit,nn.anat.coron,nn.anat.axia)
        xyz.anat <- ijk2xyz(ijk.anat,method=method,hdr.anat)$xyz
        tcltk::tclvalue(scanText.anat) <- paste("Scanner coordinates:",paste(round(xyz.anat,2),collapse=","))
        
        if (is.vol.fonc) {
          tmp <- round(xyz2ijk(xyz.anat,method=method,hdr.fonc)$ijk)
#          nn.fonc.sagit <<- tmp[1]
assign("nn.fonc.sagit", tmp[1], envir = envir)
#          nn.fonc.coron <<- tmp[2]
assign("nn.fonc.coron", tmp[2], envir = envir)
#          nn.fonc.axia <<- tmp[3]
assign("nn.fonc.axia", tmp[3], envir = envir)
          if ((1 <= nn.fonc.sagit) & (nn.fonc.sagit <= dim.fonc.sagit) &(1 <= nn.fonc.coron) & (nn.fonc.coron <= dim.fonc.coron) & (1 <= nn.fonc.axia) & (nn.fonc.axia <= dim.fonc.axia)) {
            tkrplot::tkrreplot(img.fonc.sagit)
            tkrplot::tkrreplot(img.fonc.coron)
            tkrplot::tkrreplot(img.fonc.axia)
            tkrplot::tkrreplot(img.fonc.palette)
            tcltk::tkset(slider.fonc.sagit,nn.fonc.sagit)
            tcltk::tkset(slider.fonc.coron,nn.fonc.coron)
            tcltk::tkset(slider.fonc.axia,nn.fonc.axia)

            ijk.fonc <- c(nn.fonc.sagit,nn.fonc.coron,nn.fonc.axia)
            xyz.fonc <- ijk2xyz(ijk.fonc,method=method,hdr.fonc)$xyz
            tcltk::tclvalue(scanText.fonc) <- paste("Scanner coordinates:",paste(round(xyz.fonc,2),collapse=","))

          }
        }
          }
      })
      img.anat.palette <- tkrplot::tkrplot(parent=frame2.anat, fun=function() {
        nf.anat <- layout(matrix(c(0,1,0), 1, 3, byrow = TRUE),respect = FALSE,widths=c(4,1,4),height=0.8)
        oldpar <- par(mar=c(0.4,0.1,0.4,0.1), bg = "#555555")
        maColorBar(x=seq(from=mini.anat,to=maxi.anat,len=length(col.anat)+1),col=col.anat,horizontal=FALSE)
        par(oldpar)
      }
                                  , hscale=hscaletmp, vscale=vscaletmp)
  
 
  # frame 3 to contain Quit button
      frame3.anat <- tcltk::tkframe(tt.anat, relief = "groove", borderwidth = 2, bg = "#555555") 
      neuro.anat <- tcltk::tklabel(frame3.anat,text="Neurological display")
      coord.anat <- tcltk::tklabel(frame3.anat,text=tcltk::tclvalue(scanText.anat))
      Quit.but.anat <- tcltk::tkbutton(frame3.anat,text="Quit",command=OnOK.anat, bg = "#aaaaaa")
  
# We build the tk window with its widgets
      tcltk::tkgrid(tcltk::tklabel(frame1.anat,text="             ", bg = "#555555"),label.anat.coron,tcltk::tklabel(frame1.anat,text="             ", bg = "#555555"),label.anat.sagit ,padx = 1, pady = 1)
      tcltk::tkgrid(slider.anat.sagit,img.anat.coron,slider.anat.coron,img.anat.sagit ,padx = 1, pady = 1)
      tcltk::tkgrid(frame1.anat)
      tcltk::tkgrid(tcltk::tklabel(frame2.anat,text="             ", bg = "#555555"),label.anat.axia,tcltk::tklabel(frame2.anat,text="             ", bg = "#555555"),label.anat.palette ,padx = 1, pady = 1)
      tcltk::tkgrid(slider.anat.axia,img.anat.axia,tcltk::tklabel(frame2.anat,text="             ", bg = "#555555"), img.anat.palette ,padx = 1, pady = 1)
      tcltk::tkgrid(frame2.anat)
      tcltk::tkconfigure(coord.anat,textvariable=scanText.anat) ###### ICI
      tcltk::tkgrid(neuro.anat,Quit.but.anat, coord.anat, padx = 10, pady = 10)
      tcltk::tkgrid(frame3.anat, sticky = "ew")   

    } # Fin de if (is.vol.anat)
    
  } # Fin de plot.volume()

  

  gui.end <- function(...){
    tcltk::tkdestroy(base.plot)
  }

    ##set tcl variables
  file.name.fonc <- tcltk::tclVar() ; if (!is.null(array.fonc)) tcltk::tclvalue(file.name.fonc) <- array.fonc
  file.name.anat <- tcltk::tclVar()
  file.name.time.series <- tcltk::tclVar()

   
    #set up base GUI window
  if(.Platform$OS.type == "windows") flush.console()
    
  base.plot <- tcltk::tktoplevel(bg="#555555")
  tcltk::tkwm.title(base.plot, "(F)MRI visualization")
    
    
    #frame to contain file selection
  frame1 <- tcltk::tkframe(base.plot, relief = "groove", borderwidth = 2, bg = "#555555")
    
  file.fonc.entry <- tcltk::tkentry(frame1, textvariable = file.name.fonc, width = 50, bg = "#ffffff")
  file.fonc.find.but <- tcltk::tkbutton(frame1, text = "Select functionnal NIFTI file (or give R object name)", width = 50, command = gui.file.fonc, bg = "#aaaaaa", anchor = "c")
  tcltk::tkgrid(file.fonc.find.but, file.fonc.entry, pady = 10, padx = 10)
  
  file.time.series.entry <- tcltk::tkentry(frame1, textvariable = file.name.time.series, width = 50, bg = "#ffffff")
  file.time.series.find.but <- tcltk::tkbutton(frame1, text = "Select time series component file (or give R object name)", width = 50, command = gui.file.time.series, bg = "#aaaaaa", anchor = "c")
  tcltk::tkgrid(file.time.series.find.but, file.time.series.entry, pady = 10, padx = 10)
  
  file.anat.entry <- tcltk::tkentry(frame1, textvariable = file.name.anat, width = 50, bg = "#ffffff")
  file.anat.find.but <- tcltk::tkbutton(frame1, text = "Select anatomical NIFTI file (or give R object name)", width = 50, command = gui.file.anat, bg = "#aaaaaa", anchor = "c")
  tcltk::tkgrid(file.anat.find.but, file.anat.entry, pady = 10, padx = 10)
    
  tcltk::tkgrid(frame1)
    
  hscaletmp <- tcltk::tclVar(0.6)
  vscaletmp <- tcltk::tclVar(0.6)
  
    #frame for start and end buttons     
  frame2 <- tcltk::tkframe(base.plot, borderwidth = 2, bg = "#555555")
  go.but <- tcltk::tkbutton(frame2, text = "Start", bg = "#aaaaaa", command = function() plot.volume(envir = gui_env))
  q.but <- tcltk::tkbutton(frame2, text = "Quit",command = gui.end, bg = "#aaaaaa")
  
  hscale.txt <- tcltk::tklabel(frame2,text="hscale factor", bg = "#aaaaaa")
  hscale.entry <- tcltk::tkentry(frame2, textvariable = hscaletmp, width = 5, bg = "#ffffff")
  vscale.txt <- tcltk::tklabel(frame2,text="vscale factor", bg = "#aaaaaa")
  vscale.entry <- tcltk::tkentry(frame2, textvariable = vscaletmp, width = 5, bg = "#ffffff")


  tcltk::tkgrid(go.but, q.but, hscale.txt,hscale.entry, vscale.txt,vscale.entry, padx = 30, pady = 20)
  tcltk::tkgrid(frame2)




