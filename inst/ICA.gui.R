require(tcltk) || stop("tcltk support is absent")

local({

gui.tkfilefind<-function(path=getwd(), all.names=FALSE, multiple=TRUE){
  tclRequire("::Utility")
  tclRequire("Hierarchy")
  .Tcl("namespace import -force ::Utility::*")
  done <- FALSE
  base <- tktoplevel()
  on.exit({if(!done) tkdestroy(base)})
  tkwm.title(base, "Select ANALYZE .img file")
  dirtree<-tkwidget(base, "hierarchy_dir",
                    root=path,
                    showparent="Parent",
                    showfiles=1,
                    showall=all.names,
                    selectmode=if(multiple) "multiple" else "browse")
  tkpack(dirtree, fill="both", expand=1)
  
  .tclfilename <- NULL
  selected <- function(){
    index <- tkcurselection(dirtree)
    index <- strsplit(index," ")[[1]] # multiple selections
    fname <- tkget(dirtree, index)
    tkdestroy(base)
    if (fname==""){
      .tclfilename <<- NULL
      return()
    }
    fnamelist<-strsplit(fname,"}")[[1]]
    
    for (i in seq(along=fnamelist)){
      fnamelist[i] <- gsub("[ ]*{","",fnamelist[i])
      fnamelist[i] <- paste(strsplit(fnamelist[i]," ")[[1]],
                            collapse=Platform()$file.sep)
    }
    .tclfilename<<- fnamelist
  }
  
  buttonframe<-tkframe(base)
  okbut<-tkbutton(buttonframe, text="Select", command=selected)
  qbut<-tkbutton(buttonframe, text="Quit",
                 command=function() tkdestroy(base))
  tkpack(okbut, qbut, side="left")
  tkpack(buttonframe)
  
  tkwait.window(base)
  done<-TRUE
  return(.tclfilename)
    }

gui.file<-function(){
  tclvar$file<-gui.tkfilefind()}


gui.mask<-function(){
  tclvar$mask<-gui.tkfilefind()}

gui.save<-function(...){
  assign(tclvar$save,tmp.ica.obj,envir=.GlobalEnv)
}




gui.ica<-function(...){
  
  if(tclvar$mask=="" && tclvar$create.mask==0){
    err1<-tktoplevel()
    err1.f1 <- tkframe(err1, relief="groove", borderwidth=2)
    err1.label<-tklabel(err1.f1,text="Either select a mask file or select create mask",bg="#aaaaaa",pady=20,padx=20)
    tkgrid(err1.label)
    tkgrid(err1.f1)
    return()
  }
  
  if(tclvar$mask!="" && tclvar$create.mask==0){msk<-tclvar$mask}
  if(tclvar$mask=="" && tclvar$create.mask==1){msk<-NULL}
  var.norm<-1
  if(tclvar$var.norm==0){var.norm<-0}
  slices<-NULL
  if(tclvar$slices==0){slices<-"all"}
  
tmp.ica.obj<<-f.ica.fmri(tclvar$file,n.comp=as.numeric(tclvar$n.comp),norm.col=var.norm,fun="logcosh",maxit=100,alg.type="parallel",alpha=1,tol=0.0001,mask.file.name=msk,slices)
  
  print("done")
  
}

gui.plot.ica<-function(...){  
  f.plot.ica.fmri(tmp.ica.obj,as.numeric(tclvar$comp))}


  #set up base GUI window
  base.ica <- tktoplevel(bg="#555555")
  tkwm.title(base.ica, "Spatial ICA for fMRI datasets")

  #frame to contain file selection
  ica.f1 <- tkframe(base.ica, relief="groove", borderwidth=2,bg="#555555")
  
  ica.file.entry<-tkentry(ica.f1, textvariable="file",width=50,bg="#ffffff")
  ica.file.find.but <- tkbutton(ica.f1, text="Select File",width=15, command=gui.file,bg="#aaaaaa",anchor="c")
  tkgrid(ica.file.find.but,ica.file.entry,pady=10,padx=10)
  
  ica.mask.entry<-tkentry(ica.f1, textvariable="mask",width=50,bg="#ffffff")
  ica.mask.find.but <- tkbutton(ica.f1, text="Select Mask File",width=15, command=gui.mask,bg="#aaaaaa")

  tkgrid(ica.mask.find.but,ica.mask.entry,padx=10,pady=10)
  tkgrid(ica.f1)
  
  #frame for number of components       
  ica.f2 <- tkframe(base.ica, relief="groove", borderwidth=2,bg="#555555")

  ica.n.comp.label<-tklabel(ica.f2,text="Number of components to extract",bg="#aaaaaa")      
  ica.n.comp.entry<-tkentry(ica.f2,textvariable="n.comp",width=5,bg="#ffffff")
  tclvar$n.comp<-50
  tkgrid(ica.n.comp.label,ica.n.comp.entry,padx=10,pady=10)
  
  tkgrid(ica.f2,sticky="ew")



  #frame for options
  ica.f3 <- tkframe(base.ica, relief="groove", borderwidth=2,bg="#555555")

  ica.normalise.but<-tkcheckbutton(ica.f3,text="Variance Normalise",bg="#aaaaaa",variable="var.norm") 
  ica.slices.but<-tkcheckbutton(ica.f3,text="Exclude top/bottom slices",bg="#aaaaaa",variable="slices") 
  ica.create.mask.but<-tkcheckbutton(ica.f3,text="Create Mask",bg="#aaaaaa",variable="create.mask")
  tkgrid(ica.normalise.but,ica.slices.but,ica.create.mask.but,padx=30,pady=10)
  
  tkgrid(ica.f3,sticky="ew")

  #frame for saving object to R session
  ica.f4 <- tkframe(base.ica, relief="groove", borderwidth=2,bg="#555555")

  ica.save.entry<-tkentry(ica.f4, textvariable="save",width=40,bg="#ffffff")
  ica.save.but <- tkbutton(ica.f4, text="Save to R object",width=15, command=gui.save,bg="#aaaaaa")
  tkgrid(ica.save.but,ica.save.entry,padx=10,pady=10)
  
  tkgrid(ica.f4,sticky="ew")

  #frame for plotting components
  ica.f5 <- tkframe(base.ica, relief="groove", borderwidth=2,bg="#555555")

  ica.plot.entry<-tkentry(ica.f5, textvariable="comp",width=5,bg="#ffffff")
  ica.plot.but <- tkbutton(ica.f5, text="Plot component",width=15, command=gui.plot.ica,bg="#aaaaaa")
  tkgrid(ica.plot.but,ica.plot.entry,padx=10,pady=10)
  
  tkgrid(ica.f5,sticky="ew")
  
  #frame for start and end buttons     
  fr3 <- tkframe(base.ica, borderwidth=2,bg="#555555")
  go.but<- tkbutton(fr3,text="Start",bg="#aaaaaa",command=gui.ica)
  q.but <- tkbutton(fr3,text="Quit",
                    command=function()tkdestroy(base.ica),bg="#aaaaaa")
  tkgrid(go.but,q.but,padx=30,pady=20)
  tkgrid(fr3)

})