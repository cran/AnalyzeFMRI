# require(tcltk) || stop("tcltk support is absent")

local({


    gui.file<-function(){
        tcltk::tclvalue(file.name) <- tcltk::tcl("tk_getOpenFile")   }
    
    
    gui.mask<-function(){
        tcltk::tclvalue(mask) <- tcltk::tcl("tk_getOpenFile")  }    
    
    
    
    gui.ica<-function(...){
        
        if(tcltk::tclvalue(mask) == "" && tcltk::tclvalue(create.mask) == 0){
            err1 <- tcltk::tktoplevel()
            err1.f1 <- tcltk::tkframe(err1, relief="groove", borderwidth = 2)
            err1.label <- tcltk::tklabel(err1.f1, text = "Either select a mask file or select create mask", bg = "#aaaaaa", pady = 20, padx = 20)
            tcltk::tkgrid(err1.label)
            tcltk::tkgrid(err1.f1)
            return()
        }
        
        if(tcltk::tclvalue(mask) != "" && tcltk::tclvalue(create.mask) == 0) msk <- tcltk::tclvalue(mask)
        if(tcltk::tclvalue(mask) == "" && tcltk::tclvalue(create.mask) == 1) msk <- NULL
        v.norm <- 1
        if(tcltk::tclvalue(var.norm) == 0) v.norm <- 0
        sl <- NULL
        if(tcltk::tclvalue(slices) == 0) sl <- "all"

        if (tcltk::tclvalue(spatial.or.temporal) == "spatial") is.spatial <- 1 else is.spatial <- 0
        
        cat("Computations have begun. Please, be patient ...\n")

        tmp.ica.obj <<- f.icast.fmri(foncfile = tcltk::tclvalue(file.name),
                               maskfile = msk,
                               is.spatial = is.spatial,
                               n.comp.compute = as.numeric(tcltk::tclvalue(n.comp.compute)),
                               n.comp = as.numeric(tcltk::tclvalue(n.comp)),
                               hp.filter=TRUE)


        
        cat("done\n")
        if (exists("tmp.ica.obj")) rm(tmp.ica.obj, envir = .GlobalEnv)
        gc(F)
        
    }
    
    
    gui.end<-function(...){
        if (exists("tmp.ica.obj")) rm(tmp.ica.obj, envir = .GlobalEnv)
        tcltk::tkdestroy(base.ica)
    }




    
    
    ##set tcl variables
    file.name <- tcltk::tclVar()
    mask <- tcltk::tclVar()
    n.comp <- tcltk::tclVar("")
    var.norm <- tcltk::tclVar("0")
    n.comp.compute <- tcltk::tclVar("1")
    slices <- tcltk::tclVar("0")
    create.mask <- tcltk::tclVar("0")
    save.r <- tcltk::tclVar()
    jpeg <- tcltk::tclVar()
    comp <- tcltk::tclVar()
    spatial.or.temporal <- tcltk::tclVar("spatial")
   
    #set up base GUI window
    if(.Platform$OS.type == "windows") flush.console()
    
    base.ica <- tcltk::tktoplevel(bg="#555555")
    tcltk::tkwm.title(base.ica, "Spatial or Temporal ICA for fMRI datasets")
    
    
    #frame to contain file selection
    ica.f1 <- tcltk::tkframe(base.ica, relief = "groove", borderwidth = 2, bg = "#555555")
    
    ica.file.entry <- tcltk::tkentry(ica.f1, textvariable = file.name, width = 50, bg = "#ffffff")
    ica.file.find.but <- tcltk::tkbutton(ica.f1, text = "Select File", width = 15, command = gui.file, bg = "#aaaaaa", anchor = "c")
    tcltk::tkgrid(ica.file.find.but, ica.file.entry, pady = 10, padx = 10)
    
    ica.mask.entry <- tcltk::tkentry(ica.f1, textvariable = mask, width = 50, bg = "#ffffff")
    ica.mask.find.but <- tcltk::tkbutton(ica.f1, text = "Select Mask File", width = 15, command = gui.mask, bg = "#aaaaaa")
    tcltk::tkgrid(ica.mask.find.but, ica.mask.entry, padx = 10, pady = 10)

    tcltk::tkgrid(ica.f1)
    
    #frame for number of components       
    ica.f2 <- tcltk::tkframe(base.ica, relief = "groove", borderwidth = 2, bg = "#555555")
    
    ica.n.comp.radio <- tcltk::tkradiobutton(ica.f2)
    ica.n.comp.label <- tcltk::tklabel(ica.f2, text = "Enter number of components to extract", bg = "#aaaaaa")      
    ica.n.comp.entry <- tcltk::tkentry(ica.f2, textvariable = n.comp, width = 5, bg = "#ffffff")
    ica.n.comp.compute <- tcltk::tklabel(ica.f2, text = "Or automatic choice", bg = "#aaaaaa")
    ica.n.comp.compute.radio <- tcltk::tkradiobutton(ica.f2)

    tcltk::tkconfigure(ica.n.comp.radio,variable=n.comp.compute,value="0")
    tcltk::tkconfigure(ica.n.comp.compute.radio,variable=n.comp.compute,value="1")
    
    
    tcltk::tkgrid(ica.n.comp.radio,ica.n.comp.label, ica.n.comp.entry, ica.n.comp.compute,ica.n.comp.compute.radio, padx = 10, pady = 10)
    
    tcltk::tkgrid(ica.f2, sticky = "ew")
     
       
    #frame for options
    ica.f3 <- tcltk::tkframe(base.ica, relief = "groove", borderwidth = 2, bg = "#555555")
    
    ica.normalise.but <- tcltk::tkradiobutton(ica.f3) 
    ica.slices.but <- tcltk::tkradiobutton(ica.f3) 
    tcltk::tkconfigure(ica.normalise.but,variable=spatial.or.temporal,value="spatial")
    tcltk::tkconfigure(ica.slices.but,variable=spatial.or.temporal,value="temporal")

    
    tcltk::tkgrid(tcltk::tklabel(ica.f3,text="Spatial ICA ", bg = "#aaaaaa"),ica.normalise.but,tcltk::tklabel(ica.f3,text="Temporal ICA ", bg = "#aaaaaa"), ica.slices.but, padx = 30, pady = 10)
    tcltk::tkgrid(ica.f3, sticky = "ew")



  
    #frame for start and end buttons     
    fr3 <- tcltk::tkframe(base.ica, borderwidth = 2, bg = "#555555")
    go.but<- tcltk::tkbutton(fr3, text = "Start", bg = "#aaaaaa", command = gui.ica)
    q.but <- tcltk::tkbutton(fr3, text = "Quit",
                      command = gui.end, bg = "#aaaaaa")
    tcltk::tkgrid(go.but, q.but, padx = 30, pady = 20)
    tcltk::tkgrid(fr3)
    
})
