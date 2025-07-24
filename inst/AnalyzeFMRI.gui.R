#require(tcltk) || stop("tcltk support is absent")

local({
 
    wrap.file <- function() tcltk::tclvalue(file.name) <- tcltk::tcl("tk_getOpenFile")
    
    wrap.mask <- function() tcltk::tclvalue(mask) <- tcltk::tcl("tk_getOpenFile") 
    
    do <- function(){
        if(tcltk::tclvalue(alt) == "File Summary") fs()
        if(tcltk::tclvalue(alt) == "Plot Time Series") pts()
        if(tcltk::tclvalue(alt) == "Plot Periodogram") period()
        if(tcltk::tclvalue(alt) == "Image Slice") im.sl()
        if(tcltk::tclvalue(alt) == "Image Volume") im.vol() 
        if(tcltk::tclvalue(alt) == "Movie") im.mov() 
        if(tcltk::tclvalue(alt) == "Spectral Summary") im.spec()
    }
    
    fs <- function(...) f.analyze.file.summary(tcltk::tclvalue(file.name))
    
    pts <- function(...) {
        plot(f.read.analyze.ts(tcltk::tclvalue(file.name),
                               as.numeric(tcltk::tclvalue(x)),
                               as.numeric(tcltk::tclvalue(y)),
                               as.numeric(tcltk::tclvalue(z))),
             typ = "l", ylab = "fMRI response",
             xlab = "Scans")
    }

    period <- function(...){
        oldpar <- par(mfrow = c(1, 1), mar = c(4, 4, 5, 5))
        a <- f.read.analyze.ts(tcltk::tclvalue(file.name),
                               as.numeric(tcltk::tclvalue(x)),
                               as.numeric(tcltk::tclvalue(y)),
                               as.numeric(tcltk::tclvalue(z)))
        b <- fft(a) / sqrt(2 * pi * length(a))
        b <- b[10:floor(length(b) / 2) + 1]
        b <- Mod(b)^2
        plot(b, ylab = "Periodogram", xlab = "Fourier Frequency")
        par(oldpar)
    }
    
    im.sl <- function(...){
        oldpar <- par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
        a <- f.read.analyze.slice(tcltk::tclvalue(file.name),
                                  as.numeric(tcltk::tclvalue(z)),
                                  as.numeric(tcltk::tclvalue(t)))
        
        image(a)
        par(oldpar)
    }
    
    im.vol <- function(...){
        a <- f.read.analyze.header(tcltk::tclvalue(file.name))$dim
        d <- ceiling(sqrt(a[4]))
        oldpar <- par(mfrow =c(d, d), mar = c(0, 0, 0, 0))
        b <- array(0, dim = a[2:4])
        for(i in 1:a[4]){
            b[, , i] <- f.read.analyze.slice(tcltk::tclvalue(file.name),
                                             i,
                                             as.numeric(tcltk::tclvalue(t)))
        }
        for(i in 1:a[4]){
            image(b[, , i], axes = FALSE)
            box()
        }
        par(oldpar)
    }
    
    im.mov <- function(...){
        oldpar <- par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
        a <- f.read.analyze.header(tcltk::tclvalue(file.name))$dim
        b <- array(0, dim = c(a[2], a[3], a[5]))
        for(i in 1:a[5]){
            b[, , i] <- f.read.analyze.slice(tcltk::tclvalue(file.name),
                                             as.numeric(tcltk::tclvalue(z)),
                                             i)
        }
        image(b[, , 1], axes = FALSE)
        for(i in 2:a[5]){
            image(b[, , i], axes = FALSE, add = TRUE)
        }
        par(oldpar)
    }
    
    im.spec <- function(...){
        oldpar <- par(mfrow = c(1, 1), mar = c(4, 4, 5, 5))
        if(tcltk::tclvalue(mask) == "") tcltk::tclvalue(mask) <- FALSE
        a <- f.spectral.summary(tcltk::tclvalue(file.name), tcltk::tclvalue(mask))
        par(oldpar)
    }
    
    ## set up tclVar variables

    mask <- tcltk::tclVar("sdasd")
    alt <- tcltk::tclVar()
    x <- tcltk::tclVar()
    y <- tcltk::tclVar()
    z <- tcltk::tclVar()
    t <- tcltk::tclVar()
    
    #set up base GUI window
    if(.Platform$OS.type == "windows") flush.console()
    
    base <- tcltk::tktoplevel()
    tcltk::tkwm.title(base, "ANALYZE file explore")
    f1 <- tcltk::tkframe(base, relief = "groove", borderwidth = 2)

    file.name <- tcltk::tclVar()
    tcltk::tkpack(tcltk::tkentry(f1, textvariable = file.name, width = 40))
    
    file.find.but <- tcltk::tkbutton(f1, text = "Select File", command = wrap.file)
    tcltk::tkpack(file.find.but)
    
    mask <- tcltk::tclVar()
    tcltk::tkpack(tcltk::tkentry(f1, textvariable = mask, width = 40))
    mask.find.but <- tcltk::tkbutton(f1, text = "Select Mask File", command = wrap.mask)
    tcltk::tkpack(mask.find.but)
    
    opt.rbuts <- tcltk::tkframe(base, relief = "groove", borderwidth = 2)
    
    tcltk::tkpack(tcltk::tklabel(opt.rbuts, text = "Options"))

    alt <- tcltk::tclVar()
    for (i in c("File Summary",
                "Plot Time Series",
                "Plot Periodogram",
                "Image Slice",
                "Image Volume",
                "Movie",
                "Spectral Summary")) {
        tmp <- tcltk::tkradiobutton(opt.rbuts, text = i, variable = alt, value = i)
        tcltk::tkpack(tmp, anchor = "w")
    }
    fr2 <- tcltk::tkframe(base, relief = "groove", borderwidth = 2)
    x <- tcltk::tclVar()
    x.entry <- tcltk::tkentry(fr2, textvariable = x)
    y <- tcltk::tclVar()
    y.entry <- tcltk::tkentry(fr2, textvariable = y)
    z <- tcltk::tclVar()
    z.entry <- tcltk::tkentry(fr2, textvariable = z)
    t <- tcltk::tclVar()
    t.entry <- tcltk::tkentry(fr2, textvariable = t)
    tcltk::tkgrid(f1)
    
    tcltk::tkgrid(tcltk::tklabel(fr2, text = "Variables"), columnspan = 2)
    tcltk::tkgrid(tcltk::tklabel(fr2, text = "x variable"), x.entry)
    tcltk::tkgrid(tcltk::tklabel(fr2, text = "y variable"), y.entry)
    tcltk::tkgrid(tcltk::tklabel(fr2, text = "z variable"), z.entry)
    tcltk::tkgrid(tcltk::tklabel(fr2, text = "t variable"), t.entry)
    tcltk::tkgrid(opt.rbuts)
    tcltk::tkgrid(fr2)
    
    fr3 <- tcltk::tkframe(base, borderwidth = 2)
    q.but <- tcltk::tkbutton(fr3, text = "Quit",
                      command = function() tcltk::tkdestroy(base))
    ok.but <- tcltk::tkbutton(fr3, text = "OK", command = do)
    tcltk::tkgrid(ok.but, q.but)
    tcltk::tkgrid(fr3)
    
})


