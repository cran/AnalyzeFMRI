require(tcltk) || stop("tcltk support is absent")

local({
 
  
           tkfilefind<-function(path=getwd(), all.names=FALSE, multiple=TRUE){
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

           wrap.file<-function(){
             tclvar$file<-tkfilefind()}
           
           wrap.mask<-function(){
             tclvar$mask<-tkfilefind()}

           do<-function(){
             if(tclvar$alt=="File Summary"){fs()}
             if(tclvar$alt=="Plot Time Series"){pts()}
             if(tclvar$alt=="Plot Periodogram"){period()}
             if(tclvar$alt=="Image Slice"){im.sl()}
             if(tclvar$alt=="Image Volume"){im.vol()}
             if(tclvar$alt=="Movie"){im.mov()}
             if(tclvar$alt=="Spectral Summary"){im.spec()}}
               
fs<-function(...){ f.analyze.file.summary(tclvar$file)}

pts<-function(...){
plot(f.read.analyze.ts(tclvar$file,as.numeric(tclvar$x),as.numeric(tclvar$y),as.numeric(tclvar$z)),typ="l",ylab="fMRI response",xlab="Scans")}
       
period<-function(...){
  par(mfrow=c(1,1),mar=c(4,4,5,5))
  a<-f.read.analyze.ts(tclvar$file,as.numeric(tclvar$x),as.numeric(tclvar$y),as.numeric(tclvar$z))
  b<-fft(a)/sqrt(2*pi*length(a))
  b<-b[10:floor(length(b)/2)+1]
  b<-Mod(b)^2
  plot(b,ylab="Periodogram",xlab="Fourier Frequency")
}
       
im.sl<-function(...){
   par(mfrow=c(1,1),mar=c(0,0,0,0))
a<-f.read.analyze.slice(tclvar$file,as.numeric(tclvar$z),as.numeric(tclvar$t))

  image(a)
  par(mfrow=c(1,1),mar=c(4,4,5,5))}

im.vol<-function(...){
a<-f.read.analyze.header(tclvar$file)$dim
d<-ceiling(sqrt(a[4]))
  par(mfrow=c(d,d),mar=c(0,0,0,0))
b<-array(0,dim=a[2:4])
for(i in 1:a[4]){
    b[,,i]<-f.read.analyze.slice(tclvar$file,i,as.numeric(tclvar$t))}
for(i in 1:a[4]){image(b[,,i],axes=F);box()}
  
  par(mfrow=c(1,1),mar=c(4,4,5,5))
}

im.mov<-function(...){
   par(mfrow=c(1,1),mar=c(0,0,0,0))
a<-f.read.analyze.header(tclvar$file)$dim
b<-array(0,dim=c(a[2],a[3],a[5]))
  for(i in 1:a[5]){
    b[,,i]<-f.read.analyze.slice(tclvar$file,as.numeric(tclvar$z),i)}
  for(i in 1:a[5]){
    image(b[,,i],axes=F,add=T)}
  par(mfrow=c(1,1),mar=c(4,4,5,5))
}
im.spec<-function(...){
   par(mfrow=c(1,1),mar=c(4,4,5,5))
a<-f.spectral.summary(tclvar$file,tclvar$mask)
  par(mfrow=c(1,1),mar=c(4,4,5,5))
}

#tclvar$file<-tkfilefind("/")
base <- tktoplevel()
    tkwm.title(base, "ANALYZE file explore")
           f1 <- tkframe(base, relief="groove", borderwidth=2)
          
    tkpack(tkentry(f1, textvariable="file",width=40))
    file.find.but <- tkbutton(f1, text="Select File", command=wrap.file)
    tkpack(file.find.but)

          
    tkpack(tkentry(f1, textvariable="mask",width=40))
    mask.find.but <- tkbutton(f1, text="Select Mask File", command=wrap.mask)
    tkpack(mask.find.but)
           
       opt.rbuts<-tkframe(base, relief="groove",borderwidth=2)
           
 tkpack(tklabel(opt.rbuts, text="Options"))
       
    for ( i in c("File Summary","Plot Time Series","Plot Periodogram", "Image Slice", "Image Volume","Movie","Spectral Summary")){
            tmp<-tkradiobutton(opt.rbuts, text=i, variable="alt", value=i)
            tkpack(tmp,anchor="w")
        }
           fr2<-tkframe(base, relief="groove",borderwidth=2)
            x.entry<-tkentry(fr2, textvariable="x")
            y.entry<-tkentry(fr2, textvariable="y")
            z.entry<-tkentry(fr2, textvariable="z")
            t.entry<-tkentry(fr2, textvariable="t")

           tkgrid(f1)
        tkgrid(tklabel(fr2,text="Variables"),columnspan=2)
        tkgrid(tklabel(fr2,text="x variable"), x.entry)
        tkgrid(tklabel(fr2,text="y variable"), y.entry)
        tkgrid(tklabel(fr2,text="z variable"), z.entry)
        tkgrid(tklabel(fr2,text="t variable"), t.entry)
           tkgrid(opt.rbuts)
           tkgrid(fr2)
           
    fr3 <- tkframe(base, borderwidth=2)
    q.but <- tkbutton(fr3,text="Quit",
                      command=function()tkdestroy(base))
    ok.but<-tkbutton(fr3,text="OK",
                      command=do)
    tkgrid(ok.but,q.but)
    tkgrid(fr3)


    })



