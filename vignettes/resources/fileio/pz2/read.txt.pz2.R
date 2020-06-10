# based on import example of hyperSpec read.txt.wide and FileIO/read.txt.PerkinElmer.R
# you may want to add indices to the hyperSpec obj:
# hyobj@data$indices<-spectral.indices(hyobj)
read.txt.pz2 <- function (
			    file = stop("filename is required"),
			    label = list (), 
			    wavelength = NULL, 
			    short = "read.txt.pz2", 
			    user = Sys.getenv("USER"), 
			    date = date(),
			    cols = list(spc = "Intensity",skip=2,.wavelength = expression(lambda/nm)), 
			    check.names = FALSE,...) {
## check for library
if (!require("hyperSpec")) stop("Could not load required package 'hyperSpec'")

## infos
  long <- list (file = file, ..., label = label)
  label <-modifyList(list(.wavelength = cols$.wavelength,spc = cols$spc), label)

## read data
  # this is the col starting with the measurements
  firstdatacol<-5
  # check file version string
  version<-read.csv(file,skip=0,nrows=1,sep="\t",header=FALSE)
  if (version[1] != "VERSION=2.0"){
    warning("parsing file other than VERSION=2.0, this might fail. Continuing anyway...")
  }
  
  # read header, extract wavelength list
  head<-read.csv(file,skip=2,nrows=1,sep="\t",header=FALSE)
  #files contain trailing separator, last/empty will be NA
  validcols<-!is.na(head)
  head<-head[validcols]
  if(is.null(wavelength)){
    wavelength<-head[firstdatacol:length(head)]
  }
  # read data rows
  raw<-read.csv(file,skip=3,sep="\t",header=FALSE)
  raw<-raw[,validcols]
  ## extract data
  # as far as I know there is only one WCAL line per file: the first one. check
  if (!raw[1,3] == "WCAL"){
    warning(paste("no WCAL line found, there is something wrong in file ",file))
    return(new("hyperSpec"))
  }
  wcalrow<-raw[1,]
  # delete(raw[1,])
  rawmeas<-subset(raw,raw[,3]!="WCAL")

  #parse type of measurement (Ref,Meas) and number from remaining rows
  typenr<-t(data.frame(strsplit(as.character(rawmeas[,3]),split=" ")))
  colnames(typenr)<-c("nr","mtyp")
  # measurement numbers
  measnr<-as.numeric(typenr[typenr[,2]=="M",1])
  nmeas<-length(measnr) # number of measurements
  ## compute the relative refl. values with calibration
  # dimmeas<-dim(rawmeas)
  # indices of data cols
  colidx<-firstdatacol:length(wcalrow)
  M<-subset(rawmeas,typenr[,2]=="M")[colidx]
  R<-subset(rawmeas,typenr[,2]=="R")[colidx]
  # create an object of size M with the WCAL values repeated
  # TODO: there must be a smarter, more efficient way
  WCALr<-subset(wcalrow[colidx])
  WCAL<-NULL
  for (i in 1:length(measnr)){
    WCAL<-rbind(WCAL,WCALr)
#     fnamelabel<-rbind(fnamelabel,file)
  }
  #   WCAL<-data.frame(t(array(wcalrow[colidx],dim=c(length(colidx),dim(M)[1]))))
  #   WCAL<-t(array(wcalrow[colidx],dim=c(length(colidx)),dim(M)[1]))

  # calibration function: division M/R/WCAL
  spc=M/R/WCAL
  
  # assemble meta data
  # filename as labels for metadata
  # fnamelabel<-rep(file,times=length(measnr))
  #datetime<-strptime(paste(raw[,1],raw[,2],sep=" "),format="%d.%m.%Y %H:%M.%S")
  mdat<-data.frame(
    filename=file,
    mnr=measnr,
    mtime=strptime(subset(paste(rawmeas[,1],rawmeas[,2],sep=" "),typenr[,2]=="M"),format="%d.%m.%Y %H:%M.%S"),
    integrtime=subset(rawmeas[,4],typenr[,2]=="M")
  )
  # info about file, user, etc.
  # WCAL time into log, 
  # filename=rep(file,times=length(measnr)),
  #create hyperSpec object 
  hyobj<-new("hyperSpec",spc=as.matrix (spc),wavelength=as.numeric(wavelength),data=mdat,label = label)
#   hyobj	
  return (hyobj)

# stuff left after version merge
#  hyobj<-new(hyperSpec)
#  raw$datetime<-strptime(paste(raw[,1],raw[,2],sep=" "),format="%d.%m.%Y %H:%M.%S")

}
