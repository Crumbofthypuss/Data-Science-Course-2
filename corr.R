sulfatenum<- numeric()
nitratenum<- numeric()
total<- numeric()
corr<- function(directory, threshold=0){ 
  for (i in 1:332){
    allFile <- paste(directory,"/",formatC(i,width=3,flag="0"),".csv",sep="")
    eachfile <- read.csv(allFile)
    arrangedfile <- eachfile[complete.cases(eachfile),]
    if (nrow(arrangedfile)>threshold){
      sulfatenum<- arrangedfile[["sulfate"]]
      nitratenum<- arrangedfile[["nitrate"]]
      total<- c(total,cor(sulfatenum,nitratenum))
    }
  }
  total
}