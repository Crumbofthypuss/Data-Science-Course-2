df<-data.frame()
count<-numeric()
complete<- function(directory, id=1:332){
  for (i in id){
    allFile <- paste(directory,"/",formatC(i,width=3,flag="0"),".csv",sep="")
    eachfile <- read.csv(allFile)
    arrangedfile <- eachfile[complete.cases(eachfile),]
    total<- nrow(arrangedfile)
    count<- c(count,total)
  }
  df<- data.frame("id" =id,nobs=count)
  print(df)
}