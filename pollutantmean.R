pollutantmean <- function(directory,pollutant,id=1:332){
  data=numeric()
  for (i in id){
    allFile <- paste(directory,"/",formatC(i,width=3,flag="0"),".csv",sep="")
    eachfile <- read.csv(allFile)
    data <- c(data,eachfile[[pollutant]])
  }
  print(data)
  mean(data, na.rm=TRUE)
}