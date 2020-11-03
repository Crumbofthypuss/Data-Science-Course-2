hospitaldata<-read.csv("hospital-data.csv")
outcomedata<-read.csv("outcome-of-care-measures.csv",colClasses="character")
#hist(as.numeric(outcome[,11]))

#names(outcomedata)
#head(outcomedata)

best<-function(state, outcome){
  if (is.element(state,outcomedata$State)==FALSE){
    stop("invalid state")
  }
  disease<-c("heart attack","heart failure","pneumonia")
  if (is.element(outcome,disease)==FALSE){
    stop("invalid outcome")
  }
  outcome<- gsub(" ","", outcome, fixed=TRUE)
  outcomedata<- outcomedata[which(outcomedata["State"]==state,),]
  #print(outcomedata)
  diseasename<- list(heartattack="Heart.Attack", heartfailure="Heart.Failure", pneumonia="Pneumonia")
  outcomename<- paste("Hospital.30.Day.Death..Mortality..Rates.from.",diseasename[[outcome]],sep="")
  #print(outcomedata[[outcomename,exact=FALSE]])
  results<- tapply(as.numeric(outcomedata[[outcomename,exact=FALSE]]),outcomedata$Hospital.Name,sum,na.rm=TRUE)
  #print(results)
  correctresults<-min(results[results>0])
  results[match(correctresults,results)]
}

best("TX","heart attack")