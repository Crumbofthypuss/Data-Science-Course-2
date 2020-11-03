hospitaldata<-read.csv("hospital-data.csv")
outcomedata<-read.csv("outcome-of-care-measures.csv",colClasses="character")
rankhospital<- function(state,outcome,num="best"){
  if (is.element(state,outcomedata$State)==FALSE){
    stop("invalid state")
  }
  disease<-c("heart attack","heart failure","pneumonia")
  if (is.element(outcome,disease)==FALSE){
    stop("invalid outcome")
  }
  outcome<- gsub(" ","", outcome, fixed=TRUE)
  outcomedata<- outcomedata[which(outcomedata["State"]==state,),]
  diseasename<- list(heartattack="Heart.Attack", heartfailure="Heart.Failure", pneumonia="Pneumonia")
  outcomename<- paste("Hospital.30.Day.Death..Mortality..Rates.from.",diseasename[[outcome]],sep="")
  results<- tapply(as.numeric(outcomedata[[outcomename,exact=FALSE]]),outcomedata$Hospital.Name,sum,na.rm=TRUE)
  if (num=="best"){   
    correctresults<-min(results[results>0])
    results[match(correctresults,results)]
  } else if(num=="worst"){
    correctresults<-max(results[results>0])
    results[match(correctresults,results)]
  } else if(num>length(results)){
    return("NA")
  } else{
    results<- sort(results[results>0])
    results[num]
  }
}
