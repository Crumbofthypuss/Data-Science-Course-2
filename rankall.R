hospitaldata<-read.csv("hospital-data.csv")
outcomedata<-read.csv("outcome-of-care-measures.csv",colClasses="character")

#function gets the hospital name of a specific rank for each state 
rankall <- function(outcome,num="best"){
  
  overallrank<-data.frame()
  disease<-c("heart attack","heart failure","pneumonia")
  if (is.element(outcome,disease)==FALSE){
    stop("invalid outcome")
  }
  outcome<- gsub(" ","", outcome, fixed=TRUE)
  states <- unique(outcomedata$State)
  
  # get the values of names based on num given for each state
  for (state in states){
    diseasename<- list(heartattack="Heart.Attack", heartfailure="Heart.Failure", pneumonia="Pneumonia")
    outcomename<- paste("Hospital.30.Day.Death..Mortality..Rates.from.",diseasename[[outcome]],sep="")#to reword column name
    outcomedata1<- outcomedata[which(outcomedata["State"]==state,),]#to get specific states
    outcomedata2<- outcomedata1[which(outcomedata1[[outcomename]]!="Not Available",),]#remove Not available values
    results<- data.frame("value"=as.numeric(outcomedata2[[outcomename]]),"Name"=outcomedata2$Hospital.Name)#to get values and names of each hospital
    uniquevalues<- sort(unique(results$value),decreasing=FALSE)
    #print(uniquevalues)
    #print(results)
    
    #determines what process to do based on num value
    if (num=="best"){   
      results<-results[order(results$value),]
      truevalue<- uniquevalues[1]
      #print(results)
      finalresult<-data.frame("Name"=results$Name[results$value==truevalue][[1]],"State"=state,stringsAsFactors=FALSE)
    } else if(num=="worst"){
      results<-results[order(results$value),]
      truevalue<- uniquevalues[length(uniquevalues)]
      #print(truevalue)
      #print(results)
      finalresult<-data.frame("Name"=results$Name[results$value==truevalue][1],"State"=state,stringsAsFactors=FALSE)
    } else{
      results<-results[order(results$value),]
      #results<- arrange(results,results$value,results$Name)
      #print(results)
      truevalue<- uniquevalues[num]
      #print(truevalue)
      finalresult<-data.frame("Name"=results$Name[results$value==truevalue][[1]],"State"=state,stringsAsFactors=FALSE)
      #print(finalresult)
    }
    
    #print(finalresult)
    #print(class(finalresult[[1]]))
    overallrank<-rbind(overallrank,finalresult)
  }
  overallrank<-overallrank[order(overallrank$State),]
  return(overallrank)
}