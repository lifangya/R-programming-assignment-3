rankhospital <- function(state, outcome, num="best") {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(outcome=="heart attack"){x<-11}
  if(outcome=="heart failure"){x<-17}
  if(outcome=="pneumonia"){x<-23}
  data<-data[which(data$State==state),c(2,7,x)]
  data[,3]<-as.numeric(data[,3])
  valid<-!is.na(data[,c(3)])
  data<-data[valid,]
  data[,4]<-numeric()
  colnames(data)<-c("Hospital.Name","State","Rate","Rank")
  ## Check that state and outcome are valid
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  validState = sort(unique(data[,2]))
  if (!state %in% validState) stop("invalid state")
  ## Return hospital name in that state with the given rank
  data<-data[order(data$Rate,data$Hospital.Name),]
  data[,4]<-1:nrow(data)
  if(num=="best"){num<-1}
  if(num=="worst"){num<-nrow(data)}
  final<-data[which(data$Rank==num),1]
  if(num>nrow(data)){final<-NA}
  ## 30-day death rate
  final
}