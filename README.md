# R-programming-assignment-3
best <- function(state, outcome)  {
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  data<-data[,c(2,7,11,17,23)]
  ## Check that state and outcome are valid
  data[,3]<-as.numeric(data[,3])
  data[,4]<-as.numeric(data[,4])
  data[,5]<-as.numeric(data[,5])
  valid<-!is.na(data[,c(3,4,5)])
  valid2<-c()
  for(i in 1:nrow(valid))
  {valid2[i]<-valid[i,1]&valid[i,2]&valid[i,3]}
  data<-data[valid2,]
  colnames(data)<-c("Hospital.Name","State","heart attack","heart failure","pneumonia")
  ## Return hospital name in that state with lowest 30-day death
  best<-tapply(data[outcome][,1],INDEX=factor(data$State),FUN=min)
  best2<-best[state]
  ##name<-sort(data[data$outcome==best2&data$State==state,1])
  name<-sort(data[which(data[outcome]==best2 & data$State==state),1])
  final_name<-name[1]
  final_name
  ## rate
}
