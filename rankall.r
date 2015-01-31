##function called rankall that takes two arguments: an outcome name (outcome) and a hospital ranking
##(num). The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame
##containing the hospital in each state that has the ranking specified in num. For example the function call
##rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
##are the best in their respective states for 30-day heart attack death rates. The function should return a value
##for every state (some may be NA). The first column in the data frame is named hospital, which contains
##the hospital name, and the second column is named state, which contains the 2-character abbreviation for
##the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
##hospitals when deciding the rankings.
##Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
##that the rankhospital function handles ties.

rankall <-function (outcome, num="best")
{
  
  ##Index nos to pick value from outcome file. Here 30 day mortality is used.
  indexer<-c(11,17,23)
  names(indexer)<- c("heart attack","heart failure", "pneumonia")
  
  hospital<-character(0)
  
  if(outcome !="heart attack" & outcome !="heart failure" & outcome !="pneumonia")
  { stop ("Invalid Outcome")}
  
  if(num!="best" & num!="worst" & (is.na(suppressWarnings(as.integer(num)))))
  { stop ("Incorrect Ranking Selected")}
  
 
  
  ##Read Data Frame of file
  dataread <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available"," "))
  
  ##Retrieve data for states with values available for given outcome
  subsetdata<- dataread[!is.na(dataread[,indexer[outcome]]),]
  
  
  
  ##List of unique states in above retrieved data sets
  listofstates<-sort(unique(subsetdata[,7]))
  
  for(i in seq_along(listofstates))
    
  {
    ##Subset out data per state
    subsetdata2<-subsetdata [ subsetdata[,"State"]==listofstates[i],]
    
    ## Sort the above retrieved list by outcome per state 
    subsetdata3<-subsetdata2[(order(as.double(subsetdata2[[indexer[outcome]]]),  subsetdata2[["Hospital.Name"]],  decreasing=FALSE, na.last=NA)),]
    
    if(num=="best")
    {ranknum<-1}
    else if (num=="worst")
    { ranknum<- length(subsetdata3[,indexer[outcome]])}
    else 
    { ranknum<- as.integer(num); }
    
    ##Collect name of hospitals.
    hospital[i]<-subsetdata3[ranknum,"Hospital.Name"]
  }
  
  ##Compose data frame with row titled by state names and columns titled by 'hopital'  and 'states'
  hospitaldataframe<-data.frame(hospital=hospital, state=listofstates,row.names=listofstates)
  
 
}