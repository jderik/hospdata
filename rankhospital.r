## rankhospital takes three arguments: 
##      the 2-character abbreviated name of a state (state), 
##      an outcome (outcome), 
##      and the ranking of a hospital in that state for that outcome (num).
## The function reads the outcome-of-care-measures.csv and returns a character vector with the name
## of the hospital that has the ranking specified by the num argument. 

## For example, the call rankhospital("MD", "heart failure", 5)
## would return a character vector containing the name of the hospital with the 5th lowest 
## 30-day death rate  for heart failure. 

## The num argument can take values 
##  "best", "worst", or an integer indicating the ranking (smaller numbers are better). 

## If the number given by num is larger than the number of hospitals in that
## state, then the function should return NA. Hospitals that do not have data on a particular outcome should
## be excluded from the set of hospitals when deciding the rankings.
rankhospital <- function(state, outcome, num="best")
{
  dataread <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available"," "))
  
  ##Index nos to pick value from outcome file. Here 30 day mortality is used.
  indexer<-c(11,17,23)
  names(indexer)<- c("heart attack","heart failure", "pneumonia")
  
 
  
  if(outcome !="heart attack" & outcome !="heart failure" & outcome !="pneumonia")
  { stop ("Invalid Outcome")}
  
  if (!is.element(state,dataread[,7]))
  { stop ("Invalid State") }
  
  hospital<-NA
  
  ##Check that num is 1 of 3 options
  if(num!="best" & num!="worst")
  {
    if(!is.na(suppressWarnings(as.integer(num))))
    {     
      ## Picks unique hospital count from given state
      x<-unique(dataread[dataread[,"State"]==state & !is.na(dataread[,indexer[outcome]]),"Hospital.Name"])
   
      
      if(length(x)>=as.integer(num))
      {
          ranknum<-as.integer(num) 
          
          
          ##GetSubset of Hospital data for the state
          subsetdata<- dataread[ dataread[,"State"]==state,]
          
          
          ##Sort subset by ascending order of outcome and remove na rows
          
                  ## subsetdata2<-subsetdata[(sort(as.double(subsetdata[,indexer[outcome]]), decreasing=FALSE, na.last=NA)),]
          
          subsetdata2<-subsetdata[(order(as.double(subsetdata[[indexer[outcome]]]), subsetdata[["Hospital.Name"]], decreasing=FALSE, na.last=NA)),]
                  ##subsetdata2<-subsetdata[!(is.na(subsetdata[,indexer[outcome]])),]   ## allocate non na data
                  ##subsetdata2[,indexer[outcome]]<-as.double(subsetdata2[,indexer[outcome]]) ## Now get the double values
          
          ##pick the ranked hospital
          hospital<- subsetdata2[ranknum, "Hospital.Name"]
          
          
      }
      else
      {hospital<-NA }
      
    }
    else
    { stop("Ranking is not appropriately provided")}
  }
  else
  { 
    if (num=="best")
      { source("best.r")
        
        
        ##GetSubset of Hospital data for the state
        subsetdata<- dataread[ dataread[,"State"]==state,]
        
        ##Get the minimum reading
        x<-as.double(subsetdata[,indexer[outcome]])
        minVal <-which.min(x)
        
        ##Array of Hospital where the mortality is min for given outcome
        hospitallist<- subsetdata[ minVal, "Hospital.Name"]
       ## hospital<-hospitallist[sort(hospitallist, decreasing=FALSE)]
        hospital<-hospitallist
      }
    else
      if(num=="worst")
      { 
        ##GetSubset of Hospital data for the state
        subsetdata<- dataread[ dataread[,"State"]==state,]
        
                
        subsetdata2<-subsetdata[(order(as.double(subsetdata[[indexer[outcome]]]), subsetdata[["Hospital.Name"]], decreasing=FALSE, na.last=NA)),]
        
        ##Find index of hospital from sorted list
        ranknum<-length(subsetdata2[,indexer[outcome]])
        
        ##pick the ranked hospital
        hospital<- subsetdata2[ranknum, "Hospital.Name"]
        
        
        ##Get the minimum reading
       ### x<-as.double(subsetdata[,indexer[outcome]])
       ## maxVal <-which.max(x)
        
        ##Array of Hospital where the mortality is max for given outcome
       ## hospitallist<- subsetdata[ maxVal, "Hospital.Name"]
       ## hospital<-hospitallist[sort(hospitallist, decreasing=TRUE)]
      }
  
  }
   
      
  
  hospital[1]
  
}