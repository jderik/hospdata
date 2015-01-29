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
function <- rankhospita(state, outcome, num="best")
{
  dataread <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings=c("Not Available"," "))
  
  if(outcome !="heart attack" & outcome !="heart failure" & outcome !="pneumonia")
  { stop ("Invalid Outcome")}
  
  if (!is.element(state,dataread[,7]))
  { stop ("Invalid State") }
  
  if(num!="best" & num!="worst")
  {
    if(!is.na(suppressWarnings(as.integer(num))))
    {     ranknum<-as.integer(num) }
    else
    { stop("Ranking is not appropriate")}
  }
  else
  { ranknum<-num}
}