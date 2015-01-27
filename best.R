##2 Finding the best hospital in a state
## The function takes two arguments: the 2-character abbreviated name of a state and an
## outcome name. 
## The function reads the outcome-of-care-measures.csv file and returns a character vector
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome in that state. 
## Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals  when deciding the rankings.
## Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
## be sorted in alphabetical order and the first hospital in that set should be chosen 


best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  dataread <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  if(outcome !="heart attack" AND outcome !="heart failure" AND outcome !="pneumonia")
  { stop ("Invalid Outcome")}
  
  if (!is.element(state,dataread[,7]))
  { stop ("Invalid State") }
  
  
  
  
}
