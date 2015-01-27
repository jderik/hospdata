## Part 1 of R programming assignment 3 - hospital data
##1 Plot the 30-day mortality rates for heart attack
## Read the outcome data into R via the read.csv function and look at the first few rows.

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
##There are many columns in this dataset. You can see how many by typing 

ncol(outcome)
nrow(outcome)
names(outcome) 

##(the names are also in the PDF document
## To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset),
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[,11])
