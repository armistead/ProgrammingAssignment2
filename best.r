setwd("C:/Users/johnarmistead/Data Sci Coursera/Class2/Assignment3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)
##heart attack=col11
##heart failure=col17
##pneumonia=col23

best <- function(state, outcome){
##Get data
data <- read.csv("outcome-of-care-measures.csv", na.strings='Not Available', stringsAsFactors=FALSE)
         if(!state %in% unique(data$State)) {
                stop("Invalid State")
        } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("Invalid Outcome")
        } else {
                switch(outcome, "heart attack" = {col = 11}, "heart failure" = {col = 17}, "pneumonia" = {col = 23})
                ## Return hospital name in chosen state
                df = data[data$State == state, c(2, col)]
                output <- df[which.min(df[, 2]), 1]
                }
        return(output)
}