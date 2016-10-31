setwd("C:/Users/johnarmistead/Data Sci Coursera/Class2/Assignment3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)
##heart attack=col11
##heart failure=col17
##pneumonia=col23

rankall <- function(outcome, num="best") {
        ## Get data
        ## Return a data frame with the hospital names and the state name

        data <- read.csv("outcome-of-care-measures.csv", na.strings='Not Available', stringsAsFactors=FALSE)
        
        tbl <- as.data.frame(cbind(data[, 2], data[, 7], data[, 11], data[, 17], data[, 23]))
        
        colnames(tbl) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        column <- function(outcome) {
                if (outcome == "heart attack") {
                        outcome <- "heart attack"
                } else if (outcome == "heart failure") {
                        outcome <- "heart failure"
                } else if (outcome == "pneumonia") {
                        outcome <- "pneumonia"
                }
                else {
                        stop("invalid outcome")
                }
        }
        outcome <- column(outcome)
        
        tbl[,outcome] <- as.numeric(tbl[,3])
        tbl <- tbl[order(tbl[,2], tbl[,3], tbl[,1], na.last=NA),]
        tbl <- tbl[!is.na(outcome)]
        
        s <- split(tbl[,1], tbl[,2])
        
        rank <- function(x, num) {
                if (num=="best") {
                        head(x, 1)
                } else if (num=="worst") {
                        tail(x, 1)
                } else {
                        x[num]
                }
        }
        
        result <- lapply(s, rank, num)
        data.frame(hospital = unlist(result), state = names(result), row.names = names(result))
}