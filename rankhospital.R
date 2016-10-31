setwd("C:/Users/johnarmistead/Data Sci Coursera/Class2/Assignment3")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses="character")
head(outcome)
##heart attack=col11
##heart failure=col17
##pneumonia=col23
set.seed(1)
rankhospital <- function(state, outcome, rank = "best"){
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", na.strings='Not Available', stringsAsFactors=FALSE)
        tbl <- as.data.frame(cbind(data[, 2],  data[, 7], data[, 11], data[, 17], data[, 23]),)
        colnames(tbl) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
        ## Check that state and outcome are valid
        if (!state %in% tbl[, "state"]) {
                stop('invalid state')
        } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop('invalid outcome')
        } else if (is.numeric(rank)) {
                statedata <- which(tbl[, "state"] == state)
                getstate  <- tbl[statedata, ]
                getstate[, eval(outcome)] <- as.numeric(getstate[, eval(outcome)])
                getstate <- getstate[order(getstate[, eval(outcome)], getstate[, "hospital"]), ]
                output <- getstate[, "hospital"][rank]
        } else if (!is.numeric(rank)){
                if (rank == "best") {
                        output <- best(state, outcome)
                } else if (rank == "worst") {
                        statedata <- which(tbl[, "state"] == state)
                        getstate  <- tbl[statedata, ]
                        getstate[, eval(outcome)] <- as.numeric(getstate[, eval(outcome)])
                        getstate<- getstate[order(getstate[, eval(outcome)], getstate[, "hospital"], decreasing = TRUE), ]
                        output <- getstate[, "hospital"][1]
                } else {
                        stop('invalid rank')
                }
        }
        return(output)
}


