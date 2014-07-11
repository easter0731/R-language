rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        allstates <- outcomedata [,7]
        if(sum(allstates == state) == 0) stop("invalid state")
        index = 0
        if (outcome == "heart attack") index = 11
        else if (outcome == "heart failure") index = 17
        else if (outcome == "pneumonia") index = 23
        else stop("invalid outcome")
        
        ## Return hospital name in that state with lowest 30-day death
        
        statedata <- outcomedata[state == outcomedata[,7],]
        mortality <- as.numeric(statedata[,index])  
        r <- order(mortality, statedata[,2], na.last = T)
        validatednum <- sum(!is.na(mortality))
        if (num == "best") num <- 1
        if (num == "worst") num <- validatednum
        if (num > validatednum) return(NA)
        else return(statedata[r[num],2])
        ## rate
}