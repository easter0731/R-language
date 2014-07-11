rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        if (outcome == "heart attack") index = 11
        else if (outcome == "heart failure") index = 17
        else if (outcome == "pneumonia") index = 23
        else stop("invalid outcome")
        ## For each state, find the hospital of the given rank
        allstate<-levels(factor(outcomedata[,7]))
        hospital <- vector()
        for (i in seq_along(allstate)){
                state<-allstate[i]
                statedata <- outcomedata[state == outcomedata[,7],]
                mortality <- as.numeric(statedata[,index])  
                r <- order(mortality, statedata[,2], na.last = T)
                validatednum <- sum(!is.na(mortality))
                ##print(validatednum)
                tag <- num
                if (num == "best") tag <- 1
                if (num == "worst") tag <- validatednum
                if (tag > validatednum) hospital[i] <- NA
                else hospital[i] <- statedata[r[tag],2]
                ##print(tag)
        }
        ##print(hospital)
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ##print(tag)
        rankall <- data.frame (hospital=hospital, state = allstate)
        return (rankall)
}




