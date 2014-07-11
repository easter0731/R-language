complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases

        myFiles <- list.files(path=paste(getwd(),"/",directory, sep=""), pattern="csv")
        
        sum <- integer()
        
        for (i in seq_along(id)){
                ImportedData <- read.csv(paste(getwd(),"/",directory,"/", myFiles[id[i]], sep=""))
                ok <- complete.cases(ImportedData[,1],ImportedData[,2],ImportedData[,3],ImportedData[,4])
                sum[i] <- sum(ok)
        }
        
        CompleteData <- data.frame(id = id, nobs = sum)

        return(CompleteData)
        
}