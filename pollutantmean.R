pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        ## options(digits=4)
        
        myFiles <- list.files(path=paste(getwd(),"/",directory, sep=""), pattern="csv")
        
        CombinedData <- vector()
        
        for (i in seq_along(id)){
                ImportedData <- read.csv(paste(getwd(),"/",directory,"/", myFiles[id[i]], sep=""))
                CombinedData <- c(CombinedData, ImportedData[[pollutant]])
        }

        return (mean(CombinedData, na.rm=TRUE))
}