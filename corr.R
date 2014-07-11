corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        ## options(digits=4)
        
        myFiles <- list.files(path=paste(getwd(),"/",directory, sep=""), pattern="csv")
        
        sum <- integer()
        corr <- numeric()
        
        for (i in c(1:length(myFiles))){
                ImportedData <- read.csv(paste(getwd(),"/",directory,"/", myFiles[i], sep=""))
                ok <- complete.cases(ImportedData[,1],ImportedData[,2],ImportedData[,3],ImportedData[,4])
                sum[i] <- sum(ok)
                
                if (sum[i]>=threshold && sum[i]>0) {
                        x<- 
                        
                        corr <- c(corr,cor(ImportedData[ok,2],ImportedData[ok,3]))
                }
        }
        
        return(corr)  

}