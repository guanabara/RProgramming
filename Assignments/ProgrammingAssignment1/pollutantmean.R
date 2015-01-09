pollutantmean <- function(directory, pollutant, id = 1:332) 
{
    setwd(directory)
    elements<-0
    sum <- 0
    
    ##Read files in given directory
    for (doc in id)
    {    
        filename<-paste(formatC(doc, width=3, flag="0"), ".csv", sep="")        
        df <- read.csv(filename, header=TRUE, colClasses=c("Date", "numeric", "numeric", "character"))        
    
        for(v in df[,pollutant])
        {            
            if (!is.na(v))
            {
                elements<-elements+1
                sum <- sum +v
            }
        }                
    }
    sum/elements    
}

#pollutantmean('C:/Users/e475/Desktop/IntroductionToR/Assignments/1/specdata/', "nitrate", 70:72)
