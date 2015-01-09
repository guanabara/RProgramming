complete <- function(directory, id = 1:332) {        
    resultDF<-data.frame(id=numeric(), nobs=numeric(), stringsAsFactors=FALSE) 
    
    filesList <- list.files(as.character(directory), full.names=TRUE) 
    
    for (i in id)
    {       
        doc <- filesList[i]
        df <- read.csv(doc, header=TRUE, colClasses=c("Date", "numeric", "numeric", "character"))  
                
        completCases <- df[(!is.na(df$nitrate) & !is.na(df$sulfate)),]        
        
        resultDF[nrow(resultDF)+1,] <- c(i,nrow(completCases))
    }
    resultDF
}

complete('C:/Users/e475/Desktop/IntroductionToR/Assignments/1/specdata/')