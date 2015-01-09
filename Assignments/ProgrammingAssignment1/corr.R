corr <- function(directory, threshold = 0) {    
    cases <- complete(directory)
    casesAboveThreshold <- subset(cases$id, cases$nobs>threshold)
    
    corrVector<-vector()
    
    filesList <- list.files(as.character(directory), full.names=TRUE) 
        
    for (doc in filesList[casesAboveThreshold])
    {              
        df <- read.csv(doc, header=TRUE, colClasses=c("Date", "numeric", "numeric", "character"))
        
        tmpCor <- cor(df$nitrate, df$sulfate, use="complete.obs")
        corrVector<-c(corrVector, tmpCor)
    }      
    corrVector
}
# 
#setwd('C:/Users/e475/Desktop/IntroductionToR/Assignments/1/')
#source("complete.R")
cr <- corr("specdata/")
summary(cr)
length(cr)