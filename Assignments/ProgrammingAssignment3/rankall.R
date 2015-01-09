outcomeCareFileName <- "./dataset/outcome-of-care-measures.csv"

rankall<-function(outcome, num="best")
{    
    outcomeColumnName <- validateOutcome(outcome) 
    
    outcomeDF <- read.csv(outcomeCareFileName, colClasses="character")
    outcomeDF <- outcomeDF[,c("State","Hospital.Name", outcomeColumnName)] #trimm the dataset to work with a much smaller one
    
    outcomeDF[,outcomeColumnName]<-as.numeric(as.character(outcomeDF[,outcomeColumnName]))
    outcomeDF<-outcomeDF[!is.na(outcomeDF[,outcomeColumnName]),]
       
    
    dfByState <- split(outcomeDF, outcomeDF$State)      
    
    ##order all dataframes
    dfByState <-lapply(dfByState, function(x){
        x<-x[order(x[,outcomeColumnName],x$Hospital.Name),]        
    })  
        
    result <- data.frame(matrix(ncol = 2, nrow = length(dfByState)))
    i<-1
    for (df in dfByState)
    {        
        if (num == "best")
            result[i,] <- df[1,2:1]
         else if (num == "worst")
             result[i,] <- df[nrow(df),2:1]
         else if ( num > nrow(df))
             result[i,] <- c(NA, df[1,"State"])
         else             
             result[i,] <- df[num,2:1]
        i<-i+1
    }
    names(result) <- c("hospital", "state")    
    result
}


validateOutcome <- function(outcome)
{
    if (!(outcome %in% possibleOutcomes))
    {
        stop("invalid outcome")
    }
    if (outcome == "heart attack")
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    else if (outcome == "heart failure")
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    else 
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
}

rankall("pneumonia")

