hospitalDataFilename <- "./dataset/hospital-data.csv" 
outcomeCareFileName <- "./dataset/outcome-of-care-measures.csv"
possibleOutcomes = c("heart attack", "heart failure", "pneumonia")

validateStateAndOutcome <- function(outcomeDF, state, outcome)
{
    outcomeColumnName <- validateOutcome(outcome)
    outcomeForGivenState <- subset(outcomeDF, State==state, select=c("Hospital.Name", outcomeColumnName))
    
    outcomeForGivenState[,outcomeColumnName] <- as.numeric(as.character(outcomeForGivenState[,outcomeColumnName]))
    
    outcomeForGivenState<-outcomeForGivenState[!is.na((outcomeForGivenState[,outcomeColumnName])),]
    
    if (nrow(outcomeForGivenState)==0)
    {
        stop("invalid state")
    }        
    
    stateHospitalsOrderByOutcome <<- outcomeForGivenState[order(outcomeForGivenState[,outcomeColumnName],outcomeForGivenState$Hospital.Name),]
            
    stateHospitalsOrderByOutcome
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

best <- function(state, outcome)
{
    outcomeDF <- readOutcomeFileAsNumeric()    
    filteredDF <- validateStateAndOutcome(outcomeDF, state, outcome)
    
    filteredDF[1, "Hospital.Name"]
}