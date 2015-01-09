source("best.R")

rankhospital <- function(state, outcome, num="best")
{
    best(state, outcome)
    
    if (num=="best")
        stateHospitalsOrderByOutcome[[1,"Hospital.Name"]]
    else if (num=="worst")
        stateHospitalsOrderByOutcome[[nrow(stateHospitalsOrderByOutcome),"Hospital.Name"]]
    else if (num > nrow(stateHospitalsOrderByOutcome))
        NA
    else
        stateHospitalsOrderByOutcome[[num,"Hospital.Name"]]
             
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)